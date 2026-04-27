#!/usr/bin/env bash
# MBQL probe sweep — drives /api/dataset against pg-datahike to identify
# coverage gaps. Outputs a per-probe pass/fail summary; on fail captures
# the SQL Metabase emitted (from the response error or pgwire trace).
set -uo pipefail

MB=http://localhost:3000
PROBE_LOG=/tmp/mbql-probes.log
: > "$PROBE_LOG"

# ---- session: try common admin emails first; fall back to setup ----
for em in "probe@datahike.local" "test@datahike.local"; do
  SESSION=$(curl -fsS -X POST -H "Content-Type: application/json" \
    --data "$(jq -n --arg e "$em" '{username:$e, password:"datahike-test-1!"}')" \
    "$MB/api/session" 2>/dev/null | jq -r '.id // empty')
  [[ -n "$SESSION" ]] && break
done
if [[ -z "$SESSION" ]]; then
  TOK=$(curl -fsS "$MB/api/session/properties" | jq -r '."setup-token"')
  SESSION=$(curl -fsS -X POST -H "Content-Type: application/json" \
    --data "$(jq -n --arg t "$TOK" '{token: $t,
      prefs:{site_name:"probes",site_locale:"en",allow_tracking:false},
      user:{first_name:"Probe",last_name:"Admin",email:"probe@datahike.local",
            password:"datahike-test-1!",password_confirm:"datahike-test-1!",
            site_name:"probes"}}')" \
    "$MB/api/setup" | jq -r '.id')
fi
echo "Session: ${SESSION:0:8}…"

mb() { curl -fsS -H "X-Metabase-Session: $SESSION" -H "Content-Type: application/json" "$@"; }

# ---- find or create pg-datahike datasource ----
DB_ID=$(mb "$MB/api/database" | jq -r '.data[] | select(.name=="pg-datahike") | .id' | head -1)
if [[ -z "$DB_ID" ]]; then
  DB_ID=$(mb -X POST --data '{"engine":"postgres","name":"pg-datahike","details":{"host":"localhost","port":15432,"db":"datahike","user":"datahike","password":"datahike","ssl":false,"tunnel-enabled":false,"advanced-options":false}}' \
    "$MB/api/database" | jq -r '.id')
fi
echo "DB id: $DB_ID"

# ---- wait for sync ----
for i in $(seq 1 120); do
  S=$(mb "$MB/api/database/$DB_ID" | jq -r '.initial_sync_status')
  [[ "$S" == "complete" ]] && { echo "sync complete (${i}s)"; break; }
  sleep 1
done

# ---- collect field IDs ----
META=$(mb "$MB/api/database/$DB_ID/metadata?include_hidden=true")
echo "$META" > /tmp/mb-meta.json

cust_id_id=$(echo "$META" | jq -r '.tables[]|select(.name=="customer")|.fields[]|select(.name=="id")|.id')
cust_email=$(echo "$META" | jq -r '.tables[]|select(.name=="customer")|.fields[]|select(.name=="email")|.id')
cust_name=$(echo  "$META" | jq -r '.tables[]|select(.name=="customer")|.fields[]|select(.name=="name")|.id')
cust_age=$(echo   "$META" | jq -r '.tables[]|select(.name=="customer")|.fields[]|select(.name=="age")|.id')
cust_created=$(echo "$META" | jq -r '.tables[]|select(.name=="customer")|.fields[]|select(.name=="created_at")|.id')
cust_table_id=$(echo "$META" | jq -r '.tables[]|select(.name=="customer")|.id')

ord_id_id=$(echo  "$META" | jq -r '.tables[]|select(.name=="order")|.fields[]|select(.name=="id")|.id')
ord_cust=$(echo   "$META" | jq -r '.tables[]|select(.name=="order")|.fields[]|select(.name=="customer")|.id')
ord_total=$(echo  "$META" | jq -r '.tables[]|select(.name=="order")|.fields[]|select(.name=="total_cents")|.id')
ord_status=$(echo "$META" | jq -r '.tables[]|select(.name=="order")|.fields[]|select(.name=="status")|.id')
ord_table_id=$(echo "$META" | jq -r '.tables[]|select(.name=="order")|.id')

cat <<EOF
Field IDs:
  customer.id=$cust_id_id email=$cust_email name=$cust_name age=$cust_age created_at=$cust_created (table=$cust_table_id)
  order.id=$ord_id_id customer=$ord_cust total_cents=$ord_total status=$ord_status (table=$ord_table_id)
EOF

# ---- probe runner ----
PASS=0; FAIL=0
declare -a FAILS
probe() {
  local name="$1" mbql="$2"
  local body="$(jq -n --argjson dbid "$DB_ID" --argjson q "$mbql" \
    '{database:$dbid,type:"query",query:$q}')"
  local resp="$(mb -X POST --data "$body" "$MB/api/dataset")"
  local status="$(echo "$resp" | jq -r '.status // "ok"')"
  if [[ "$status" == "completed" || "$status" == "ok" ]]; then
    local rowcnt="$(echo "$resp" | jq '.data.rows|length')"
    echo "  OK  $name (rows=$rowcnt)"
    PASS=$((PASS+1))
  else
    echo "  FAIL $name [$status]"
    local err="$(echo "$resp" | jq -r '.error // .message // "no-msg"' | head -c 200)"
    echo "       $err"
    FAILS+=("$name :: $err")
    FAIL=$((FAIL+1))
    echo "=== $name ===" >> "$PROBE_LOG"
    echo "$resp" | jq '.' >> "$PROBE_LOG"
  fi
}

echo "==== MBQL probe sweep ===="

# 1. Simple count baseline
probe "01-count-customers" "$(jq -n --argjson t "$cust_table_id" \
  '{"source-table":$t,"aggregation":[["count"]]}')"

# 2. count-distinct
probe "02-count-distinct-status" "$(jq -n --argjson t "$ord_table_id" --argjson f "$ord_status" \
  '{"source-table":$t,"aggregation":[["distinct",["field",$f,null]]]}')"

# 3. count-where
probe "03-count-where-status-paid" "$(jq -n --argjson t "$ord_table_id" --argjson f "$ord_status" \
  '{"source-table":$t,"aggregation":[["count-where",["=",["field",$f,null],"paid"]]]}')"

# 4. distinct-where
probe "04-distinct-where-emails-age-gt-25" "$(jq -n --argjson t "$cust_table_id" --argjson e "$cust_email" --argjson a "$cust_age" \
  '{"source-table":$t,"aggregation":[["distinct-where",["field",$e,null],[">",["field",$a,null],25]]]}')"

# 5. multi-breakout: status + customer
probe "05-multi-breakout-status-customer" "$(jq -n --argjson t "$ord_table_id" --argjson s "$ord_status" --argjson c "$ord_cust" \
  '{"source-table":$t,"aggregation":[["count"]],"breakout":[["field",$s,null],["field",$c,null]]}')"

# 6. HAVING — count by status, having count > 1.
# MBQL idiom: nested :source-query (top-level :filter [:aggregation N] is
# a known Metabase compilation quirk that emits invalid PG SQL —
# `WHERE <agg-alias> > N`. pgwire-datahike now raises 42703 with a
# helpful hint for that form; the probe uses the canonical idiom that
# Metabase's QP correctly compiles to a valid sub-query + outer WHERE.
probe "06-having-count-gt-1" "$(jq -n --argjson t "$ord_table_id" --argjson s "$ord_status" \
  '{"source-query":{"source-table":$t,"aggregation":[["count"]],"breakout":[["field",$s,null]]},
    "filter":[">",["field","count",{"base-type":"type/Integer"}],1]}')"

# 7. CASE expression — sum(total_cents) where status='paid' else 0
probe "07-case-sum-paid" "$(jq -n --argjson t "$ord_table_id" --argjson s "$ord_status" --argjson tc "$ord_total" \
  '{"source-table":$t,
    "aggregation":[["sum",["case",[[["=",["field",$s,null],"paid"],["field",$tc,null]]],{"default":0}]]]}')"

# 8. Custom expression — total_cents / 100 as dollars
probe "08-expression-cents-to-dollars" "$(jq -n --argjson t "$ord_table_id" --argjson tc "$ord_total" \
  '{"source-table":$t,
    "expressions":{"dollars":["/",["field",$tc,null],100]},
    "aggregation":[["sum",["expression","dollars"]]]}')"

# 9. coalesce
probe "09-coalesce-status-pending" "$(jq -n --argjson t "$ord_table_id" --argjson s "$ord_status" \
  '{"source-table":$t,
    "expressions":{"safe_status":["coalesce",["field",$s,null],"pending"]},
    "aggregation":[["count"]],"breakout":[["expression","safe_status"]]}')"

# 10. top-N: orders by total desc, limit 2
probe "10-top-n-by-total" "$(jq -n --argjson t "$ord_table_id" --argjson tc "$ord_total" --argjson id "$ord_id_id" \
  '{"source-table":$t,"order-by":[["desc",["field",$tc,null]]],"limit":2,"fields":[["field",$id,null],["field",$tc,null]]}')"

# 11. top-N after JOIN: customer name + total, ordered by total desc, limit 3
probe "11-top-n-after-join" "$(jq -n --argjson t "$ord_table_id" --argjson tc "$ord_total" --argjson cid "$cust_id_id" --argjson cname "$cust_name" --argjson c "$ord_cust" \
  '{"source-table":$t,
    "joins":[{"source-table":'"$cust_table_id"',"alias":"c","fields":"none",
              "condition":["=",["field",$c,null],["field",$cid,{"join-alias":"c"}]]}],
    "fields":[["field",$cname,{"join-alias":"c"}],["field",$tc,null]],
    "order-by":[["desc",["field",$tc,null]]],"limit":3}')"

# 12. JOIN with WHERE filter
probe "12-join-with-filter" "$(jq -n --argjson t "$ord_table_id" --argjson cid "$cust_id_id" --argjson c "$ord_cust" --argjson age "$cust_age" \
  '{"source-table":$t,
    "joins":[{"source-table":'"$cust_table_id"',"alias":"c","fields":"none",
              "condition":["=",["field",$c,null],["field",$cid,{"join-alias":"c"}]]}],
    "filter":[">",["field",$age,{"join-alias":"c"}],30],
    "aggregation":[["count"]]}')"

# 13. percentile
probe "13-percentile-50" "$(jq -n --argjson t "$ord_table_id" --argjson tc "$ord_total" \
  '{"source-table":$t,"aggregation":[["percentile",["field",$tc,null],0.5]]}')"

# 14. avg of expression
probe "14-avg-of-expression" "$(jq -n --argjson t "$ord_table_id" --argjson tc "$ord_total" \
  '{"source-table":$t,
    "expressions":{"dollars":["/",["field",$tc,null],100]},
    "aggregation":[["avg",["expression","dollars"]]]}')"

# 15. nested aggregation: avg of count per customer
probe "15-nested-agg-avg-orders-per-customer" "$(jq -n --argjson t "$ord_table_id" --argjson c "$ord_cust" \
  '{"source-query":{"source-table":$t,"aggregation":[["count"]],"breakout":[["field",$c,null]]},
    "aggregation":[["avg",["field","count",{"base-type":"type/Integer"}]]]}')"

# 16. between filter on numeric
probe "16-between-filter-age" "$(jq -n --argjson t "$cust_table_id" --argjson age "$cust_age" \
  '{"source-table":$t,"filter":["between",["field",$age,null],28,40],"aggregation":[["count"]]}')"

# 17. starts-with / contains / ends-with
probe "17-string-starts-with" "$(jq -n --argjson t "$cust_table_id" --argjson e "$cust_email" \
  '{"source-table":$t,"filter":["starts-with",["field",$e,null],"a"],"aggregation":[["count"]]}')"

# 18. NOT NULL filter
probe "18-not-null-status" "$(jq -n --argjson t "$ord_table_id" --argjson s "$ord_status" \
  '{"source-table":$t,"filter":["not-null",["field",$s,null]],"aggregation":[["count"]]}')"

# 19. multi-aggregation
probe "19-multi-agg" "$(jq -n --argjson t "$ord_table_id" --argjson tc "$ord_total" \
  '{"source-table":$t,"aggregation":[["count"],["sum",["field",$tc,null]],["avg",["field",$tc,null]],["min",["field",$tc,null]],["max",["field",$tc,null]]]}')"

# 20. abs / round / ceil / floor expressions
probe "20-numeric-fns" "$(jq -n --argjson t "$ord_table_id" --argjson tc "$ord_total" \
  '{"source-table":$t,
    "expressions":{"absv":["abs",["field",$tc,null]]},
    "aggregation":[["sum",["expression","absv"]]]}')"

echo
echo "==== summary ===="
echo "passed: $PASS"
echo "failed: $FAIL"
if [[ $FAIL -gt 0 ]]; then
  echo "failures (first line each):"
  for f in "${FAILS[@]}"; do echo "  - $f"; done
  echo
  echo "Full responses logged to $PROBE_LOG"
fi
