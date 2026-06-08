# LATERAL Join Support — Test Plan & Implementation Notes

> Research output (background agent, 2026-06-07) capturing the test design and
> parser findings for implementing SQL `LATERAL` joins. Drives both the
> implementation scope (roadmap A4) and a new `test/datahike/test/pg_lateral_join_test.clj`.
> Pairs with `doc/design-alignment.md` (the LATERAL design seam).

## 1. How existing translation tests are written

SQL-translation behaviour is tested **end-to-end through the pgwire server over a
real JDBC connection**, not by calling `translate-select` directly. Closest
analogues / templates:

- `test/datahike/test/pg_fk_join_test.clj` — JOIN translation (FK-via-ref,
  inner/left, WHERE on joined table). **Best template for the new file.**
- `test/datahike/test/pg_sql_cte_test.clj` — CTEs / derived-table materialization;
  has the multi-column `rows` helper.
- `test/datahike/test/pg_array_sql_test.clj` — SRF/`unnest`/`generate_series`.

### Conventions
- `clojure.test` (`deftest`/`is`/`use-fixtures`) under kaocha.
- `^:dynamic *conn*` + `^:dynamic *port*`; `use-fixtures :each` fixture that:
  resets the lock registry, creates an in-memory Datahike db
  (`{:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}`),
  transacts a **native Datahike schema** (`:db/ident`/`:db/valueType`/`:db/cardinality`,
  `:db/unique :db.unique/identity` for the business PK), transacts seed maps, then
  `pg/start-server {"<dbname>" conn} {:port 0}` and binds `*port*`.
- A private `jdbc` connection factory; a positional `rows`/`pairs` helper reading
  every column via `.getString` (string form keeps golden assertions stable).
- Schema is Datahike-native (idents like `:customer/id`); refs use `:db.type/ref`
  storing the target eid; SQL `c.id` maps to `:customer/id`. FK columns join via
  `a.fk = b.id` (auto-detected when RHS is `:db.unique/identity`).

Multi-column `rows` helper to copy:
```clojure
(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs ^long %) (range 1 (inc n)))))
          acc)))))
```

Create `test/datahike/test/pg_lateral_join_test.clj`, db name `"lat"`, shared
`customers`/`orders` schema.

## Parser findings (jsqlparser 5.2, verified via REPL)

| FROM form | `.getRightItem` | `.isSimple` | `.isCross` | `.isLeft` | detect LATERAL via |
|---|---|---|---|---|---|
| `t1, LATERAL (subq) sub` | `LateralSubSelect` | true | false | false | right item is `LateralSubSelect` |
| `t1 CROSS JOIN LATERAL (subq) sub` | `LateralSubSelect` | false | true | false | right item is `LateralSubSelect` |
| `t1 LEFT JOIN LATERAL (subq) sub ON true` | `LateralSubSelect` | false | false | true | right item is `LateralSubSelect` |
| `t1, LATERAL generate_series(1,t1.n) g` | `TableFunction` | true | false | false | `.getPrefix tf` == `"LATERAL"` |
| `t1 CROSS JOIN LATERAL generate_series(1,t1.n) g` | `TableFunction` | false | true | false | `.getPrefix tf` == `"LATERAL"` |

- Subquery LATERAL always surfaces as `LateralSubSelect` (`.getPlainSelect` / `.getSelect`,
  `.getAlias`). Clean additive branch.
- SRF LATERAL has **no wrapper class** — a bare `TableFunction`; the only signal is
  `.getPrefix == "LATERAL"`. Must check the prefix or correlated SRFs silently route
  through the constant path and `t.n` fails to resolve (`::corr`).
- Comma form = CROSS semantics, never LEFT. `LateralSubSelect` carries its own
  `.getLimit`/`.getOrderByElements`; delegate to the inner select compile.

Reuse seams (`src/datahike/pg/sql/stmt.clj`): `materialize-table-function` (649, takes
`eval-fn` for correlated args), `srf-const-eval` (629), `table-fn->virtual-table` (732),
`*from-bindings*` Column branch (expr.clj ~1784), scalar-subquery branch.
UPDATE per-row binding: `server.clj` `build-update-tx-for-bindings` (~1944).

## 2. Test cases — shared schema/seed

```
customers: :customer/id (long identity), :customer/name (string)[, :customer/min_amount (long)]
orders:    :order/id (long identity), :order/customer (ref→customer),
           :order/amount (long), :order/created (long, sortable "day")
seed customers: {1 "Alice"} {2 "Bob"} {3 "Carol"}      ;; Carol has ZERO orders
seed orders: {10 c1 100 d1} {11 c1 200 d2} {12 c1 300 d3} {13 c1 400 d4}  ;; Alice 4
             {20 c2 500 d1}                                                ;; Bob 1
```
SQL writes `o.customer_id = c.id` against ref `:order/customer` (FK-via-ref auto-join).
Fallback if correlation-inside-lateral is too hard initially: non-ref
`:order/customer_id (long)` plain value equality (note divergence in docstring).

- **CASE 1 (core) Top-N via CROSS JOIN LATERAL** — `... CROSS JOIN LATERAL (SELECT o.amount FROM orders o WHERE o.customer_id=c.id ORDER BY o.created DESC LIMIT 2) o ORDER BY c.name, o.amount DESC`. Carol dropped. Expect `["Alice" "400"] ["Alice" "300"] ["Bob" "500"]`.
- **CASE 2 (core) LEFT JOIN LATERAL ... ON true** — same inner, Carol kept with NULL. Expect CASE 1 rows + `["Carol" nil]`.
- **CASE 3 (core) Correlated aggregate** — `LEFT JOIN LATERAL (SELECT count(*) n, sum(o.amount) total FROM orders o WHERE o.customer_id=c.id) s ON true`. Aggregate-no-GROUP-BY always one row. Expect `["Alice" "4" "1000"] ["Bob" "1" "500"] ["Carol" "0" nil]`.
- **CASE 4 (important) Correlated SRF generate_series** — fixture `thing {1 n1}{2 n3}{3 n0}`; `... CROSS JOIN LATERAL generate_series(1, t.n) AS g ORDER BY t.id, g.g`. n=0 empty→dropped. Expect `["1" "1"] ["2" "1"] ["2" "2"] ["2" "3"]`.
- **CASE 5 (important) Correlated unnest** — `box {1 ["a" "b"]}{2 []}{3 ["c"]}`; `... b, LATERAL unnest(b.tags) AS u ...`. Expect `["1" "a"] ["1" "b"] ["3" "c"]` (box 2 dropped; LEFT variant → box2 NULL).
- **CASE 6 (important) comma vs CROSS equivalence** — assert `(= (rows A) (rows B))`, both `["Alice" "400"] ["Bob" "500"]`.
- **CASE 7 (important) multiple correlations** — inner WHERE `o.customer_id=c.id AND o.amount>=c.min_amount`. With Alice min 250/Bob 1000/Carol 0: expect `["Alice" "300"] ["Alice" "400"]`.
- **CASE 8 (core) CROSS vs LEFT row-count delta** — simplest inner, no LIMIT. CROSS=5 rows (Carol absent), LEFT=6 (+Carol nil). Pins the defining semantics.
- **CASE 9 (nice) mixed outer+inner projection w/ inner alias** — `SELECT c.id,c.name,o.order_id,o.amount ... (SELECT o.id AS order_id, o.amount ... LIMIT 1)`. Expect `["1" "Alice" "13" "400"] ["2" "Bob" "20" "500"]`.
- **CASE 10 (stretch/defer) chained LATERAL referencing prior LATERAL** — mark pending.

## 3. Implementation priority order
1. Subquery `CROSS JOIN LATERAL` + comma form (nested-loop core; CASE 1,6,8,9).
2. `LEFT JOIN LATERAL ... ON true` null-extension (CASE 2,8-LEFT) — highest-value distinction.
3. Correlated aggregate inner (CASE 3) — mostly falls out of #1/#2 + "aggregate→one row".
4. Multiple-correlation WHERE (CASE 7) — falls out of #1 if `*from-bindings*` carries whole row.
5. Correlated-arg SRF generate_series/unnest (CASE 4,5) — `TableFunction` + `.getPrefix=="LATERAL"`, per-row eval-fn. Independent; parallelizable.
6. Chained/nested LATERAL (CASE 10) — defer.

## 4. PG-semantics gotchas
- **G1** CROSS (and comma) drops empty-inner outer rows; LEFT ... ON TRUE keeps once with NULLs. THE defining behaviour.
- **G2** Aggregate-only inner (no GROUP BY) always emits one row (count=0, sum/max/min=NULL) → CROSS and LEFT identical for it.
- **G3** LIMIT/ORDER BY are evaluated INSIDE the lateral per outer row (Top-N per group, not global). Delegate to inner compile; don't hoist.
- **G4** `generate_series(1,0)` empty (inclusive of stop), `unnest('{}')` empty → dropped under CROSS, NULL under LEFT.
- **G5** SRF LATERAL has no wrapper; rely on `.getPrefix`.
- **G6** Correlation must resolve through FK-via-ref rewrite with one side a bound constant. Verify early; fallback non-ref schema.
- **G7** Outer ORDER BY/LIMIT/WHERE run over the combined stream (separate from inner's). Use deterministic outer ORDER BY in tests.
- **G8** Single-column SRF aliased `AS g` → column named `g`; unaliased → function name.
- **G9** `ON true` is the only ON to support first; arbitrary ON predicates can be deferred (apply as combined-row filter if cheap).
