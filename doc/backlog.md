# Known defects backlog

Conformance gaps found by differential testing against a real PostgreSQL
17 instance, plus code review. Each entry says what the wrong behaviour
IS, not just what is missing — a wrong answer and an unimplemented
feature need different urgency.

**Severity key**

- **S1 silent wrong answer** — the client cannot tell it got bad data.
  Fix before anything else.
- **S2 wrong error** — fails, but with the wrong SQLSTATE or an internal
  message. Breaks error handling, not data.
- **S3 missing** — honestly unimplemented; a client sees a clear error.

---

## S1 — silent wrong answers

### String literals escape-process backslashes

`SELECT length('a\tb')` → **3**, PostgreSQL → **4**.

With `standard_conforming_strings = on` — which we report — a backslash
in an ordinary single-quoted string is a literal backslash. Only
`E'...'` strings process escapes. We process `\t \n \r \b \f`
everywhere, so any literal containing them is silently corrupted:
`'C:\temp'` is 6 characters for us and 7 for PostgreSQL.

Not JSON-specific — this affects every string literal. It is also what
made one case of the jsonb canonical-form differential disagree, since
the tab reached the JSON parser already unescaped.

### jsonb `->` drops the row

`jsonb-get` returns `nil` for a missing key, and a datalog
function-binding that yields `nil` filters the row out of the result.
`SELECT p->'missing', p->>'missing' FROM t` returns **zero rows** where
PostgreSQL returns one row of two NULLs. `jsonb-get-text` has the
`:__null__` guard; `jsonb-get` does not.

### jsonb equality, DISTINCT and GROUP BY are text comparisons

PostgreSQL's jsonb `=` is **structural and numeric-scale-insensitive**:
`'1.00'::jsonb = '1'::jsonb` is true, `jsonb_hash` agrees, and DISTINCT
over `1.00 / 1.0 / 1` collapses to one row — while their *texts* differ.
We compare canonical text, so we answer false for all of these.

No canonical byte or text form can fix this: PostgreSQL deliberately
preserves display scale while comparing scale-insensitively. Equality
needs a structural comparator whatever we store.

### jsonb ORDER BY / `<` / `>` / MIN / MAX use text order

PostgreSQL orders jsonb by type class (`Object > Array > Boolean >
Number > String > Null`), then objects by pair-count before comparing
key/value pairs interleaved. We sort by canonical text, which will never
approximate it. Note PostgreSQL's own documented anomaly: an empty
top-level array sorts *below* every scalar, and upstream has declared it
unfixable because btree indexes depend on it.

### `||` on jsonb is string concatenation in SELECT and WHERE

`SELECT p || '{"z":9}'` yields `{"a":1}{"z":9}`. Only the UPDATE SET
path has the jsonb branch.

### `?|` and `?&` return zero rows against `array[...]`

The right-hand side arrives as a `PgArray` record and the implementation
iterates it as a map, so every test fails.

### `jsonb_agg` / `jsonb_object_agg` are not aggregates

`SELECT jsonb_agg(id) FROM t` returns one row per input row instead of
one array. They are per-row functions and are absent from
`sql-aggregate->datalog`.

### `::jsonb` is a no-op cast

`cast-category` has no json branch, so the value passes through
untouched while the wire OID is still set to 3802 — the value and its
advertised type disagree.

### Function calls in INSERT … VALUES are not evaluated

`INSERT INTO t VALUES (1, repeat('x',10000))` stores the 18-character
string `repeat('x',10000)`. `SELECT repeat('x',10)` on its own is
correct, so this is specific to the VALUES row path.

### jsonb canonical form differs from PostgreSQL

Key order is alphabetical, PostgreSQL is **length-first then bytewise
over UTF-8**; separators are compact, PostgreSQL emits `": "` and `", "`;
numbers go through doubles, PostgreSQL keeps `numeric` scale (`1e3` →
`1000`, `1.00` stays `1.00`, integers stay exact past 2^53).

A verified writer exists (16/17 byte-identical against PG 17) but cannot
land alone — see the equality entry above; the two must change together.

---

## S2 — wrong errors

### `json` accepts operators PostgreSQL does not have

PostgreSQL has **no** `=`, `<>`, `<`, `>`, `@>`, `?` on `json` — the type
has no btree/hash opclass at all. We accept them silently. PostgreSQL
raises 42883, or 42704 for an index attempt.

### Unknown functions leak internal errors

`SELECT json_build_object('a',1)` →
`ERROR: Unknown function 'json_build_object in [(json_build_object "a" 1) ?v1]`.
PostgreSQL raises 42883 `undefined_function`. `p - 'b'` similarly
surfaces a raw `ClassCastException`.

### `json` reports two different OIDs

`compute-schema-oids` hardcodes `"json"` → 3802 while the catalog path
resolves it to 114, so `SELECT j` and `SELECT *` on the same column can
disagree.

### Derived table over a table function exposes no column names

`SELECT x.r FROM (SELECT s.r FROM generate_series(1,2) AS s(r)) AS x`
fails while `SELECT *` over the same subquery works. This is the last
unfixed hop in pgjdbc's `TypeInfoCache` probe, so `ResultSet.getObject`
still fails on any introspected column type.

### `jsonb_insert` ignores `insert_after`

It delegates to `jsonb_set` and does no array insertion, so it returns a
plausible wrong answer rather than an error.

---

## S3 — missing

- `#>`, `#>>` — **parse fine** as `JsonExpression`; blocked only by the
  `#` operator-character check in `sql/unsupported-op-chars`. Cheap.
- `#-`, `@?` — do **not** parse; need a pre-parse rewrite or a parser
  change. Different cost tier.
- The whole `json_*` function family (`json_build_object`, `json_agg`,
  `to_json`, `row_to_json`, `json_each`, …). PostgreSQL mirrors every
  `jsonb_*` name; we have none.
- SQL/JSON path: `jsonb_path_query` and friends, `@@`.
- `jsonb_each`, `jsonb_array_elements` and friends are not
  set-returning; they serialize the whole collection into one cell.
- `jsonb_object_keys` returns a JSON array string rather than rows.
- `chr()`.
- `CREATE INDEX` is accepted and discarded — `(empty-result "CREATE
  INDEX")`. No GIN analogue exists, and the current operator lowering
  (an opaque `:in`-supplied closure) forecloses index use by
  construction.
- Data-modifying CTEs raise 0A000; PostgreSQL executes the inner DML and
  feeds RETURNING to the outer query.

---

## Test-surface note

Before this round the entire JSON surface was **6 assertions**, all
about ingest canonicalization, and all three vendored driver suites skip
their JSON tests. That is why most of the S1 entries above survived. A
`bb cross-engine --record` oracle file for jsonb is the cheapest way to
stop this recurring.
