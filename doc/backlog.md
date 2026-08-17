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

> **FIXED (7b2b618)**

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

> **FIXED (ff6e28d)**

`SELECT p || '{"z":9}'` yields `{"a":1}{"z":9}`. Only the UPDATE SET
path has the jsonb branch.

### `?|` and `?&` return zero rows against `array[...]`

> **FIXED (ff6e28d)**

The right-hand side arrives as a `PgArray` record and the implementation
iterates it as a map, so every test fails.

### `jsonb_agg` / `jsonb_object_agg` are not aggregates

> **jsonb_agg FIXED (ff6e28d); jsonb_object_agg still open**

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

> **FIXED (7b2b618)**

Key order is alphabetical, PostgreSQL is **length-first then bytewise
over UTF-8**; separators are compact, PostgreSQL emits `": "` and `", "`;
numbers go through doubles, PostgreSQL keeps `numeric` scale (`1e3` →
`1000`, `1.00` stays `1.00`, integers stay exact past 2^53).

A verified writer exists (16/17 byte-identical against PG 17) but cannot
land alone — see the equality entry above; the two must change together.

---

### We accept JSON that PostgreSQL rejects

Running a 110-line slice of PostgreSQL's own `src/test/regress/sql/jsonb.sql`
found **27 statements where we return a value and PostgreSQL raises
22P02**. Our parser is Jackson, which is lenient exactly where
PostgreSQL's is strict:

    '"abc'::jsonb        unclosed quote        -> we return `"abc`
    '"abc\ndef"'::jsonb  unescaped newline     -> we return it
    '"\v"'::jsonb        invalid escape        -> we return it
    '01'::jsonb          leading zero          -> we return `01`

PostgreSQL's rules are in `src/common/jsonapi.c`: the valid escape set
is exactly `" \ / b f n r t u`, a raw byte below 0x20 inside a string
must be escaped, and the number grammar is strict RFC 8259. Accepting
malformed input is how invalid documents reach storage, so this is a
gate rather than a cosmetic gap.

## S2 — wrong errors

### `json` accepts operators PostgreSQL does not have

> **FIXED (496a1da + follow-up)**

PostgreSQL has **no** `=`, `<>`, `<`, `>`, `@>`, `?` on `json` — the type
has no btree/hash opclass at all. We accept them silently. PostgreSQL
raises 42883, or 42704 for an index attempt.

### `pg_dump` cannot run against us

`pg_dump` fails immediately on `pg_catalog.pg_is_in_recovery()`, which
is unimplemented. Our own dump command works and round-trips json
verbatim and jsonb canonically, but `pg_dump` is the interop path most
users reach for — and dump/restore fidelity is what makes any future
storage-representation change reversible. Small function, high leverage.

### A value-size cap surfaces as XX000 with a raw Clojure exception

> **FIXED (496a1da)**

`:db/maxLength` / `:max-string-length` fire correctly but emerge as
`XX000: INSERT error: clojure.lang.ExceptionInfo: String value for
:cap/d exceeds max length 512 (was 2008) {:error :transact/max-length,
...}` — internal ex-data and all. PostgreSQL's class for a size ceiling
is 54000 `program_limit_exceeded`. Until this is mapped, the per-column
cap is not usable by clients.

### Unknown functions leak internal errors

> **FIXED (496a1da)**

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

- ~~`#>`, `#>>`~~ — **FIXED (496a1da)**. The check now excludes those two
  exact tokens rather than the `#` character.
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

## Latent — not currently reachable, but will be

### Cardinality-many BigDecimal attributes collapse scale-distinct values

Writing `1.00M`, `1.0M`, `1.000M` to one `:db.cardinality/many`
`:db.type/bigdec` attribute leaves a single value, `1.00M`. Datahike's
`compare-value` bottoms out in `BigDecimal.compareTo`, which is
scale-blind, so the later values are "already present".

Cardinality-ONE is unaffected — verified with and without `:db/index`,
the last write wins and its scale is preserved. SQL scalar columns are
cardinality-one, so this does not affect jsonb or numeric columns today.
It would affect any future multi-valued numeric column.

### Large jsonb values are stored inline, with no promotion threshold

A jsonb column is a `:db.type/string` datom value, so an arbitrarily
large document sits inline in the index. There is no out-of-line
promotion and no size guard beyond `:max-string-length`.

Datalevin promotes at **497 bytes** (`+val-bytes-wo-hdr+`) into a
separate `datalevin/giants` DBI, keeping a truncated key plus a
reference in the main index, and zstd-compresses above a further
threshold. PostgreSQL TOASTs at ~2 KB for the same reason: keep the
tuple small so scans that do not read the value stay cheap.

Measured so far: 1000 rows x 20 KB documents (~19 MB) showed **no**
penalty on queries that never touch the jsonb column — `count(*)`,
point lookup and a scan on a sibling text column were all within noise
of an identical table without the column. That is consistent with AEVT
grouping datoms by attribute, so an attribute-scoped scan never walks
the big values.

The risk is therefore not disproved, only unobserved at that scale: an
entity-ordered walk (EAVT) does put a row's big value adjacent to its
small ones, and node size affects what a fetch pulls in. Worth
re-measuring with fewer, much larger documents and with a file backend
before concluding. If it does bite, the fix is a promotion threshold —
`:db.type/store-ref` exists and is GC-marked, though datahike's own
schema note argues against it for structured data, and content-id
identity would give byte equality rather than jsonb equality.

### A numeric LITERAL loses its scale

`SELECT 1.10::numeric` and `SELECT 1.10` both answer `1.1`; PostgreSQL
answers `1.10`. A numeric literal is parsed as a double before anything
else sees it. A numeric COLUMN is unaffected — its declared scale
restores the value — and `to_jsonb` of such a column is correct, so this
is a literal-parsing gap rather than a jsonb one. It does mean
`to_jsonb(1.10::numeric)` is wrong today.

### An ARRAY literal passed to jsonb_build_object leaks record internals

`jsonb_build_object('a', ARRAY[1,2])` yields
`{"a": {":dims": null, ":elements": null, …}}`. `to_jsonb(ARRAY[1,2])`
is correct, so the dispatch is right; the value reaching the builder is
an all-nil map rather than a PgArray, i.e. the ARRAY constructor is not
materialised when it appears as a function-call argument.

### jsonb numeric limits are not enforced

PostgreSQL's numeric caps display scale at 16383 and integer digits at
131072, raising 22003 `value overflows numeric format` beyond that.
`BigDecimal` is strictly more permissive, so a document PostgreSQL
rejects would be accepted. Needs an explicit range check at parse time.

### `BigDecimal.hashCode()` is scale-sensitive

`(hash 1M)` and `(hash 1.00M)` agree (both 31); `.hashCode()` gives 31
and 3102. Clojure's `=`/`hash` reproduce PostgreSQL's scale-insensitive
numeric equality for free, but only through `clojure.core/hash` — any
Java-interop collection (`HashMap`, `HashSet`) silently breaks the
invariant.

---

## Test-surface note

Before this round the entire JSON surface was **6 assertions**, all
about ingest canonicalization, and all three vendored driver suites skip
their JSON tests. That is why most of the S1 entries above survived. A
`bb cross-engine --record` oracle file for jsonb is the cheapest way to
stop this recurring.
