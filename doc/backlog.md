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

### jsonb DISTINCT and GROUP BY are still text comparisons

> Equality itself is **FIXED on `fix/jsonb-equality`** — see below. This
> entry now covers only DISTINCT and GROUP BY.

`SELECT count(DISTINCT j)` over `1.00` / `1` answers 2 where PostgreSQL
answers 1, and `GROUP BY j` makes two groups where PostgreSQL makes
one.

Unlike `=`, this cannot be fixed by swapping in a comparator, because
DISTINCT and GROUP BY are not predicates here: `has-distinct?` works by
NOT adding the entity var to `:with-vars`, so deduplication is
datalog's set semantics over the projected `:find` tuple — and the
projected value is the canonical TEXT, in which `1.00` and `1` differ.

Fixing it means projecting a scale-NORMALISED key and rendering a
representative of the group, which needs a decision:

- PostgreSQL displays a member of the group — `1.00` here, i.e. the
  first encountered — but the choice is implementation-defined (hash
  aggregation order), so any member is defensible.
- Projecting the normalised key directly is the cheapest fix and gives
  the right COUNT, but displays `1` where PostgreSQL displays `1.00`.
- Full fidelity means a hidden normalised grouping key plus an
  aggregate that picks the representative, on the same machinery
  GROUP BY already uses for hidden find elements.

The count being wrong is the real defect; which representative shows is
cosmetic.

### jsonb equality was reachable from only one operand shape

> **FIXED on `fix/jsonb-equality`**

`jsonb-eq?` was correct but wired in at exactly one site — the WHERE
path taken when the right operand is a BARE literal. `j = '1'` was
right; `j = '1'::jsonb` (a CastExpression), `'1'::jsonb = j`,
`a.j = b.j` and any comparison in the SELECT list fell through to `=`
on the canonical text and answered false. Equi-joins were worse than
wrong-answer: they unified on a shared logic var, making the join key
text equality.

### jsonb ORDER BY / `<` / `>` / MIN / MAX use text order

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

> **FIXED** — `json`/`jsonb` are now a `cast-category`, handled in the
> shared `cast-scalar`, so both the constant-folded literal path and
> `translate-cast-expr` validate and (for jsonb) canonicalise.

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

> **FIXED** — validation now runs on the cast and on the write path; the
> regression slice went from 27 accepted-what-PG-rejects to 12, and from
> 18 to 51 identical lines of 65. The remainder is the backslash-literal
> bug below contaminating the JSON escape tests.

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

### Casting NULL to a string type yields the empty string

> **FIXED on `fix/null-string-cast` (PR #38)**

`SELECT NULL::text IS NULL` answers **false**. NULL cast to `text`,
`varchar` or `char` becomes `''` rather than staying NULL, so every
NULL-aware construct downstream silently takes the wrong branch:

    length(NULL::text)                  -> 0    PG: NULL
    NULL::text = ''                     -> true PG: NULL
    coalesce(NULL::text, 'FELLBACK')    -> ''   PG: 'FELLBACK'
    NULL::varchar IS NULL               -> f    PG: t
    CASE WHEN NULL::text IS NULL
         THEN 'a' ELSE 'b' END          -> 'b'  PG: 'a'

`NULL::bool` proves the mechanism: it raises `invalid input syntax for
type boolean: ""`, i.e. NULL was stringified to `''` and that was then
parsed as a boolean. `NULL::int`, `::numeric`, `::date` and `::jsonb`
are all correct, and bare `NULL IS NULL` is correct — it is specific to
the string casts.

Found via asyncpg's `test_prepare_03`, which prepares
`SELECT CASE WHEN $1::text IS NULL THEN <default> ELSE $1::text END`
and gets the ELSE branch for a NULL argument. Reproduces without any
parameter, so it is a cast bug rather than a protocol one.

### MIN/MAX/GREATEST/LEAST crash on every non-numeric type

> **FIXED on `fix/order-aggregates`**

`SELECT max(name) FROM users` raised
`class java.lang.String cannot be cast to class java.lang.Number`.
`filter-min`/`filter-max` were `clojure.core/min`/`max` and
`greatest`/`least` mapped straight to them; all four are numeric-only.
Text, date, timestamp and time all died.

It survived the suite because it only fires when two or more values are
actually compared — `apply max` on a one-element seq returns it
untouched, so single-row groups and WHERE-narrowed aggregates passed.

The same change fixes an error-class bug: for the types PostgreSQL has
no min/max aggregate for we raised a ClassCastException where PG raises
42883. Those are `boolean` and `uuid`, absent on every release including
master. `bytea` has an aggregate upstream and is supported, ordered by
unsigned byte value as `byteacmp` does. A jsonb value is a
String by then and indistinguishable from text, so `max(jsonb)` answers
instead of raising — closing that needs the declared column type at the
aggregate layer, which it does not carry.

### DDL loses date-ness and integer width

`CREATE TABLE t (id int, d date)` reports `d` as `timestamp without
time zone` and `id` as `bigint` through `format_type`/`pg_attribute`,
so `SELECT d` renders `2020-01-01 00:00:00` where PostgreSQL renders
`2020-01-01`. An explicit `d::date` is correct, so the cast path knows
the type and the column path does not.

Drivers read these OIDs to pick codecs, so this is more than cosmetic.
Found while fixing MIN/MAX, where `max(d)` inherited the same wrong
rendering as the bare column.

### Three-valued logic is wrong in scalar position

A comparison or boolean operator in the SELECT list does not propagate
NULL. PostgreSQL answers NULL for all but one of these; we answer a
definite boolean:

                      ours   PG
    NULL = NULL         t    NULL
    1 = NULL            f    NULL
    NULL <> 1           t    NULL
    NOT NULL            t    NULL
    true AND NULL       f    NULL
    false AND NULL      t    f
    true OR NULL      NULL   t

Note the last two are wrong in the *other* direction: `false AND NULL`
is FALSE in SQL (the false operand decides it) and `true OR NULL` is
TRUE, and we get both backwards.

`WHERE` is mostly right, because the datalog lowering prepends
`(not= ?v :__null__)` guards — `v = 10`, `v <> 10`, `v = NULL` and
`v IS NULL` all match PostgreSQL. Two defects remain there:

- `WHERE NOT (v = 10)` **includes** the NULL row; PostgreSQL excludes
  it (UNKNOWN negates to UNKNOWN, not TRUE).
- Projecting a comparison, `SELECT v = 10`, yields `false` for a NULL
  input where PostgreSQL yields NULL.

Found while fixing the NULL-cast bug above. Bigger than it looks: it
touches every comparison and boolean operator, and the WHERE and
projection paths lower differently, so they need fixing together.

### Schema-qualified set-returning functions in FROM fail

> **FIXED on `fix/string-agg`**

`SELECT count(*) FROM pg_catalog.generate_series(1,3)` raised the
internal `Query for unknown vars: [?_eid]`. `materialize-table-function`
matched the RAW function name, so anything schema-qualified missed its
`cond`, returned nil, and left the FROM item with no relation at all —
`count(*)` then emitted an entity var nothing bound.

PostgreSQL resolves the qualified and unqualified forms to the same
function through search_path, and pgjdbc writes the qualified one.

### Unordered aggregates do not preserve input order

`array_agg(id)` over rows 1..4 gives `{1,4,2,3}`; PostgreSQL gives
`{1,2,3,4}`. Same for `json_agg`, `jsonb_agg` and `string_agg`. The
order varies between runs, so it is set/bag iteration order rather than
a fixed permutation.

SQL leaves this UNSPECIFIED without an in-aggregate `ORDER BY`, and
PostgreSQL's own order is incidental (a parallel plan changes it), so
this is not a correctness defect — but clients do rely on it in
practice, and every differential test of an unordered aggregate will
flag it. Adding `ORDER BY` inside the aggregate gives the right answer
today.

Making it match would mean ordering the collected values by entity id,
which costs a sort on every aggregate for a guarantee PostgreSQL does
not itself make.

### `string_agg` is not an aggregate

> **FIXED on `fix/string-agg`**

`SELECT string_agg(nm, ',') FROM t` returned ONE ROW PER INPUT ROW
(`a`, `b`) instead of the single concatenated `a,b`, and the ORDER BY
inside it is ignored. Same defect `jsonb_agg` had before it was
registered in `sql-aggregate->datalog` — it is a per-row function that
was never folded over the group.

`array_agg`, `json_agg` and `jsonb_agg` all handle `ORDER BY` inside
the aggregate correctly, so the ordered-aggregate machinery exists and
`string_agg` just is not wired into it.

### `json_agg` over composites omits PostgreSQL's newline

PostgreSQL separates `json_agg` elements with `", "`, and adds a
further `"\n "` when the element category is ARRAY or COMPOSITE
(`json_agg_transfn`, json.c: "add some whitespace if structured type
and not first item"). So `json_agg(t)` over a table is

    [{"id":1,"nm":"a"}, \n {"id":2,"nm":"b"}]

and ours is that without the newline. Scalar elements are unaffected
and already match. Cosmetic, but it is text a client can compare.

### Whole-row references

> **FIXED on `feat/whole-row-refs`**

`SELECT t FROM t` returned NULL rather than the composite `(1,a)`, and
`to_json(t)`, `row_to_json(t)` and `json_agg(t)` inherited it —
`json_agg(t)` answering `[null, null]`.

### The asyncpg suite gives different answers in CI and locally

Same commit, freshly restarted server: **95 passed / 74 failed locally,
45 / 127 in CircleCI**. Both are stable — CI produced byte-identical
counts and the same six resolved tests on `main` (build 1279) and on
`fix/jsonb-conformance` (1342), and two local runs agreed with each
other.

The two environments disagree in *both* directions, so neither is simply
"more broken": six tests pass in CI and fail locally
(`test_prepare_03`, `test_invalid_input`, the two `executemany` ones,
two custom-codec ones), while `test_connect_params` does the reverse.
One local failure is `test_prepare_03` asserting `'?v4' != 'aaa'` — a
datalog variable reaching the client as a value, which is an S1-shaped
symptom whatever causes the divergence.

Until this is understood, `expected-failures.txt` tracks CI and a local
run will report spurious regressions. Candidate causes not yet ruled
out: a different asyncpg build (CI compiles it; the local `.venv` may
not), Python 3.11 in CI vs 3.12 locally, and accumulated tables in the
long-lived local database changing what introspection returns.

## Test-surface note

Before this round the entire JSON surface was **6 assertions**, all
about ingest canonicalization, and all three vendored driver suites skip
their JSON tests. That is why most of the S1 entries above survived. A
`bb cross-engine --record` oracle file for jsonb is the cheapest way to
stop this recurring.
