# Changelog

All notable changes to pg-datahike.

## [Unreleased]

### Datahike 0.8.1895

- Retracting an attribute no longer leaves an empty schema entry behind
  (datahike #1089). Every DROP TABLE left one per column: after 400
  CREATE/DROP cycles the schema map held 2527 entries, 2400 of them
  empty, against 127 now.

### CHECK constraints are evaluated like SELECT

CHECK constraints, domain checks and ON CONFLICT ... WHERE are evaluated by the SELECT translator (`datahike.pg.sql.row-eval`). The expression's columns become typed parameters of a one-row SELECT, which is translated once per expression. The interpreter it replaces is deleted. That interpreter treated every shape it did not know as satisfied, and compared only numbers with `<`/`>`. As a result:

- `CHECK (name > 'm')`, `CHECK (d < '2030-01-01')`, `CHECK (s LIKE 'a%')`, regex, `IS DISTINCT FROM`, jsonb operators and array subscripts accepted every row.
- Errors inside a CHECK (`x / 0`) were swallowed.
- `ON CONFLICT DO UPDATE ... WHERE s LIKE 'a%'` never updated.

Violations report PostgreSQL's messages: `new row for relation "t" violates check constraint "..."` and `value for domain d violates check constraint "..."`.

Other fixes in this change:

- Columns are typed exactly as declared: modifiers (`bit(3)`, `char(3)`), enum types and quoted identifiers such as Django's `"age"` are respected. A subscript's index may itself be a column. Shapes that can't be evaluated per row raise 0A000 instead of being evaluated wrongly.
- CREATE TABLE refuses a subquery in a CHECK (`cannot use subquery in check constraint`), as PostgreSQL does.
- UPDATE validates CHECK, domain and enum constraints through the same plan INSERT uses. The server's second copy of that code is removed.
- Enums compare in declaration order in value position (`SELECT 'happy'::mood > 'sad'`, CHECKs), not by their labels' text.
- An enum label outside the enum (`m > 'xyz'`) raises 22P02 when the statement is read, as PostgreSQL's parser does, in WHERE and in value position alike. `COLLATE` inside a CHECK, `timestamptz(3)`/`time(2)` columns and quoted mixed-case enum names evaluate correctly. CREATE DOMAIN refuses a subquery in its CHECK.
- Writes keep a coerced `false`, and SQL NULL from `INSERT ... SELECT`. Six write paths wrapped the column coercion in `(or (coerce v) v)`: a coerced `false` fell back to its input text, and a NULL fell back to the query engine's internal sentinel, which Datahike then rejected.
- A plan that is only a chain of function calls (a FROM-less one-row SELECT) runs without `d/q`, so per-row CHECKs cost about what the rest of the insert does.
- A cast of a bound temporal value (`CAST($1 AS date)` over a `java.util.Date`) became NULL, because the fold read `Date.toString`. It now converts the value.
- An untyped literal compared with a `time`/`timetz` expression is read as that type.
- DEFAULT:
  - A numeric literal keeps its text, so `numeric DEFAULT 1.50` keeps its scale.
  - The server's second `eval-default`, which used the wall clock for `now()`, is removed.
  - `now() AT TIME ZONE` is folded into `now()` only for UTC. Other zones are refused (0A000) instead of silently storing the wrong time.

### Text is read by PostgreSQL's input functions

`datahike.pg.input` ports the input functions of bool, int2/int4/int8, oid, float4/float8 and uuid (boolin, pg_strtoint*, uint32in_subr, float4in/float8in, uuid_in). numeric keeps `coerce-numeric`, which already matched `numeric_in`. Every path from text to these types now goes through them, with PostgreSQL's SQLSTATEs and messages:

- An untyped literal compared with a typed operand. `WHERE b = 'false'` found nothing, because a successfully read `false` was mistaken for "no coercion". `WHERE i = 'abc'` answered no rows instead of 22P02.
- Casts from text. `'1.5'::int` rounded to 2 instead of raising 22P02, and the error named `numeric`.
- INSERT/UPDATE, COPY and `pg_input_is_valid`/`pg_input_error_info`, which reports 22003 for out-of-range values.
- Array elements. Integer arrays keep their declared width (`int2[]` was read as int8) and are range-checked.
- Text-format Bind parameters. The Java decoder read every bool except t/true/1 as false. Invalid numbers were XX000.

The accepted syntax is PostgreSQL 16+'s:
- `0x`/`0o`/`0b` integers and `_` separators;
- `-1` as an oid (4294967295);
- `inf`, hex floats, and float underflow as 22003.

The duplicated bool and uuid parsers in `coerce`, `copy` and `PgParamCodec` are gone. Two new fuzzer classes, `litcmp` and `scalarin`, exercise these paths.
### PostgreSQL's catalog is generated, not transcribed

- `src/datahike/pg/pg_catalog.edn` holds the pg_type, pg_cast, pg_proc and
  pg_operator rows of PostgreSQL 17.7, generated from the release's
  `src/include/catalog/*.dat` by `bb gen-catalog`. Type lengths,
  categories, preferred types, collations, array types, implicit and
  explicit casts, and aggregate signatures are now derived from it instead
  of hand-written tables. A test fails when the file is edited by hand or
  disagrees with the pinned source.
- Two hand-copied facts were wrong: `"char"` is category Z, not S. And
  `time -> timetz` is an implicit cast; it stays excluded from resolution,
  in a named set, until comparisons convert the time operand (with it,
  `time = timetz` would answer false instead of raising 42883).

### Aggregates resolve their argument types

- Aggregate and window-function calls are resolved against every
  `pg_proc.dat` aggregate/window signature (generated from the pinned
  catalog). A call with no matching overload raises 42883, and an untyped
  literal that fits several candidates raises 42725, as in PostgreSQL.
  `sum(text)` used to fail with an internal ClassCastException, `sum(text)
  OVER ()` with 0A000, and `bool_and(int)` answered false.

### Values render and parse by their SQL type

- One output function (`types/->pg-text`) serves the wire, `::text`, `||`,
  `concat`, array elements and record fields, and dispatches on the value's
  TYPE before its JVM class. money prints `$1,234.50`; time keeps its seconds
  (`10:00:00`); fractional seconds print PostgreSQL's way (`.12`, not
  `.120`); timestamp array elements no longer use java.util.Date.toString in
  the JVM's zone; ARRAY and ROW take element/field types from the
  expressions.
- `AT TIME ZONE` converts (it returned its operand unchanged) and swaps
  timestamp/timestamptz as PostgreSQL does; numeric zones are POSIX-style.
- time/timetz and money input validate (22007/22008/22P02) instead of
  passing text through, on casts and on writes; stored time values are
  canonical.
- A bare `(a, b)` is a row (it leaked query variable names); `f((a, b))`
  passes one row argument.
- `timetz` columns are typed as such (CREATE TABLE had its own copy of the
  type-hint table, which lacked the short name).
- SQL NULL never renders as the internal `:__null__` sentinel.
- Tests that asserted the old rendering are corrected to PostgreSQL's
  expected output, including four admitted money.sql strict slices.
- The differential fuzzer gains type-directed classes over a second table.

### Differential fuzzing is a per-commit gate

- `datahike.fuzz.differential` (`bb fuzz`) generates SELECT, prepared and
  DML samples, runs them on PostgreSQL 17.7 and pg-datahike, and compares
  rows, or SQLSTATEs when both fail. The `differential-fuzz` CI job runs a
  fixed seed against a 17.7 sidecar and gates deployment. Known divergences
  are listed with reasons in `test/integration/fuzz/expected-divergences.edn`.
- `bb fncov` measures function breadth by calling every buildable
  `pg_catalog` overload on both servers.
- `doc/integration-testing.md` maps each behaviour area to the harnesses
  that cover it, and records the PostgreSQL regression-suite status.

### Silent wrong answers found by SQLSTATE-exact fuzzing

- `INSERT ... VALUES` evaluates non-literal expressions as PostgreSQL does.
  It used to store the SQL text of anything it could not evaluate (`'a' ||
  NULL`, `CASE`, a bare identifier), the internal `:__null__` sentinel for
  NULL results, and `now()` for any `AT TIME ZONE`. Expressions over
  prepared-statement parameters are evaluated at Bind (they reached the
  column as text such as `$3 + 1`).
- A `SELECT` without `FROM` raises 42703 / 42P01 for column references
  instead of answering no rows or implicitly adding the table. asyncpg's
  `test_prepare_02` now passes.
- Casts with no pathway in PostgreSQL (`date::int`, `bool::numeric`, ...)
  raise 42846 at parse instead of 22P02 at run time, using the pinned
  `pg_cast.dat`.

### PostgreSQL identity and loud refusals

- The server reports PostgreSQL 17.7 (`server_version` 17.7,
  `server_version_num` 170007, `version()`), the release the semantics and
  the regression campaign are pinned to. It reported 15.0. All four
  reporting paths read one constant in `PgWireServer`.
- `CREATE TABLE … PARTITION BY` is refused with 0A000 instead of silently
  creating an unpartitioned table. The `:pg-dump` preset keeps loading the
  parent as a plain empty table (new reject-kind `:partitioned-table`).
- The pagila `pg_dump` round-trip now also compares the partition tables
  (21 tables, previously 14): the partition filter had excluded them.

### Beta-exit verification

- Added an executable 0.2.0 beta-exit ledger covering per-commit, manual-release,
  and planned gates, with evidence paths and explicit non-goals. CI validates
  that every required job exists and blocks deployment.
- The PostgreSQL regression inventory now has a non-destructive setup command
  for its pinned `REL_17_7` source. CI verifies all 222 scheduled tests remain
  classified and all strict source slices still point to executable tests.
- The default-format Pagila `pg_dump` round-trip now blocks deployment rather
  than running as a non-release check.
- Fixed the lint task's false-green exit behavior. It now fails when clj-kondo
  reports errors; three pre-existing static-field invocation errors were
  corrected so the stricter gate starts green.
- Recorded the first local beta-exit baseline, including the environment-dependent
  asyncpg divergence and four distinct pgjdbc `BatchExecuteTest` failure classes.
- Deleting a row inserted earlier in the same transaction now cancels its buffered
  insert and update writes instead of asking Datahike to retract a tempid. All four
  pgjdbc `testMixedBatch` variants now pass.
- Text-format Bind parameters now reject embedded NUL and malformed UTF-8 instead
  of accepting Java replacement characters. The error uses PostgreSQL's `22021`
  `character_not_in_repertoire` SQLSTATE, and all four pgjdbc embedded-NUL batch
  variants now pass.
- Explicit casts around prepared INSERT parameters are now deferred until Bind
  supplies a value instead of trying to cast the `ParamRef` placeholder during
  Parse. All four pgjdbc alternating-parameter-type batch variants now pass.

### Password authentication and TLS

- The pgwire server can now request and verify PostgreSQL cleartext-password
  authentication through a deployment-owned callback or a small fixed `:users`
  map. Authentication failures use PostgreSQL's `28P01` response without
  revealing whether a user exists.
- PostgreSQL `SSLRequest` can now upgrade the existing connection to native
  TLS through an injected `SSLContext` or a PKCS#12 keystore. The pre-upgrade
  reader does not buffer or consume bytes past `SSLRequest`, matching
  PostgreSQL's protection against TLS buffer-stuffing attacks.
- Non-loopback `start-server` binds now require both password authentication
  and TLS, and reject plaintext startup before requesting a password. Existing
  loopback development listeners remain compatible and unauthenticated by
  default.
- JDBC coverage exercises accepted and rejected passwords,
  `sslmode=require`, hostname/certificate verification with
  `sslmode=verify-full`, and plaintext rejection. SCRAM and PostgreSQL MD5
  authentication are not part of this change.

### Licensing

- **pg-datahike is now under the PostgreSQL License**, replacing the Eclipse
  Public License 2.0. A PostgreSQL-compatible server should not be harder to
  build on than PostgreSQL: the new terms let anyone embed, fork, or ship it
  commercially and closed-source. Contributions are accepted under a DCO
  sign-off rather than a CLA, so contributed code arrives already licensed for
  commercial use — see `CONTRIBUTING.md`. Every line in the repository is the
  work of a single copyright holder, so no consent had to be collected.

  This governs pg-datahike's own code. Distributed artifacts — notably the
  standalone uberjar — still bundle datahike and the replikativ storage stack
  under EPL-1.0, which is file-level copyleft: proprietary work on top is
  fine, and only modifications to those libraries' own files must be
  published. The new `NOTICE` file records the full third-party breakdown and
  is now packaged into both jars at `META-INF/`.

### PostgreSQL conformance (issues #18–#22)

- **An empty query now answers `EmptyQueryResponse` instead of a parse
  error (#18).** A query string holding no statement — `""`, `";"`,
  `"  ;  ;  "`, a comment-only string — is an *empty query* in PostgreSQL:
  one `I` message and nothing else, per `exec_simple_query`'s
  `if (!parsetree_list) NullCommand(dest)`. `splitStatements` correctly
  discarded every blank fragment and then handed the *raw* string back, so
  `";"` reached JSqlParser and came back `42601`. It now reports zero
  statements, which routes to the `parsed == null` path that already
  existed. Two neighbours went with it: the statement loop's blank test
  missed comment-only fragments (comment stripping yields `" "`, not
  `""`), and `Parse(";")` errored in the extended protocol where PG
  accepts it. Verified message-for-message against PostgreSQL 17.10 on
  both protocols.

- **Math functions follow PostgreSQL semantics rather than
  `java.lang.Math`'s (#22).** `sqrt(-42)` returned `NaN`; PG raises
  `2201F`. The cause was structural — SQL functions were mapped straight
  onto their Java namesakes, which differ three separate ways:
  *domain errors* (Java returns NaN/Infinity where PG raises: `sqrt` of a
  negative `2201F`, `ln(0)`/`ln(-x)` `2201E`, `power(0,-x)` and
  `power(-x, 0.5)` `2201F`, `asin`/`acos` outside `[-1,1]` `22003`,
  overflow **and underflow** `22003`); *a different function under the
  same name* — SQL `log(x)` is base-10 while `Math/log` is natural, so
  `log(100)` silently returned `4.605` instead of `2`, a wrong answer
  rather than an error; and *different tie-breaking* — `Math/round`
  rounds halves toward positive infinity, so `round(-2.5)` gave `-2`
  where PG gives `-3`. Argument counts now resolve at translate time as
  `42883` instead of leaking Clojure's `ArityException` as `XX000`.
  Adds `ln`, `log10`, `log(b, x)`, `cbrt`, `asin`, `acos`, `atan2`,
  `sinh`/`cosh`/`tanh`, `asinh`/`acosh`/`atanh`, `degrees`, `radians`,
  `cot`, `trunc`, `gcd`, `lcm`, `width_bucket`, `pi`. PG's own
  inconsistencies are mirrored deliberately: `sinh`/`cosh` overflow to
  Infinity without error while `exp` raises, `cot(0)` is Infinity,
  `atanh(±1)` is Infinity, float8 `sign(NaN)` is `0`, and
  `width_bucket`'s argument failures are `2201G`, not the `22003` the
  surrounding float code uses.

- **`CREATE`/`ALTER SEQUENCE` are parsed in full (#21).**
  `CREATE SEQUENCE … INCREMENT 20 START WITH 400` failed to parse. The
  reported token was one hole in a grammar that is a strict subset of
  PG's: `START 400` (no `WITH`), `INCREMENT -1` (signed), `AS bigint`,
  `IF NOT EXISTS`, `NO MINVALUE` and every form of `ALTER SEQUENCE` also
  failed. Two pre-parse rewrite rules existed to delete the offending
  tokens, and that only moved the problem — the option *values* were then
  recovered by regex over the re-rendered SQL
  (`increment\s+by\s+(\d+)`), which cannot see a negative increment and
  silently dropped `MINVALUE`/`MAXVALUE`/`CACHE`/`CYCLE`, so
  `CREATE SEQUENCE s MAXVALUE 10 CYCLE` reported success and produced an
  unbounded non-cycling sequence. Sequence DDL is now token-classified in
  full and never reaches JSqlParser; both rewrite rules are deleted rather
  than extended. Defaults and validation mirror `init_params` including
  its *order*, so a statement with several problems reports the one PG
  reports — covering the direction-dependent defaults (`INCREMENT -1`
  gives min type-min, max `-1`, start `-1`), the `AS`-type bounds, and
  `NO MINVALUE` meaning "recompute the default" rather than "unbounded".
  The options are now honoured at runtime: `CYCLE` wraps to `MINVALUE`
  ascending and `MAXVALUE` descending (not to `START`), and exhaustion
  without `CYCLE` raises `2200H`. `ALTER SEQUENCE` revalidates against the
  sequence's current parameters; `RESTART` moves the counter without
  changing `START`.

- **`bit` / `bit varying` are real types (#19).** `SELECT 0::bit`
  returned a text column and `pg_typeof(0::bit)` answered `text`. The
  digits were already right — the value was a bare string, so both type
  paths fell through to text. Bit values are now a wrapper carrying the
  two things a string cannot: the **width**, which is part of the value
  (PG compares bit strings by content then length, so `B'101'` is *not*
  equal to `B'10100000'` and `B'0' < B'00' < B'000'`), and the
  `bit`/`bit varying` distinction (OIDs 1560/1562, with different width
  coercion — `bit(n)` zero-pads on the right, `bit varying(n)` truncates
  but never pads). Also fixes cases where treating a bit as its text gave
  a *wrong answer*: hex input expands to four bits per digit including
  leading zeros (`X'1F'` is the 8-bit `00011111`, not 5 bits, which
  changes `length`, `octet_length` and sort position); `int → bit(n)`
  keeps the rightmost `n` bits and sign-extends on the left, so
  `(-44)::bit(12)` is `111111010100`; and `bit → int` reinterprets the
  bits rather than reading the digits as decimal, so `'101'::bit(3)::int`
  is `5`, not `101`. `pg_typeof` also now reports its own return type as
  `regtype` (2206) rather than `text` — that is the OID quoted in the
  issue as the expected bit type.

- **`TRUNCATE TABLE` (#20)** was fixed earlier on this branch and simply
  had not shipped; v0.1.58 predates it.

### CAST consolidation

- **One implementation of `CAST`.** Cast semantics were written out four
  times — the table-free literal fast path, `translate-cast` (both its
  compile-time fold and its runtime binding), `apply-sql-cast`, and the
  INSERT coercion path — each a dispatch over the same type categories
  that had drifted from the others. Which copy ran depended on the
  *shape* of the expression rather than its meaning, so one cast could
  behave three ways: `29::bit(4)` was correct, `(-44)::bit(12)` passed
  the value through untouched, and `'101'::bit(3)::int` read the digits
  as decimal. Issue #12 hit this for `'1'::boolean` and was fixed by
  patching four call sites; #19 hit it again. The value-level semantics
  now live in one namespace and the scalar paths delegate to it; callers
  keep only their own surrounding logic. Unifying exposed differences the
  copies had accumulated, resolved toward the more complete behaviour:
  `apply-sql-cast` had no `:date`, `:time` or `:numeric` branch at all
  and returned such casts unchanged. Also fixes `length()` /
  `char_length()` / `octet_length()` on a bit string, which were bare
  `count` and so reported the wrapper's field count rather than the bit
  width.

### jsonb fidelity

- **jsonb is now canonicalized on ingest, so it behaves like PostgreSQL
  `jsonb` rather than `json`.** Previously a jsonb value was stored as its
  raw input text, so `'{"a":1,"b":2}'::jsonb = '{"b":2,"a":1}'::jsonb` was
  `false` (two different strings) where Postgres returns `true`, and
  duplicate keys survived. jsonb writes now recursively sort object keys,
  strip insignificant whitespace, and collapse duplicate keys to the last —
  so `=`, `DISTINCT` and `GROUP BY` compare by structure, and the value a
  client reads back is canonical. Array element order is preserved (only
  object keys sort); the text-faithful `json` type is left untouched. The
  jsonb-ness of a column (`:pg/type`, which lives as an ident-entity fact,
  not in datahike's `:db/*` schema) is now surfaced into the INSERT coercion
  path. Numeric normalization is Jackson's, not Postgres's, so a few numeric
  edge cases (`1.00`, `1e3`) may not match PG's exact jsonb numeric form —
  structural canonicalization is exact. This is the correctness prerequisite
  for indexing jsonb; index acceleration (a GIN-like secondary index) is
  tracked separately.

### Bulk-insert performance

- **Pagila replay: 274s → 12s (23×).** Cumulative across five
  changes layered on the wire path:
  - **Deferred-CC INSERT batching** in both Simple Query (`Q`) and
    Extended Query (`Bind/Execute … Sync`) so multiple INSERTs in
    one sync group commit through a single `d/transact`. dc/with at
    append time keeps constraint errors synchronous (matches PG
    IMMEDIATE semantics); only system-level / cross-connection
    failures land deferred.
  - **Parse-sql LRU cache** + **JSqlParser AST cache** keyed on the
    SQL string so repeated SQL (pgjdbc unnamed prepared statements,
    ORM-generated select-by-id, repeated INSERT shapes) skips re-
    parsing.
  - **Lexical INSERT-VALUES templater** (`datahike.pg.sql.template`)
    rewrites `INSERT INTO t (cols) VALUES (lit, …)` to
    `(? , …)` and captures literals. The templated SQL hits the
    cache; per-row work is a typed-substitute walk (~10 µs vs
    ~1 ms full parse). Bails on ON CONFLICT, INSERT … SELECT,
    SQL with existing `?` placeholders, and any non-templatable
    token shape — slow path stays correct.
  - **`now()` / `current_timestamp` family marker-ised** like the
    existing `nextval` marker so the cached parsed map doesn't
    bake a parse-time `Date`. Resolved per-execute; identity-
    tracked so the same marker appearing in multiple parts of
    tx-data resolves once per logical use.
  - **`describeParams` infers OIDs for column-less INSERTs** by
    falling back to `pgs/column-info`'s declared column order. Fixes
    pgjdbc's `executeBatch` with positional `INSERT INTO t VALUES
    (?, ?, ?)` (was raising `Can't change resolved type for param`).
- **Throughput at 1000 rows/connection:**
  - JDBC `PreparedStatement.executeBatch`: ~5 k r/s
  - Simple-Query multi-stmt (`psql -f`, `pg_dump` replay): ~4 k r/s
  - Explicit `BEGIN; INSERT*; COMMIT`: ~1.4 k r/s
  - Single-stmt-per-call (default JDBC): ~370 r/s (bound by
    per-call commit cost in Datahike).

### Migration & pg_dump interop

- **`dump` tool + CLI** — `datahike.pg.dump/dump` walks any Datahike
  database (SQL- or Datalog-created) and emits pg_dump-shaped SQL.
  Output replays into either pg-datahike or real PostgreSQL via
  `psql`. CLI: `java -jar pg-datahike.jar dump --data-dir DIR --db
  NAME [--out FILE] [--inserts|--copy] [--schema-only|--data-only]
  [--exclude-table NAME] [--config CONFIG.edn]`. The `--config`
  escape hatch reads a full Datahike config EDN, so any konserve
  backend (file, jdbc, s3, redis, lmdb, …) is reachable; store-id
  is auto-discovered from the persisted `:db` branch.
- **Native Datahike databases dump cleanly** — without any setup, a
  database created via `d/transact` exports as valid PG SQL:
  `:db.unique/identity` → `PRIMARY KEY`, `:db.unique/value` → `UNIQUE`,
  `:db.cardinality/many T` → `T[]` with PG array literals,
  `:db.type/ref` → `bigint` (entity-id). FK constraints opt-in via
  `set-hint!` `:datahike.pg/references`.
- **pg_dump-import via psql** — `pg_dump` output replays into pg-
  datahike with the new `:compat :pg-dump` preset. Coverage:
  `CREATE TABLE` with `DEFAULT nextval('s'::regclass)` (incl. schema-
  qualified seq names), `CREATE SEQUENCE … NO MINVALUE/MAXVALUE/
  CYCLE`, multi-row `INSERT`, `COPY … FROM stdin` (text + CSV),
  `CREATE TYPE … AS ENUM`, `CREATE DOMAIN`, partitioned tables
  (parent + children), `\restrict`/`\unrestrict` psql metacommands,
  `pg_catalog.set_config(...)`. Triggers, functions, materialized
  views, ALTER OWNER, ATTACH PARTITION are silently accepted under
  `:pg-dump`.
- **`:compat :pg-dump` preset** — superset of `:permissive` that
  bundles the per-feature reject-kinds pg_dump emits and we don't
  model: `:trigger :function :procedure :aggregate :rule :operator
  :cast :language :materialized-view :attach-partition :alter-type
  :alter-domain :type` (non-ENUM CREATE TYPE forms).
- **Validated round-trip** against real PostgreSQL: Chinook (15.6 k
  rows / 11 tables / FKs / NUMERIC / TIMESTAMP) byte-identical
  per-row equality at every leg; Pagila (50 k rows / 22 tables / ENUM
  / DOMAIN / partitioning / triggers / functions) schema and data
  load end-to-end.

### First-class type system additions

- **ENUM** — `CREATE TYPE … AS ENUM (…)` bypasses JSqlParser via a
  custom parser (`datahike.pg.sql.types`) and lands as a registry
  entity (`:datahike.pg.enum/{name,values,values-ordered}`). Columns
  declared with the enum lower to `:db.type/string` + a
  `:datahike.pg/enum-of` tag so the dump re-emits the column with
  the original enum type, not `text`.
- **DOMAIN** — `CREATE DOMAIN [name] AS [base] [CHECK (…)]`. Same
  registry-entity architecture (`:datahike.pg.domain/{name,base-type,
  check-expr,not-null,…}`). Column resolution lowers to the base
  type with `:datahike.pg/domain-of` for re-emission.
- **DOMAIN / ENUM runtime enforcement** — INSERTs into a DOMAIN- or
  ENUM-typed column are validated against the registry at txdb time:
  - DOMAIN CHECK violations raise `23514` ("value for domain X violates
    check constraint Y"). PG 3VL: NULL → unknown → satisfied.
  - DOMAIN NOT NULL raises `23502` ("domain X does not allow null
    values").
  - ENUM non-members raise `22P02` ("invalid input syntax for type
    {enum}"). NULL is allowed unless the column is also NOT NULL.
  Implementation reuses the existing `:db.fn/call` wrapper layered on
  INSERT tx-data — `apply-column-constraints` already runs at txdb-
  time for NOT NULL / CHECK / FK; we add a sibling pass for domain/
  enum. CHECK ASTs are pre-parsed at cache-build time, ENUM value-
  sets frozen, both memoised per (schema, table). Tables without
  domain- or enum-typed columns pay zero overhead. Pagila replay
  (which has both `year` DOMAIN with CHECK and `mpaa_rating` ENUM)
  stays at ~4 k rows/s.
- **`consume-name` parses quoted-identifier domain names** including
  `public."bıgınt"` (Turkish dotless-i in Pagila's schema), and the
  symmetric `"schema"."name"` form. The existing rule only matched
  bare-alphanumeric `schema.name`.

### `nextval` / sequence handling

- **`DEFAULT nextval('seq')` parses** — token-driven rewrite wraps
  `DEFAULT <fn>(…)` in extra parens for `nextval`/`currval`/`lastval`
  so JSqlParser accepts the form. Identical AST to the parenthesised
  form. Fixes `pg_dump`'s SERIAL/IDENTITY emit.
- **`nextval()` in INSERT VALUES resolved** — sibling-pass
  architecture in `params.clj`: tx-data flows through
  `substitute-params` (Bind-time) and then `resolve-nextvals!`
  (Execute-time, against the live conn). PG-correct non-transactional
  semantics: nextval advances stick across rollback, concurrent
  callers get distinct values via CAS-retry. `nextval!` core
  extracted from `handle-nextval` and shared by both call sites.
- **Schema-qualified sequence names** (`public.foo_id_seq`) accepted
  by `nextval`/`currval`/`setval` and by `DEFAULT nextval`.
- **`<table>_seq` no longer false-matches** as the IDENTITY sequence
  for table `<table>`. The matcher requires a non-empty `<col>`
  between prefix and suffix.

### Other server fixes

- `splitStatements` filters whitespace-only chunks (after stripComments
  turns trailing comments into spaces); `handleParse` (extended-query
  path) applies `stripComments` before JSqlParser — both fix trailing-
  comment handling.
- `translate-create-sequence` unquotes the sequence name (was storing
  literal quotes for `CREATE SEQUENCE "x"`, breaking subsequent
  `setval`).
- `:set-config` added to `system-result-metadata` — fixes "Received
  resultset tuples, but no field structure for them" error pgjdbc
  raised on `SELECT pg_catalog.set_config(...)` from pg_dump preludes.
- `database/tokenize` recognises multi-char operators (`>=`, `<=`,
  `<>`, `!=`, `||`) and single chars `<`/`>`/`!`/`~`/`^`/`|` (was
  silently dropping them in `:else`). DOMAIN CHECK round-trips
  correctly as a result.
- `parse-timestamp-string` accepts PG's `Y/M/d` slash-date format
  (used by Chinook's employee hire-dates).
- `string-value-text` helper reproduces PG's `N'...'` (national-
  character) trailing-space trimming for Chinook fidelity.
- Token rewrite rule `partition-by-rule` strips `PARTITION BY
  <strategy> (<col>)` from CREATE TABLE so partitioned tables parse.
- Token rewrite rule `create-sequence-no-clause-rule` strips
  `NO MINVALUE/MAXVALUE/CYCLE` two-token groups.
- Dump output preserves source column declaration order via
  `pgs/column-order-from-db`; composite-PK tuple attrs no longer
  emitted as phantom columns.

### Earlier in this branch (pre-Pagila work)

- Renamed from `pgwire-datahike` to `pg-datahike` — the project is a
  PostgreSQL adapter for Datahike, not just a wire-protocol server.
  Namespaces (`datahike.pg.*`) and the `PgWireServer` Java class are
  unchanged. Clojars coord: `org.replikativ/pg-datahike`.
- Extended Query RowDescription for system queries — pgjdbc's default
  `preferQueryMode=extended` now works for `current_database()`,
  `now()`, `version()`, advisory locks, `nextval`, etc.
- FK-via-ref JOIN rewrite: `JOIN c ON p.fk = c.pk` resolves correctly
  on native Datahike schemas where refs store target entity-ids.
- `:datahike.pg/*` schema hints (column rename, hidden attr, FK target,
  table rename) — let users customize the SQL view of a native
  Datahike database without DDL.
- Multi-DB registry: `start-server` accepts `{name → conn}`; clients
  route via the JDBC URL's database name, virtual `pg_database`
  catalog enumerates the registry, unknown names get 3D000.
- Initial extraction from datahike's `pg-server/` subtree.
- Public API facade `datahike.pg` (start-server, stop-server,
  make-query-handler, register-catalog-table!, unregister-catalog-table!,
  reset-lock-registry!, reset-advisory-locks!).
- Token-driven SQL classification (`datahike.pg.sql.classify`) — routes
  statements to the right handler before JSqlParser sees them.
- Structural SELECT shape matcher (`datahike.pg.sql.shape`) — identifies
  pgjdbc/Odoo catalog probes without substring matching.
- Token-driven source rewriter (`datahike.pg.sql.rewrite`) — inline
  REFERENCES stripping, CREATE INDEX anonymous-name injection,
  SELECT-FROM empty-projection injection.
- Constraint enforcement (NOT NULL, DEFAULT, CHECK, FK child-side +
  parent-side RESTRICT on DELETE and key-UPDATE).
- Extension seam for virtual catalog tables (`register-catalog-table!`).
- `:compat :permissive` / `:silently-accept` handler options for
  tolerating ORM-emitted no-op DDL (GRANT, REVOKE, POLICY, RLS,
  CREATE EXTENSION).
- Advisory-lock support (pg_advisory_lock, pg_try_advisory_lock,
  pg_advisory_xact_lock, pg_advisory_unlock, pg_advisory_unlock_all)
  with proper per-session / per-tx lifecycles — needed for every
  serious migration tool (Flyway, Alembic, Ecto, Rails, Liquibase).
- Session introspection (pg_backend_pid, txid_current, pg_sleep).
- SAVEPOINT / RELEASE / ROLLBACK TO with correct PG error codes
  (25P01 outside-tx, 3B001 missing-savepoint).
- Maintenance no-ops (VACUUM, REINDEX, CLUSTER, CREATE SCHEMA).
- `pg_extension` as an always-empty virtual table for framework feature
  probes.

### Integration

- pgjdbc ResultSetTest: 80/80 passing.
- Unit test suite: 703 tests / 1952 assertions.
- Real-PG round-trip: Chinook end-to-end with byte-identical per-row
  equality; Pagila schema + data load.
