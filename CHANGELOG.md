# Changelog

All notable changes to pg-datahike.

## [Unreleased]

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
