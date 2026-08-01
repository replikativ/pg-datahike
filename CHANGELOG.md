# Changelog

All notable changes to pg-datahike.

## [Unreleased]

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
