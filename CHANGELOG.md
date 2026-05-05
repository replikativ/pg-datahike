# Changelog

All notable changes to pgwire-datahike.

## [Unreleased]

### Added
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
  CREATE EXTENSION, COPY).
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
- Unit test suite: 170 tests / 779 assertions.
