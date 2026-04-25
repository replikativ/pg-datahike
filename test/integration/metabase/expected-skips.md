# Metabase — what the harness covers and what it skips

The harness drives a real Metabase instance against pg-datahike and
asserts that the catalog-sync code path works end-to-end. It is the
truth gate for "Metabase works on pg-datahike 0.1".

## What's exercised

When Metabase first connects to a Postgres datasource, it fans out
into many introspection probes. A passing run.sh proves that all of
the following resolve correctly:

| Code path                                       | Probed via |
|-------------------------------------------------|------------|
| `current_setting('TimeZone')` and friends       | connection bringup |
| `CURRENT_USER` / `SESSION_USER`                 | session info |
| `pg_namespace` / `pg_class` / `pg_attribute`    | listing schemas + tables + columns |
| `format_type(atttypid, atttypmod)`              | per-column type display |
| `pg_get_indexdef`, `pg_get_constraintdef`       | constraint discovery |
| `pg_index`, `pg_constraint`                     | PK / UNIQUE / FK rendering |
| Regex `~` / `!~` in privilege filter            | `WHERE schemaname !~ '^pg_'` |
| `current_schemas(true)[1]` array subscript      | getSchemas() idiom |
| Native SQL execution                            | `SELECT count(*) FROM customer` |

The fixture (`seed.clj`) deliberately uses a schema that hits all
four constraint kinds (PK, UNIQUE, FK, CHECK) plus three column
types (long, string, instant) so the column-type rendering covers
the OID lookups Metabase performs.

## What's skipped (out of scope for 0.1)

Metabase is a much larger product than its data-source integration.
We don't drive any of:

| Feature                          | Why skipped |
|----------------------------------|-------------|
| Saved questions / dashboards     | Stored in Metabase's internal DB; doesn't touch pg-datahike. |
| Alerts / pulses / SMTP           | Notifications path; orthogonal to SQL translation. |
| Geospatial visualisations        | We don't implement PostGIS. |
| `MBQL → SQL` query builder       | Generates the same SQL shapes the native path uses; covered indirectly. |
| Sample database (H2-bundled)     | Not a pg-datahike datasource. |
| User management UI               | Single admin created in setup. |

## Re-running

The harness is one-shot: `./run.sh` boots a fresh Metabase + a fresh
pgwire from `seed.clj`, runs the assertions, and tears both down.
There is no per-commit CI integration yet — this gate is run
manually before publishing a release.

If `./run.sh` exits non-zero, `last-run.log` has stdout+stderr from
both processes; tail it to see Metabase's stack trace and the
pgwire-side query log.
