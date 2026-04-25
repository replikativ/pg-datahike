# Metabase ↔ pg-datahike conformance harness

End-to-end test that boots a real Metabase instance, points it at
pg-datahike, and asserts that database sync + a native query both
succeed. This is the highest-fidelity proof that the catalog
implementation works for Metabase, complementing the unit-level
shape tests under `test/datahike/`.

## Layout

```
metabase/
  setup.sh           # downloads metabase.jar (~600MB; gitignored)
  run.sh             # boots pgwire + Metabase, drives the API, asserts
  seed.clj           # populates a fixture DB and starts pgwire on :15432
  expected-skips.md  # what's covered, what's not
  README.md          # this file
  metabase.jar       # (gitignored) downloaded JAR
  data/              # (gitignored) Metabase's H2 metadata DB, fresh per run
  last-run.log       # (gitignored) combined Metabase + pgwire stdout
```

## Prerequisites

- `java` >= 21 on `PATH` (or set `METABASE_JDK_HOME`). Older JDKs
  hit Metabase's runtime checks; newer ones (24+) are fine.
- `clojure` (deps tools.deps) so `seed.clj` can boot pgwire.
- `curl`, `jq`.
- ~700 MB of disk for the JAR and Metabase's H2 cache.
- Ports 3000 (Metabase) and 15432 (pgwire) free. Override via
  `MB_PORT` / `PGWIRE_PORT` env vars.

The harness builds its own pgwire process; **do not pre-start one**.
A second pgwire on the same port will fail to bind.

## Running

```
./setup.sh   # one-shot: downloads metabase.jar
./run.sh     # ~3 minutes wall-clock — most of that is Metabase JIT
```

`run.sh` exits 0 on success. Failures dump the tail of `last-run.log`
to stderr.

## What the harness asserts

A single passing run.sh proves that:

1. The pgwire server accepts a JDBC connection from Metabase's
   PostgreSQL driver.
2. Metabase's initial sync completes (`initial_sync_status: complete`).
3. Both fixture tables (`customer`, `order`) appear with at least
   their expected fields.
4. A native `SELECT count(*) FROM customer` round-trips and returns 3.

Step 2 is the load-bearing one: it walks every catalog probe Metabase
performs — pg_class, pg_attribute, pg_index, pg_constraint, format_type,
pg_get_indexdef, pg_get_constraintdef, current_setting, the regex
privilege filter — and any failure surfaces as a sync error.

## Re-running after pg-datahike changes

The JAR is cached, so subsequent `./run.sh` invocations are JVM-startup
bound (~30s of Metabase boot + a few seconds of sync + a few hundred
ms of native query). Logs accumulate in `last-run.log`.

If `seed.clj` or pg-datahike sources change, `./run.sh` picks them up
automatically (it invokes `clojure -M:server` from the repo root).

## Adding a new assertion

`run.sh` is a single bash script — drop your assertion in next to the
existing ones. If you need a new fixture column or table, edit
`seed.clj`. Keep both files small; this is a smoke test, not a
conformance suite.
