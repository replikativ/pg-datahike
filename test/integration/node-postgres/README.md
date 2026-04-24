# node-postgres (`pg`) integration tests against datahike pgwire

This directory drives a curated subset of the upstream
[node-postgres](https://github.com/brianc/node-postgres) `pg` package's
integration tests against the Datahike pgwire server.

node-postgres is the canonical JS PostgreSQL client. Its protocol
implementation is an independent codebase from libpq and from asyncpg, so it
catches a third slice of bugs: we've seen cases where pgjdbc and asyncpg both
pass and node-pg fails on the same underlying server issue because of
differences in how they drive the extended-query protocol.

## Layout

```
node-postgres/
  setup.sh           # clone upstream + install deps + build TS (idempotent)
  run.sh             # run the curated focus list, produce a summary
  expected-skips.md  # what we cover, what we skip, and why
  README.md          # this file
  node-postgres/     # (gitignored) cloned upstream monorepo
  last-run.log       # (gitignored) full test output from the most recent run
```

## Prerequisites

- Node >= 18 on `PATH` (the upstream `test-worker` target gates on this; so
  do we for consistency).
- `npm` (or `yarn` - setup.sh prefers yarn if available).
- `git`.
- Internet access the first time `setup.sh` runs.
- A running Datahike pgwire server on `localhost:15432`.

`setup.sh` pins the upstream to `pg@8.14.1`.

## Running

```
./setup.sh     # idempotent: clone + npm install + tsc build, ~1-2 min first time
./run.sh       # run focused files, ~30-120 s
```

`run.sh` exits 0 when every non-skipped file passes. Summary line:

```
SUMMARY: 22 passed, 0 failed, 0 skipped
```

`last-run.log` has the full output.

## Environment variables used

`run.sh` exports these. node-postgres' `test/test-helper.js` reads all of the
standard `PG*` env vars.

| Var | Value |
|---|---|
| `PGHOST` | `127.0.0.1` |
| `PGPORT` | `15432` |
| `PGUSER` | `datahike` |
| `PGDATABASE` | `datahike` |
| `PGPASSWORD` | `datahike` |
| `PGTESTNOSSL` | `1` (upstream flag to skip SSL tests) |

## Interpreting output

- `N passed, 0 failed, 0 skipped`: clean run.
- One or more files fail:
  - `run.sh` prints the list of failing files with their Node exit codes.
  - Cross-check each against `expected-skips.md` "Expected failures".
  - Dig into `last-run.log` - each file's output is preceded by `---- <path>
    ----` so you can grep for it.

## Timing

A focused run is ~30-120 seconds: each test file is a fresh `node` process
and spends most of its time on startup/shutdown. The full upstream suite
(including unit + native + worker) takes 3-5 minutes and tests things
unrelated to our server (libpq bindings, Cloudflare Workers).

## Notes on the test harness quirks

- node-postgres' upstream `Makefile` runs tests via
  `find test/integration -name "*-tests.js" | xargs -n 1 -I file node file`.
  Files are run sequentially; each sets up and tears down its own fixtures.
- Exit code 255 (the "uncaught exception" path in `test-helper.js`) and
  anything else non-zero both count as "failed".
- There's no built-in `--test-filter`; to run a single file just call
  `node test/integration/client/<file> --` from inside the cloned tree.
