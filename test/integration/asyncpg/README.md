# asyncpg wire-protocol regression tests against datahike pgwire

This directory drives a curated subset of the upstream
[asyncpg](https://github.com/MagicStack/asyncpg) test suite against the
Datahike pgwire server.

asyncpg is the cleanest low-level wire-protocol regression target available:
it is a pure-Python protocol implementation with no libpq dependency, so its
tests exercise the actual bytes on the wire rather than libpq's compatibility
layer. When asyncpg's `test_prepare.py` fails, our Parse/Bind/Describe path
has a bug, full stop.

## Layout

```
asyncpg/
  setup.sh           # clone upstream, create venv, pip install (idempotent)
  run.sh             # run the curated module list, produce a summary
  expected-skips.md  # what we cover, what we skip, and why
  README.md          # this file
  asyncpg/           # (gitignored) cloned upstream source tree
  .venv/             # (gitignored) Python virtualenv with asyncpg + pytest
  last-run.log       # (gitignored) full pytest output from the most recent run
```

## Prerequisites

- Python >= 3.9 on `PATH` (3.12 recommended).
- A C compiler (`cc` / `gcc`) - asyncpg ships Cython extensions that are
  compiled during `pip install -e .`.
- `git`.
- Internet access the first time `setup.sh` runs.
- A running Datahike pgwire server on `localhost:15432`.

`setup.sh` pins asyncpg to `v0.30.0`.

## Running

```
./setup.sh     # idempotent: clone + venv + compile C extensions
./run.sh       # run focused modules, ~1-3 min
```

`run.sh` exits 0 when every non-skipped test passes. Summary line:

```
SUMMARY: 187 passed, 0 failed, 3 skipped   (pytest rc=0)
```

`last-run.log` has the full pytest output.

## What is covered

See `expected-skips.md`. In short: extended-query protocol
(Parse/Bind/Describe/Execute/Sync), connection startup, prepared statement
caching, cursor/portal management, transactions, error mapping, per-type
codecs. Explicitly excluded: COPY, LISTEN/NOTIFY, pool-with-cancel,
logical-replication adjacent tests.

## Environment variables used

`run.sh` exports these before invoking pytest. asyncpg's testbase detects
`PGHOST` and switches to "use the already-running cluster" mode rather than
trying to spawn `initdb`.

| Var | Value |
|---|---|
| `PGHOST` | `127.0.0.1` |
| `PGPORT` | `15432` |
| `PGUSER` | `datahike` |
| `PGDATABASE` | `datahike` |
| `PGPASSWORD` | `datahike` |
| `PGSSLMODE` | `disable` |

## Interpreting output

- `N passed, 0 failed, M skipped`: clean.
- `N passed, K failed, M skipped`, `K > 0`:
  - Cross-check each failure against the "Expected failures" section of
    `expected-skips.md`. Known gaps there are not regressions.
  - Otherwise treat as a regression; `last-run.log` has the full stack.
- pytest rc != 0 with zero failures reported: likely a collection/import
  error in asyncpg itself. Look for `ERROR` lines early in the log.

## Timing

Single focused run is ~1-3 minutes. The full upstream suite takes ~10 minutes
and exercises features we do not implement - don't run it.
