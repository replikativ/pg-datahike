# asyncpg - expected skips and known gaps

asyncpg is a pure-Python PostgreSQL protocol implementation (no libpq). That
makes it a high-signal regression target for our pgwire server: there is no
"libpq compatibility shim" between the test code and the bytes on the wire.

`run.sh` runs a curated module list instead of the whole `tests/` tree.
Anything outside the "Included" list is deliberately excluded; anything
failing inside that list is either a known gap (listed below) or a
regression.

## Included (the focus list in `run.sh`)

| Module | Rationale |
|---|---|
| `tests/test_connect.py` | Startup, auth (cleartext), connection params, cancellation of an in-flight `.connect()`. |
| `tests/test_prepare.py` | Extended-query protocol: Parse / Bind / Describe / Execute / Sync ordering, named statements, param types. |
| `tests/test_cache_invalidation.py` | Statement cache invalidation when schemas change mid-session. |
| `tests/test_cursor.py` | `DECLARE CURSOR` + `FETCH`, async iterator semantics, portal management. |
| `tests/test_execute.py` | `executemany`, `execute` vs `fetch` vs `fetchrow`, returning-rows behavior. |
| `tests/test_exceptions.py` | Server-side error -> client-side exception mapping (SQLSTATE, ErrorResponse fields). |
| `tests/test_introspection.py` | `pg_type` / `pg_class` / `pg_attribute` reflection for codec setup. |
| `tests/test_record.py` | `Record` object semantics, field access by name/index. |
| `tests/test_transaction.py` | `BEGIN/COMMIT/ROLLBACK`, savepoints, nested transaction blocks. |
| `tests/test_types.py` | Per-type I/O (int, text, bool, uuid, bytea, jsonb, timestamp, etc.). |
| `tests/test_codecs.py` | Custom codec registration, binary vs text wire format per type. |

## Excluded (deliberately not in the focus list)

These exercise features our pgwire server does not implement. Including them
would be pure noise.

| Module | Why excluded |
|---|---|
| `tests/test_copy.py` | COPY IN/OUT protocol not implemented. |
| `tests/test_listeners.py` | LISTEN/NOTIFY not implemented. |
| `tests/test_pool.py` | Connection pool tests rely on LISTEN-based cancel. |
| `tests/test_logging.py` | `LoggingMessage` streaming from PG depends on NoticeResponse patterns we don't emit. |
| `tests/test_timeout.py` | Server-driven timeouts (`statement_timeout` etc.) not configurable. |
| `tests/test_cancellation.py` | Full cancel-request key protocol - our implementation is partial. |
| `tests/test_adversity.py` | Fuzzing proxy + chaos testing; out of scope for a smoke test. |
| `tests/test_subinterpreters.py` | CPython subinterpreter machinery; unrelated to server. |
| `tests/test__environment.py`, `tests/test__sourcecode.py` | Upstream CI-only checks (version matching, lint). |
| `tests/test_test.py` | Meta-tests of asyncpg's own testbase. |

## Expected failures within the focus list (known gaps, NOT regressions)

Populate after the first successful run. Suggested format:

```
test_prepare.py::TestPrepare::test_prepare_30_pgbouncer_safe - DEALLOCATE ALL unsupported. [#NNN]
test_types.py::TestTypes::test_interval_binary - binary interval codec absent. [#NNN]
```

## Known caveats of this harness

- asyncpg's testbase calls `cluster.get_pg_version()` on startup. When `PGHOST`
  is set, it uses `RunningCluster` which derives the version from the first
  connection's `server_version` `ParameterStatus`. Our server needs to emit a
  sensible value there (the startup code path already does: we report `17.0`).
- Many tests `CREATE TABLE test_xyz` / `DROP TABLE` at module scope. Our
  in-memory DB resets when the pgwire JVM is restarted, but within a single
  run we rely on correct `DROP TABLE IF EXISTS` semantics.
- Some tests use `asyncio.wait_for(..., timeout=1)` to catch hangs. Our
  server is usually fast enough, but a busy laptop may hit these. Re-run
  before filing a regression.
