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

### Baseline ledger — 2026-06 (focus list, `test_cursor` runs but is fragile)

Per-module pass/fail at the time of writing (server at commit 5254806):

| Module | pass | fail | clean? |
|---|---|---|---|
| test_record | 25 | 0 | ✅ |
| test_transaction | 6 | 0 | ✅ |
| test_types | 1 | 0 | ✅ (rest skipped) |
| test_connect | 18 | 4 | mostly |
| test_execute | 15 | 5 | mostly |
| test_prepare | 20 | 15 | mixed |
| test_exceptions | 1 | 2 | |
| test_introspection | 0 | 7 | setup-blocked |
| test_cache_invalidation | 0 | 9 | setup-blocked |
| test_codecs | 3 | 34 | setup-blocked + type gaps |

The ~76 failures cluster by ROOT CAUSE (most are `setUp` cascades from a few
parser gaps, so the test count overstates the work). Classification:

**`gap:ddl-parser` (~23) — fixable, highest leverage.** jsqlparser rejects DDL
the test fixtures use, so the whole test class errors in `setUp`:
- `CREATE TYPE … AS ENUM (…)` / `AS RANGE (…)`  → token "TYPE" (~15)
- `CREATE DOMAIN …`                              → token "DOMAIN" (~5)
- `ALTER … SET DATA TYPE …`                      → token "DATA" (~3)
Fixing these in the DDL pre-parser unblocks classes across test_codecs /
test_introspection / test_cache_invalidation (and metabase/odoo/sqlalchemy).

**`bug:*` (~16) — real correctness fixes:**
- `bug:wire-trailing` (4) — `BufferError: unexpected trailing N bytes`: we emit
  a malformed wire message for some type (introspection + custom-codec paths).
  MOST URGENT (silent corruption risk). test_introspection::no_stmt_cache_01-03,
  test_codecs::custom_codec_override_tuple.
- `bug:decode-mismatch` (6) — value decodes wrong (binary vs expected), incl.
  jsonb (`\x01…{"foo":1}` vs `{"foo":1}`). test_codecs::standard_codecs,
  test_table_as_composite, test_prepare_03, …
- `bug:codec-input` (5) — param input mismatch (json/array). test_range_types,
  test_multirange_types, test_array_with_custom_json_text_codec, …
- `bug:array-bounds` (1) — multi-dim array with explicit `[lo:hi]` bounds:
  `[1:3][-1:0]={{1,2},…}` not parsed. test_codecs::test_arrays.

**`gap:type-coverage` (Tier B) — additive:** interval, void, aclitem[] (relacl),
citext/hstore (also need the type to exist), some numeric edges.

**`oos:*` (~13) — out-of-scope (Tier C / not-a-surrogate-goal), permanent skips:**
- `CREATE EXTENSION` (5), `CREATE FUNCTION`/plpgsql (2), `DO` blocks (2)
- separate database (`CREATE DATABASE` / connect to `postgres`/`testdb`) (2)
- multi-host / standby connection routing (test_connect::prefer_standby*,
  target_server_attribute*)
- statement_timeout / cancellation / concurrent-call tests
  (test_execute::executemany_timeout*, test_prepare::*_interrupted_close,
  *_concurrent_calls) — partial cancel protocol, Tier C
- `flaky:timing` (1) — asyncpg testbase asserts a block runs < 0.25s; our
  engine is slower. Environment, not a protocol issue.

NOTE: `bug:other` in the raw triage mixes the oos timeout/concurrency/standby
tests above with a few genuine bugs; re-triage each as we touch its module.
The per-test list is regenerated by /tmp build_ledger scripts; promote to a
checked-in manifest + harness diff once the big DDL cluster is cleared.

WORKLIST (priority order): 1) DDL parser cluster (CREATE TYPE ENUM/RANGE,
CREATE DOMAIN, ALTER SET DATA TYPE). 2) bug:wire-trailing. 3) bug:decode /
bug:codec-input (jsonb, range/multirange, arrays). 4) bug:array-bounds.
5) Tier-B type tail. 6) Tier-A exec model per doc/design-alignment.md.
7) mark oos:* as permanent documented skips.

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
