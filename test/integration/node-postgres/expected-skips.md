# node-postgres - expected skips and known gaps

node-postgres (`pg`) is the canonical JavaScript PostgreSQL client. Its
implementation stack is completely different from asyncpg's, so it exercises
different corners of our pgwire server.

`run.sh` is file-granularity: each entry is one `*-tests.js` file that
upstream runs as a separate `node file.js` invocation. A file passes iff
`node <file>` exits 0. The focus list is split into two tiers:

- **`FILES`** — the must-pass set. A failure here fails the job.
- **`XFAIL_FILES`** — known-gap files (see the table at the bottom). Each
  fails on a specific, documented missing feature, so it is run but a
  failure counts as *skipped*, not failed. If a fix flips one green it is
  reported as **XPASS** and should be promoted into `FILES`.

## Included — must-pass (`FILES` in `run.sh`)

| File | Rationale |
|---|---|
| `test/integration/client/big-simple-query-tests.js` | Large result sets, streaming chunking. |
| `test/integration/client/simple-query-tests.js` | Simple/multi-query execution, including isolated same-named temp tables on concurrent clients. |
| `test/integration/client/empty-query-tests.js` | `EmptyQueryResponse` handling. |
| `test/integration/client/prepared-statement-tests.js` | Extended-query protocol end-to-end. |
| `test/integration/client/multiple-results-tests.js` | `;`-separated statements in one Query. |
| `test/integration/client/no-data-tests.js` | `NoData` response to Describe on DDL. |
| `test/integration/client/no-row-result-tests.js` | `CommandComplete` with zero rows. |
| `test/integration/client/row-description-on-results-tests.js` | Shape of `RowDescription` fields passed to callback. |
| `test/integration/client/result-metadata-tests.js` | `result.fields`, `result.rowCount`, `result.command`. |
| `test/integration/client/results-as-array-tests.js` | `rowMode: 'array'` opt. |
| `test/integration/client/query-column-names-tests.js` | Column alias propagation through descriptor. |
| `test/integration/client/promise-api-tests.js` | Promise-returning `query()`. |
| `test/integration/client/query-as-promise-tests.js` | `new Query(...)` + `.promise()`. |
| `test/integration/client/api-tests.js` | Callback API behavior, including startup rejection for an unknown database. |
| `test/integration/client/transaction-tests.js` | `BEGIN` / `COMMIT` / `ROLLBACK`. |
| `test/integration/client/json-type-parsing-tests.js` | `json` / `jsonb` parsing. |

## Excluded (deliberately not in the focus list)

| File / pattern | Why excluded |
|---|---|
| `test/unit/**/*` | Pure unit tests with no DB; not our concern. |
| `test/cloudflare/**` | Cloudflare Workers runtime; requires `wrangler` + vitest. |
| `test/native/**` | `pg-native` bindings (libpq). Compile-time dep; not our path. |
| `test/integration/connection-pool/tls-tests.js` | Core TLS is covered separately; this upstream fixture expects its own certificate/server matrix. |
| `test/integration/connection-pool/**` | Pool tests are mostly timing/LISTEN based. Can be added later if stable. |
| `test/integration/client/ssl-tests.js` | Core TLS is covered separately; this upstream fixture expects its own certificate/server matrix. |
| `test/integration/client/sasl-scram-tests.js` | SCRAM auth not implemented. |
| `test/integration/client/notice-tests.js` | `NoticeResponse` channel not populated. |
| `test/integration/client/connection-timeout-tests.js` | Timing-sensitive; flaky locally. |
| `test/integration/client/connection-parameter-tests.js` | Relies on specific server-side `search_path` / `client_min_messages` behavior. |
| `test/integration/client/configuration-tests.js` | Exercises `SET` / `SHOW` variants we do not fully implement. |
| `test/integration/client/appname-tests.js` | `application_name` round-trip via `pg_stat_activity`. |
| `test/integration/client/async-stack-trace-tests.js` | Stack-trace hygiene, unrelated to server. |
| `test/integration/client/custom-types-tests.js` | Requires user-defined types via `CREATE TYPE`. |
| `test/integration/client/timezone-tests.js` | Session `timezone` GUC. |
| `test/integration/client/statement_timeout-tests.js` | Requires `statement_timeout` GUC. |
| `test/integration/client/idle_in_transaction_session_timeout-tests.js` | Requires `idle_in_transaction_session_timeout` GUC. |
| `test/integration/client/quick-disconnect-tests.js` | Timing-sensitive socket half-close. |
| `test/integration/client/network-partition-tests.js` | Uses OS-level packet loss simulation. |
| `test/integration/client/huge-numeric-tests.js` | Requires `numeric` type with precision/scale - partial support only. |
| `test/integration/client/array-tests.js` | PG array codec; partial support only. |
| `test/integration/client/parse-int-4-tests.js` (if present) | Covered adequately by `parse-int-8-tests.js`. |
| `test/integration/client/domain-tests.js` | `CREATE DOMAIN` not supported. |
| `test/integration/client/gh-issues/**` | Ad-hoc regression tests for upstream bugs; not protocol conformance. |

## Known-gap files (`XFAIL_FILES` in `run.sh`)

Each of these fails on a specific missing feature, NOT a regression. They
run on every CI pass; a failure is reported as `xfail` (counted as
skipped) and a fix that flips one green is reported as `XPASS` — when that
happens, move the file up into `FILES`.

| File | Failing test(s) | Missing feature |
|---|---|---|
| `error-handling-tests.js` | `non-query error with callback` | The harness connects with an unknown user but this unauthenticated local server accepts it, so the callback receives no `Error`. Temp-table isolation cases in this file now pass. |
| `field-name-escape-tests.js` | quoted-identifier escaping | Backslash / exotic quoted-identifier rules differ from PG (JSqlParser identifier handling). Adversarial; low realistic-use value. |
| `query-error-handling-tests.js` | "client can do nothing on cancellation" | Query cancellation via `pg_cancel_backend()` over `pg_stat_activity` not implemented. |
| `query-error-handling-prepared-statement-tests.js` | backend-terminate path | `pg_terminate_backend()` over `pg_stat_activity` not implemented. |
| `type-coercion-tests.js` | "date range extremes" | PostgreSQL and ECMAScript accept timestamps up to year 275760 and BCE dates far outside `java.time`'s practical conversion path. The 13 core coercion cases, timestamptz round-trip, and "selecting nulls" (`7 <> $1`, `$1=NULL`) pass. |
| `parse-int-8-tests.js` | `SELECT COUNT(*), '{1,2,3}'::bigint[] FROM asdf` | `COUNT(*)` over an empty table must return one row (count 0); plus the `'{1,2,3}'::bigint[]` array-literal cast. |

Priority for closing these: the array-literal cast, then narrower authentication
and query-error cases. The latter require SQL helpers around
`pg_stat_activity`; `field-name-escape` is adversarial and lowest priority.
