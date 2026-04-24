# node-postgres - expected skips and known gaps

node-postgres (`pg`) is the canonical JavaScript PostgreSQL client. Its
implementation stack is completely different from asyncpg's, so it exercises
different corners of our pgwire server.

`run.sh` is file-granularity: each entry in its focus list is one
`*-tests.js` file that upstream runs as a separate `node file.js` invocation.
A file passes iff `node <file>` exits 0.

## Included (the focus list in `run.sh`)

| File | Rationale |
|---|---|
| `test/integration/client/simple-query-tests.js` | Single-statement `Q` / `RowDescription` / `DataRow` path. |
| `test/integration/client/big-simple-query-tests.js` | Large result sets, streaming chunking. |
| `test/integration/client/empty-query-tests.js` | `EmptyQueryResponse` handling. |
| `test/integration/client/prepared-statement-tests.js` | Extended-query protocol end-to-end. |
| `test/integration/client/multiple-results-tests.js` | `;`-separated statements in one Query. |
| `test/integration/client/no-data-tests.js` | `NoData` response to Describe on DDL. |
| `test/integration/client/no-row-result-tests.js` | `CommandComplete` with zero rows. |
| `test/integration/client/row-description-on-results-tests.js` | Shape of `RowDescription` fields passed to callback. |
| `test/integration/client/result-metadata-tests.js` | `result.fields`, `result.rowCount`, `result.command`. |
| `test/integration/client/results-as-array-tests.js` | `rowMode: 'array'` opt. |
| `test/integration/client/query-column-names-tests.js` | Column alias propagation through descriptor. |
| `test/integration/client/field-name-escape-tests.js` | Quoted identifiers, case preservation. |
| `test/integration/client/error-handling-tests.js` | `ErrorResponse` -> Error mapping. |
| `test/integration/client/query-error-handling-tests.js` | Mid-query error recovery via Sync. |
| `test/integration/client/query-error-handling-prepared-statement-tests.js` | Error during Bind / Execute of a prepared stmt. |
| `test/integration/client/api-tests.js` | `client.query(text, values, cb)` overloads. |
| `test/integration/client/promise-api-tests.js` | Promise-returning `query()`. |
| `test/integration/client/query-as-promise-tests.js` | `new Query(...)` + `.promise()`. |
| `test/integration/client/transaction-tests.js` | `BEGIN` / `COMMIT` / `ROLLBACK`. |
| `test/integration/client/type-coercion-tests.js` | Default text-codec round-trip for core types. |
| `test/integration/client/parse-int-8-tests.js` | `int8` (bigint) parsing option. |
| `test/integration/client/json-type-parsing-tests.js` | `json` / `jsonb` parsing. |

## Excluded (deliberately not in the focus list)

| File / pattern | Why excluded |
|---|---|
| `test/unit/**/*` | Pure unit tests with no DB; not our concern. |
| `test/cloudflare/**` | Cloudflare Workers runtime; requires `wrangler` + vitest. |
| `test/native/**` | `pg-native` bindings (libpq). Compile-time dep; not our path. |
| `test/integration/connection-pool/tls-tests.js` | TLS not implemented. |
| `test/integration/connection-pool/**` | Pool tests are mostly timing/LISTEN based. Can be added later if stable. |
| `test/integration/client/ssl-tests.js` | TLS not implemented. |
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

## Expected failures within the focus list (known gaps, NOT regressions)

Populate after the first successful run. Each entry should state:

- file + line (or test name string from the `test(...)` call)
- one-line reason
- tracking issue in the datahike repo

Example:

```
prepared-statement-tests.js: "errors when a named prepared statement is re-parsed" - we allow silent re-parse. [#NNN]
type-coercion-tests.js: "oid parses to number" - we return oid as string. [#NNN]
```
