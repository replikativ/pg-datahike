# pgjdbc — expected skips and deferred classes

This file is the contract for what `run.sh` is and is not exercising. If
you see a failure outside the lists below, treat it as a regression
signal.

## Included in per-commit CI

The verified-stable surface:

| Class | Rationale |
|---|---|
| `org.postgresql.test.jdbc2.ResultSetTest` | 80/80 pass. Exercises cursor behavior, metadata, `wasNull`, type coercions on getters/setters, updatable-ResultSet flags. Binary + text modes both green. |
| `org.postgresql.test.jdbc2.BatchExecuteTest` | 132/132 pass as of 2026-09-04. Exercises statement and prepared batches across regular/forced binary transfer and rewritten/non-rewritten inserts, including generated keys, mixed NULLs, error rollback and update counts. |
| `org.postgresql.test.jdbc2.DriverTest` | 13/13 pass. Driver registration, URL parsing and property handling. |
| `org.postgresql.test.jdbc2.ConnectionTest` | 15/15 pass. Connection state, read-only transactions, warnings and close behavior. |
| `org.postgresql.test.jdbc2.MiscTest` | 4 pass, 1 upstream skip. Basic JDBC escape and utility behavior. |
| `org.postgresql.test.jdbc2.ServerPreparedStmtTest` | 13/13 pass. Server-prepared statement lifecycle and reuse. |
| `org.postgresql.test.jdbc2.DatabaseMetaDataPropertiesTest` | 13/13 pass. Driver and database metadata properties, including the `max_index_keys` catalog probe. |
| `org.postgresql.test.jdbc42.PreparedStatementTest` | 5/5 pass. JDBC 4.2 temporal parameter behavior. |

Together these eight classes contain 276 cases: 275 pass and one is skipped by
the upstream suite.

## Rerun and deferred

These exist in the pgjdbc test suite and pg-datahike routes most of
their traffic successfully, but each class has at least one known gap
that blocks it from joining the per-commit CI list:

| Class | Why deferred |
|---|---|
| `jdbc2.StatementTest` | 0 pass, 42 fail. Its shared fixture requires `CREATE FUNCTION`; the class also covers set-returning functions, multi-statement execution, formatting functions and cancellation timing outside the admitted surface. |
| `jdbc2.PreparedStatementTest` | 69 pass, 21 fail, 4 skip. Remaining gaps include dollar-quoted and multi-statement SQL, `pg_prepared_statements`, geometric operators, extended numeric codecs, temporal coercion and stricter type errors. Some paths still expose internal exceptions and remain defect candidates. |
| `jdbc2.ResultSetMetaDataTest` | 0/60 because shared setup requires generated identity columns, which are explicitly unsupported. |
| `jdbc2.GetXXXTest` | 0/2 because its user-defined-type fixture is outside the core type surface and uses SQL the current parser rejects. |
| `jdbc2.DatabaseMetaDataTest` | Shared setup requires stored functions, custom composite types, comments and mutable PostgreSQL catalogs. The run was stopped after the common `CREATE FUNCTION` failure was established. |
| `jdbc3.DatabaseMetaDataTest` | 1 pass, 1 fail; domain-column metadata is incomplete. |
| `jdbc4.DatabaseMetaDataTest` | 0/24 because shared setup requires stored functions and procedures. |
| `jdbc42.DatabaseMetaDataTest` | 6 pass, 4 fail; enum-array, numeric-scale and missing-OID metadata differ. |
| `jdbc2.DatabaseMetaDataTransactionIsolationTest` | 0/14 because every fixture changes the database-wide `default_transaction_isolation` with `ALTER DATABASE`; pg-datahike supports connection and transaction settings instead. |

`EmptyQueryTest`, named in an earlier inventory, does not exist in the pinned
`REL42.7.5` source tree.

**How to re-enable**: add a class back to `TESTS=(…)` in `run.sh`, run
it, triage the failures, either fix or pin a specific test via
`expected-failures` (this file) and update the class's entry.

## Excluded permanently (feature gaps we don't implement)

These exercise features pg-datahike will not implement. Including them
produces only noise.

| Pattern | Why skipped |
|---|---|
| `CopyTest`, `CopyLargeFileTest`, `CopyBothResponseTest`, `PGCopyInputStreamTest` | pg-datahike admits text/CSV `COPY FROM STDIN`; these classes are dominated by `COPY OUT`, binary/file streaming and cancellation variants outside that surface. |
| `BlobTest`, `BlobTransactionTest` | Large-object API (`lo_*` server functions) not implemented. |
| `NotifyTest` | `LISTEN` / `NOTIFY` async messages not implemented. |
| `ReplicationTest`, `LogicalReplicationTest`, `V3ReplicationProtocolTest` | Logical / physical replication not implemented. |
| `SslTest`, `Ssl*Test`, anything under `test/ssl/` | Core TLS, `sslmode=require`, and `verify-full` are covered in pg-datahike. The upstream suite additionally controls a PostgreSQL installation's certificate/client-auth matrix, which this harness does not provision. |
| `ScramTest`, SASL-related auth | SCRAM auth is not implemented; configured servers use PostgreSQL cleartext-password authentication inside TLS. |
| `XA*Test`, `test/xa/` | XA / two-phase commit not implemented. |
| `CallableStmtTest`, `Jdbc42CallableStatementTest` | Stored procedures / functions / `CALL` not supported. |
| `ArrayTest`, `EnumTest`, `GeometricTest`, `IntervalTest`, `JsonbTest`, `UUIDTest`, `XmlTest` | Non-core PG types; partial support only, excluded for signal. |
| `EncodingTest`, `ClientEncodingTest`, `DatabaseEncodingTest` | UTF-8 is hardcoded; other encodings will fail by design. |
| `ConnectTimeoutTest`, `LoginTimeoutTest` | Timing-sensitive network tests; flaky under local loopback. |
| `CustomTypeWithBinaryTransferTest`, `NumericTransferTest*` | Binary transfer codecs for extended types not implemented. |
| `ConcurrentStatementFetch`, `CursorFetchTest` | Server-side cursors with fetch-size; partially supported only. |
| `AutoRollbackTestSuite` | Exercises PostgreSQL-specific autosave and rollback modes beyond the transaction error behavior admitted by the application gates. |
| `hostchooser/`, `socketfactory/`, `sspi/` | Multi-host failover and Kerberos/SSPI not applicable. |

## Expected failures within the focus list

None currently. All admitted classes run clean. Any failure here = real
regression.
