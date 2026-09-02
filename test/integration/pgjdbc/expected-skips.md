# pgjdbc — expected skips and deferred classes

This file is the contract for what `run.sh` is and is not exercising. If
you see a failure outside the lists below, treat it as a regression
signal.

## Included in 0.1 CI

Just one class — the verified-stable surface:

| Class | Rationale |
|---|---|
| `org.postgresql.test.jdbc2.ResultSetTest` | 80/80 pass. Exercises cursor behavior, metadata, `wasNull`, type coercions on getters/setters, updatable-ResultSet flags. Binary + text modes both green. |

## Deferred to post-0.1

These exist in the pgjdbc test suite and pg-datahike routes most of
their traffic successfully, but each class has at least one known gap
that blocks it from joining the per-commit CI list:

| Class | Why deferred |
|---|---|
| `DriverTest` | TBD — re-run once 0.1 is out. |
| `ConnectionTest` | TBD. |
| `MiscTest` | TBD. |
| `StatementTest` | TBD. |
| `PreparedStatementTest` (+ `jdbc42`) | ~30% fail rate; one specific bug is `testUpdateWithPGobject` under FORCE binary (addressed), others open. |
| `ServerPreparedStmtTest` | TBD. |
| `BatchExecuteTest` | 126/132 passed on 2026-09-01. `testMixedBatch` and `testBatchWithEmbeddedNulls` now pass all four binary/rewrite variants. Two distinct failures remain: `testBatchWithAlternatingTypes` lets an unresolved `ParamRef` reach bigint coercion, and rewritten `testBatchReturningMixedNulls` does not provide the expected chained `BatchUpdateException`. |
| `ResultSetMetaDataTest` | TBD. |
| `GetXXXTest` | TBD. |
| `DatabaseMetaDataTest` / `jdbc4` / `jdbc42` | Pounds pg_catalog / information_schema projections; our virtual catalogs cover most but not all columns. |
| `DatabaseMetaDataPropertiesTest` | TBD. |
| `DatabaseMetaDataTransactionIsolationTest` | TBD. |
| `EmptyQueryTest` | TBD. |

**How to re-enable**: add a class back to `TESTS=(…)` in `run.sh`, run
it, triage the failures, either fix or pin a specific test via
`expected-failures` (this file) and update the class's entry.

## Excluded permanently (feature gaps we don't implement)

These exercise features pg-datahike will not implement. Including them
produces only noise.

| Pattern | Why skipped |
|---|---|
| `CopyTest`, `CopyLargeFileTest`, `CopyBothResponseTest`, `PGCopyInputStreamTest` | `COPY IN/OUT` protocol is not implemented. |
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
| `AutoRollbackTestSuite` | Exercises server-side rollback-on-error path. |
| `hostchooser/`, `socketfactory/`, `sspi/` | Multi-host failover and Kerberos/SSPI not applicable. |

## Expected failures within the focus list

None currently. `ResultSetTest` runs clean. Any failure here = real
regression.
