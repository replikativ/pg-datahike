# PostgreSQL compatibility

pg-datahike implements PostgreSQL's application-facing SQL and wire-protocol
boundary on top of Datahike. It is intended to let PostgreSQL clients and
applications use Datahike without adopting a separate query protocol. It is
not a drop-in replacement for the PostgreSQL server or its internal storage,
planning, administration, replication, and extension machinery.

Compatibility is developed and tested at the boundaries applications observe:

| Area | Compatibility target |
|---|---|
| Wire protocol | Startup, authentication, simple and extended queries, prepared statements, portals, result metadata, and SQLSTATE-bearing errors used by supported clients |
| SQL | A growing application-oriented subset of PostgreSQL DDL, DML, expressions, relational queries, transactions, and type semantics |
| Types and functions | PostgreSQL-compatible behavior for admitted built-ins; coverage varies by type and function family |
| Catalogs | The catalog relations and metadata queries needed by supported drivers, tools, and ORMs |
| Datahike semantics | Datahike remains the storage and transaction system; PostgreSQL behavior that conflicts with its model may be unsupported or documented as different |

PostgreSQL-specific server features such as procedural languages, server-side
extensions, replication, roles and server administration, physical storage
features, and planner controls are generally outside this compatibility target.
Their syntax may still be recognized where that is useful, but applications
must not assume that arbitrary PostgreSQL SQL or extensions will run unchanged.

## Wire security

pg-datahike implements PostgreSQL's conventional `SSLRequest` negotiation and
cleartext-password authentication exchange. The password exchange is intended
to run inside TLS: `start-server` rejects a non-loopback bind unless both TLS
and a password authenticator are configured, and it rejects a plaintext
StartupMessage before asking for a password. Standard client modes including
`sslmode=require` and `sslmode=verify-full` are tested with pgjdbc and psql.

Authentication is a deployment boundary rather than a PostgreSQL role system.
The wire layer delegates verification to an application callback, with a fixed
user map available for small deployments. Authenticated users are not yet
mapped to Datahike's per-database authorization model. PostgreSQL HBA rules,
SCRAM, MD5, client-certificate authentication, GSS encryption, and direct TLS
negotiation are outside the current surface.

## Compatibility guarantees

Supported behavior is established by focused tests, client and framework
integration suites, and admitted slices of PostgreSQL's upstream regression
suite. Upstream tests are also used as a discovery corpus: a regression file
may exercise both useful application behavior and PostgreSQL server internals,
so using it during development does not imply that the whole file is supported.

Within the supported surface, pg-datahike aims to:

- return PostgreSQL-compatible rows, column metadata, type OIDs, and NULL
  semantics;
- behave consistently across simple and extended query protocols;
- reject unsupported or invalid operations with explicit, SQLSTATE-bearing
  errors rather than internal failures or silent approximations;
- preserve Datahike's transaction and durability guarantees.

Compatibility is still evolving. Before replacing PostgreSQL for an existing
application, run that application's own integration and migration tests against
pg-datahike. Please report a minimal query, expected PostgreSQL behavior, actual
pg-datahike behavior, and client/driver details for any discrepancy.

Maintainers can find the upstream-suite workflow, admission criteria, and
PostgreSQL source map in the
[regression harness documentation](../test/integration/postgres-regress/README.md).
