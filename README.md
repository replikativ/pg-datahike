# pg-datahike

<p align="center">
<a href="https://clojurians.slack.com/archives/CB7GJAN0L"><img src="https://badgen.net/badge/-/slack?icon=slack&label"/></a>
<a href="https://clojars.org/org.replikativ/pg-datahike"><img src="https://img.shields.io/clojars/v/org.replikativ/pg-datahike.svg"/></a>
<a href="https://circleci.com/gh/replikativ/pg-datahike"><img src="https://circleci.com/gh/replikativ/pg-datahike.svg?style=shield"/></a>
<a href="https://github.com/replikativ/pg-datahike/tree/main"><img src="https://img.shields.io/github/last-commit/replikativ/pg-datahike/main"/></a>
</p>

**PostgreSQL access for [Datahike](https://github.com/replikativ/datahike).**
Embeds a PG-compatible adapter — wire protocol, SQL translator, virtual
`pg_*` / `information_schema` catalogs, constraint enforcement, schema
hints — inside a Datahike process. Clients that speak PostgreSQL
(pgjdbc, psql, psycopg2, Rails ActiveRecord, Hibernate, Odoo, Flyway,
Alembic) talk to Datahike without a PostgreSQL install.

The integration is **bidirectional at the datom layer**: tables created
over SQL are normal Datahike schemas and queryable from Clojure via
`(d/q …)`; existing Datahike schemas are visible as SQL tables with no
setup. Temporal queries (`as-of`, `history`, `since`) are exposed through
`SET datahike.as_of`.

Status: **beta**. Ready for the workloads it has been tested
against (pgjdbc `ResultSetTest` 80/80, Odoo 19 module boot + TestORM,
Flyway-style migrations). Not a full PG dialect; see
[Compatibility](#compatibility).

## Quickstart

### Datahike Server (recommended)

For a network deployment, start with
[Datahike Server](https://github.com/replikativ/datahike). Its Docker image and
standalone JAR bundle pg-datahike with Datahike's HTTP API, durable system
catalog, authentication, and backend configuration. Enable the beta
PostgreSQL listener in the server configuration:

```clojure
{:host "0.0.0.0"
 :port 4444
 :token "replace-with-a-long-random-token"
 :system-db {:store {:backend :file :path "/var/lib/datahike/system"}}
 :pg-listener
 {:host "0.0.0.0"
  :port 5432
  :users {"app" "replace-with-a-separate-postgresql-password"}
  :tls {:keystore "/run/secrets/datahike-pg.p12"
        :keystore-password "replace-with-the-keystore-password"}}}
```

With that configuration saved as `server.edn`, run the published container:

```bash
docker volume create datahike-data
docker run --name datahike --detach \
  --publish 4444:4444 --publish 5432:5432 \
  --mount type=volume,source=datahike-data,target=/var/lib/datahike \
  --mount type=bind,source="$PWD/server.edn",target=/run/secrets/server.edn,readonly \
  --mount type=bind,source="$PWD/datahike-pg.p12",target=/run/secrets/datahike-pg.p12,readonly \
  ghcr.io/replikativ/datahike-server:latest \
  --config /run/secrets/server.edn
```

Alternatively, download `datahike-http-server-VERSION-standalone.jar` from a
[Datahike release](https://github.com/replikativ/datahike/releases) and run:

```bash
java -jar datahike-http-server-VERSION-standalone.jar --config server.edn
```

See Datahike's
[server deployment guide](https://github.com/replikativ/datahike/blob/main/doc/distributed.md#docker-and-podman)
for version-pinned images, secret files, storage, and TLS details.

### pg-datahike standalone (direct adapter)

Using pg-datahike directly remains supported and is useful for local work,
small single-purpose services, or deployments that provide their own catalog,
authentication, and lifecycle management.

Each pg-datahike release ships a runnable uberjar on
[GitHub releases](https://github.com/replikativ/pg-datahike/releases) —
JDK 17+ is the only prerequisite:

```bash
curl -fLO https://github.com/replikativ/pg-datahike/releases/latest/download/pg-datahike-standalone.jar
java -jar pg-datahike-standalone.jar --port 5432 --memory
# pg-datahike VERSION ready on 127.0.0.1:5432
#   backend:  memory
#   CREATE DATABASE:  enabled
#   databases: ["datahike"]

psql postgresql://datahike@localhost:5432/datahike
```

The default listener is deliberately local-only and does not authenticate.
Use Datahike Server for a batteries-included network deployment, or configure
password authentication and TLS when embedding pg-datahike. A non-loopback
`start-server` bind is rejected unless both are effective:

```clojure
(pg/start-server
 {"prod" prod-conn}
 {:host "0.0.0.0"
  :port 5432
  :users {"app" (System/getenv "PG_DATAHIKE_PASSWORD")}
  :tls {:keystore "/run/secrets/pg-datahike.p12"
        :keystore-password (System/getenv "PG_DATAHIKE_KEYSTORE_PASSWORD")}})
```

The TLS shorthand loads a PKCS#12 keystore. Applications with their own
certificate lifecycle can pass a configured `javax.net.ssl.SSLContext` as
`:ssl-context`. For external identity stores, replace `:users` with an
`:authenticator` function receiving connection metadata and a short-lived
password char array. PostgreSQL users are currently a node-level access gate;
they are not yet mapped to per-database Datahike principals.

Every `start-server` listener limits a single SELECT result to 100,000 rows by
default. Results over the limit fail with SQLSTATE `54000` before pg-datahike
allocates the pgwire row array; set-shaped queries also push `limit + 1`
into Datahike so their result relation stays bounded. Tune the ceiling for the
deployment with `:max-result-rows 250000`, or set it to `false` only when the
host process provides its own memory controls. PostgreSQL cursor paging and
driver fetch sizes remain useful for network backpressure, but Datahike queries
are not generally lazy.

Clients use ordinary PostgreSQL TLS settings:

```bash
PGPASSWORD="$PG_DATAHIKE_PASSWORD" \
  psql "host=datahike.example port=5432 dbname=prod user=app sslmode=verify-full sslrootcert=/path/to/ca.crt"
```

Password authentication uses PostgreSQL's cleartext password exchange inside
TLS. MD5 is deprecated by PostgreSQL and intentionally unsupported; SCRAM is a
future verifier-only credential option. `:require-tls? true` applies the same
TLS requirement to a loopback listener and is automatic for non-loopback
binds.

Useful flags (`--help` for the full list):

```bash
java -jar pg-datahike-standalone.jar --memory                      # ephemeral
java -jar pg-datahike-standalone.jar --port 15432 --data-dir /var/lib/dh
java -jar pg-datahike-standalone.jar --db prod --db staging        # pre-create dbs
```

The `dump` subcommand exports a database to portable PostgreSQL SQL —
replay-ready in either pg-datahike or real PG via `psql`. See
[Migration & pg_dump interop](#migration--pg_dump-interop):

```bash
java -jar pg-datahike-standalone.jar dump --data-dir /var/lib/dh --db prod \
                                         --out prod.sql
```

### Embedded library

```clojure
;; deps.edn
{:deps {org.replikativ/datahike     {:mvn/version "LATEST"}
        org.replikativ/pg-datahike  {:mvn/version "LATEST"}}}
```

### Native Datahike → SQL

A plain Clojure-created Datahike database is already a PG database:

```clojure
(require '[datahike.api :as d]
         '[datahike.pg :as pg])

(def cfg {:store {:backend :memory :id (random-uuid)}
          :schema-flexibility :write})
(d/create-database cfg)
(def conn (d/connect cfg))

(d/transact conn
  [{:db/ident :person/id   :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :person/name :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(d/transact conn [{:person/id 1 :person/name "Alice"}])

(pg/start-server conn {:port 5432})
;; psql postgresql://localhost:5432/datahike
;;   => SELECT * FROM person;
;;        id |  name
;;       ----+-------
;;         1 | Alice
```

### SQL → Datahike

`CREATE TABLE` + `INSERT` over pgwire transact normal Datahike schemas
and datoms. They're immediately queryable from Clojure:

```clojure
;; Over any PG client:
;;   CREATE TABLE widget (sku TEXT PRIMARY KEY, weight INT);
;;   INSERT INTO widget VALUES ('A', 10), ('B', 20);

;; From Clojure:
(d/q '[:find ?sku ?w :where [?e :widget/sku ?sku] [?e :widget/weight ?w]]
     (d/db conn))
;; => #{["A" 10] ["B" 20]}
```

## Four integration patterns

### 1. Multi-database server

One `start-server` call serves many Datahike connections; clients route
on the JDBC URL's database name. Three modes for managing the registry:

#### Static — register from Clojure once

```clojure
(pg/start-server {"prod"    prod-conn
                  "staging" staging-conn
                  "reports" reports-conn}
                 {:port 5432})
```

```
jdbc:postgresql://localhost:5432/prod      → prod-conn
jdbc:postgresql://localhost:5432/staging   → staging-conn
jdbc:postgresql://localhost:5432/nonsuch   → 3D000 invalid_catalog_name
```

`SELECT current_database()` returns the connected name; `pg_database`
enumerates the registry.

#### Dynamic — mutate from Clojure at runtime

```clojure
(def srv (pg/start-server {"main" main-conn} {:port 5432}))

(pg/add-database!    srv "extra"  extra-conn)   ; new client connections route
(pg/remove-database! srv "extra")               ; drop from the registry
(pg/databases        srv)                       ; #{"main"}
```

The Clojure-side and SQL-side mutation paths share the same atom, so
either source is visible to both.

#### SQL-driven — clients self-provision via `CREATE DATABASE`

Set a `:database-template` (a partial Datahike config) and pgwire
clients can use plain SQL to provision and tear down databases:

```clojure
(pg/start-server {"datahike" boot-conn}
                 {:port 5432
                  :database-template {:store {:backend :memory}
                                      :schema-flexibility :write
                                      :keep-history? false}})
```

```sql
CREATE DATABASE myapp;                                    -- new memory store, fresh UUID id
CREATE DATABASE myapp2 WITH KEEP_HISTORY = true;          -- override per database
CREATE DATABASE histdb WITH (BACKEND = 'memory',          -- Yugabyte-style paren form also works
                              KEEP_HISTORY = true);
DROP DATABASE myapp;
DROP DATABASE IF EXISTS old_one;
```

Without a `:database-template` (or explicit `:on-create-database` /
`:on-delete-database` hooks), `CREATE`/`DROP DATABASE` return SQLSTATE
`0A000 feature_not_supported` — provisioning from SQL is a deployment
policy decision, not a default.

The accepted `WITH` clause maps option names case-insensitively to
Datahike config:

| `WITH` option | Datahike config | Notes |
|---|---|---|
| `BACKEND` | `[:store :backend]` keyword | `'memory'`, `'file'`, `'pg'`, `'redis'`, `…` |
| `STORE_ID` | `[:store :id]` | Defaults to a fresh UUID per CREATE if omitted |
| `PATH` | `[:store :path]` | File backend; `{{name}}` interpolation supported |
| `HOST` / `PORT` / `USER` / `PASSWORD` / `DBNAME` | `[:store :*]` | `pg` / `redis` backends |
| `SCHEMA_FLEXIBILITY` | `:schema-flexibility` keyword | `'read'` or `'write'` |
| `KEEP_HISTORY` | `:keep-history?` boolean | |
| `INDEX` | `:index` keyword | `'persistent-set'` → `:datahike.index/persistent-set` |
| `OWNER` / `TEMPLATE` / `ENCODING` / `LC_COLLATE` / `LC_CTYPE` / `LOCALE` / `TABLESPACE` / … | — | PostgreSQL-only; silently accepted with a NOTICE for `pg_dump` round-trips |

Unknown non-PG option names return SQLSTATE `42601 syntax_error`. For
full control bypassing the template, set the hooks directly:

```clojure
(pg/start-server {"main" main-conn}
  {:port 5432
   :on-create-database (fn [name parsed-options]
                         ;; full Clojure freedom — derive config however
                         (let [cfg (operator-policy-for name parsed-options)]
                           (d/create-database cfg)
                           (d/connect cfg)))
   :on-delete-database (fn [name conn _parsed-options]
                         (d/release conn)
                         (d/delete-database (operator-config-for name)))})
```

### 2. Schema hints (native DBs)

`:datahike.pg/*` meta-attributes let you customize the SQL-side view of
an existing Datahike schema without touching its structure:

```clojure
(pg/set-hint! conn :person/full_name {:column "name"})      ; rename column
(pg/set-hint! conn :person/ssn       {:hidden true})        ; exclude from SQL
(pg/set-hint! conn :person/company   {:references :company/id}) ; FK target
```

After `set-hint!`, `SELECT name FROM person` works, `ssn` is invisible
to `SELECT *` / `information_schema.columns`, and
`JOIN company c ON p.company = c.id` resolves on Datahike's native ref
semantics.

### 3. Temporal queries via SET

Datahike's time-travel primitives are exposed as session variables:

```sql
SET datahike.as_of   = '2024-01-15T00:00:00Z';  -- d/as-of
SET datahike.since   = '2024-01-01T00:00:00Z';  -- d/since
SET datahike.history = 'true';                  -- d/history
RESET datahike.as_of;
```

Transparent to the client — every subsequent query sees the chosen
view of the db.

### 4. Git-like branching

Branching is cheap in Datahike: every transaction produces a new
immutable commit, so a "branch" is just a named pointer at a commit
UUID. Creation is O(1) — one konserve write, no data copy, no WAL
replay, no container provisioning. pgwire exposes the read side and
the admin ops through standard PG session variables and a small
`datahike.*` function namespace.

```sql
-- Introspect
SELECT datahike.branches();            -- returns one row per branch
SELECT datahike.current_branch();
SELECT datahike.commit_id();           -- current session's db head
SELECT datahike.parent_commits();      -- immediate parents (0/1/2+)

-- Admin — konserve-level writes, don't go through the tx writer
SELECT datahike.create_branch('preview', 'main');
SELECT datahike.create_branch('from-cid', '69ea6ee1-…');  -- from commit UUID
SELECT datahike.delete_branch('preview');

-- Session view: three cuts on the same immutable log.
-- They compose — feature branch's state as of yesterday is just two SETs.
SET datahike.branch    = 'feature';
SET datahike.commit_id = '69ea6ee1-2feb-5b61-be14-5590b9e01e48';
SET datahike.as_of     = '2024-01-15T00:00:00Z';
RESET datahike.branch;
```

Or pin a branch at connect time via the JDBC URL's database name
(same suffix pattern as the multi-DB registry):

```
jdbc:postgresql://localhost:5432/prod:feature   → prod-conn, pinned to :feature
jdbc:postgresql://localhost:5432/prod           → prod-conn, default branch
```

`SET datahike.commit_id = '<uuid>'` is Datahike-unique: no other
PG-compatible database lets you query by an exact commit identifier.

`SET datahike.branch` changes the session's read view; writes continue
to land on the connection's configured branch because the Datahike writer is
connection-wide. A connection opened on `/<db>:<branch>` is different: its
Datahike reader and writer are both pinned to that branch, so it is the route
for branch-local SQL mutations. The Clojure API
(`datahike.versioning/branch!`, `merge!`, …) remains available for workflows
that need explicit branch administration.

## Migration & pg_dump interop

pg-datahike round-trips with real PostgreSQL via `pg_dump` SQL on both
sides. Three concrete workflows:

### Real PostgreSQL → pg-datahike (load a `pg_dump` file)

`pg_dump` output replays straight into pg-datahike via `psql` or any
JDBC client. Schema-side coverage includes `CREATE TABLE` with FK
constraints, `CREATE SEQUENCE`, `DEFAULT nextval(...)`, `CREATE TYPE …
AS ENUM`, `CREATE DOMAIN`, partitioned tables (parent + children).
Data-side coverage includes `INSERT` (single + multi-VALUES) and
`COPY … FROM stdin` (text and CSV formats). Run with the `:pg-dump`
compat preset to silently accept the rest of `pg_dump`'s noise
(triggers, functions, materialized views, ALTER OWNER):

```clojure
(pg/start-server registry {:port 5432 :compat :pg-dump})
```

```bash
psql -h localhost -p 5432 -U datahike -d datahike -f my_pg_dump.sql
```

Validated end-to-end against:
- **Chinook** (15.6 k rows / 11 tables / FKs / NUMERIC / TIMESTAMP) —
  full bidirectional roundtrip, byte-identical per-row equality.
- **Pagila** (50 k rows / 22 tables / ENUM / DOMAIN / partitioning /
  triggers / functions) — schema parses end-to-end, data loads in
  ~12 s (≈ 4 k rows/s).

Replay throughput depends on how the client sends the inserts. The
fast paths are multi-statement Simple Query (what `psql -f` does),
JDBC `PreparedStatement.executeBatch`, and explicit transactions —
all of which commit batched rows through one underlying transact
instead of per-row. Each-INSERT-its-own-roundtrip clients are bound
by per-call commit cost (~370 r/s).

### pg-datahike → portable PG SQL (the `dump` tool)

`datahike.pg.dump/dump` walks a Datahike database and emits pg_dump-
shaped SQL. The output replays into either pg-datahike or real
PostgreSQL via `psql`:

```clojure
(require '[datahike.pg.dump :as dump])

(spit "out.sql" (dump/dump-to-string conn))
;; or stream:
(doseq [stmt (dump/dump conn)] (println stmt))
```

Or via the standalone uberjar's `dump` subcommand:

```bash
java -jar pg-datahike.jar dump --data-dir DIR --db NAME --out out.sql
java -jar pg-datahike.jar dump --config datahike-config.edn --copy
```

Options: `--inserts` (default) / `--copy`, `--schema-only` /
`--data-only`, `--exclude-table NAME`. The `--config` flag accepts a
full Datahike config EDN — works for any konserve backend (file, jdbc,
s3, redis, lmdb, …); store-id is auto-discovered from the persisted
`:db` branch so you don't need it up front.

### Native Datahike databases dump too — no setup required

A plain Datahike database (created via `d/transact`, never touched by
SQL) dumps as clean PG SQL out of the box:

- `:db.unique/identity` → `PRIMARY KEY NOT NULL`
- `:db.unique/value` → `UNIQUE`
- `:db.cardinality/many T` → `T[]` with PG array literals (`'{a,b}'`)
- `:db.type/ref` → `bigint` (the entity-id)

For FK constraints in the dump, opt in with `set-hint!` on ref attrs:

```clojure
(pg/set-hint! conn :order/customer {:references :customer/id})
;; → ALTER TABLE "order" ADD CONSTRAINT … FOREIGN KEY ("customer")
;;     REFERENCES "customer"("id");
```

This makes the "evaluate Datahike, fall back to real PG cleanly"
story concrete: a native Datahike database can be exported to a
PostgreSQL-shaped SQL file at any time, replayed on real PG, and
queried there. No schema rewrite, no second source of truth.

## Embedding without TCP

Bypass the wire layer for tests or in-process applications:

```clojure
(def h (pg/make-query-handler conn))
(.execute h "CREATE TABLE person (id INT PRIMARY KEY, name TEXT)")
(.execute h "INSERT INTO person VALUES (1, 'Alice')")
(.execute h "SELECT * FROM person")
```

## Compat with strict vs. permissive clients

By default the handler rejects unsupported DDL (GRANT, REVOKE, POLICY,
ROW LEVEL SECURITY, CREATE EXTENSION, triggers, functions, …) with
SQLSTATE `0A000 feature_not_supported`. Real-world clients emit a lot
of this noise unconditionally — opt in by named preset:

```clojure
;; ORM noise: GRANT/REVOKE/POLICY/RLS/EXTENSION (Hibernate, Odoo, Rails)
(pg/make-query-handler conn {:compat :permissive})

;; pg_dump output: superset of :permissive that also accepts
;; CREATE TRIGGER / FUNCTION / PROCEDURE / AGGREGATE / RULE,
;; MATERIALIZED VIEW, ATTACH PARTITION, ALTER TYPE/DOMAIN, etc.
;; Use this when loading dump files from real PostgreSQL.
(pg/make-query-handler conn {:compat :pg-dump})

;; or accept specific kinds only
(pg/make-query-handler conn {:silently-accept #{:grant :policy}})
```

See `datahike.pg.server/compat-presets` for the preset bundles. The
underlying `:silently-accept` set is also exposed on the standalone
server: `(pg/start-server … {:compat :pg-dump})`.

## Extending the catalog

Libraries can expose app-level metadata (audit tables, health checks,
per-tenant views) as virtual catalog tables without forking this repo:

```clojure
(pg/register-catalog-table!
 "app_metrics"
 {:schema [{:db/ident :app_metrics/key :db/valueType :db.type/string
            :db/cardinality :db.cardinality/one}
           {:db/ident :app_metrics/val :db/valueType :db.type/long
            :db/cardinality :db.cardinality/one}
           {:db/ident (datahike.pg.schema/row-marker-attr "app_metrics")
            :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
  :data-fn (fn [_user-schema _cte-db] (current-metric-rows))})
```

Then queryable as a regular table: `SELECT * FROM app_metrics`.

## Performance

Measured with stock `pgbench` against a real PostgreSQL 17 on the same
machine — see **[doc/benchmarks.md](doc/benchmarks.md)** for the current
numbers (memory and durable file store), full methodology, caveats, and
reproduction commands. Summary as of 2026-08: point reads within 3.8–6.8×
of PostgreSQL, writes within 2.1–4.1× (memory store) with zero failed
transactions and PostgreSQL-style row-locking concurrency.

## Compatibility

Tested against:
- **pgjdbc** 42.7.x — `ResultSetTest` 80/80
- **psql** / libpq
- **psycopg2** (Python)
- **Hibernate** / Spring (via pgjdbc)
- **Rails ActiveRecord** / Ecto (via `pg` gems)
- **Odoo** 17 / 19 boot + module loading + TestORM suite
- **Flyway** / **Alembic** migrations (via advisory locks)
- **Chinook** + **Pagila** `pg_dump` fixtures load end-to-end (see
  [Migration & pg_dump interop](#migration--pg_dump-interop))

### Modeled / first-class

`CREATE SEQUENCE`, `nextval` / `currval` / `setval` (with PG-correct
non-transactional semantics — advance survives rollback, concurrent
callers get distinct values), `DEFAULT nextval('s'::regclass)`,
`CREATE TYPE … AS ENUM` (membership enforced on INSERT — non-members
raise 22P02), `CREATE DOMAIN … [NOT NULL] [CHECK (…)]` (CHECK predicate
evaluated on INSERT — violations raise 23514, NOT NULL raises 23502;
`VALUE` keyword resolves to the column value), `text[]` arrays,
`tsvector` (opaque round-trip), pgvector-compatible `vector` / `vector(n)`
values, scalar distance functions and `<->` / `<#>` / `<=>` operators (exact
scan evaluation; ANN indexes, vector arrays, vector uniqueness, and
DISTINCT/GROUP BY vector keys are not exposed yet), `bytea`,
`timestamp with time zone`, `CHARACTER(N)`, `SERIAL`,
`COPY … FROM stdin` (text + CSV), `ALTER TABLE … ADD CONSTRAINT
FOREIGN KEY … ON UPDATE CASCADE ON DELETE RESTRICT`.

### Accepted under `:compat :pg-dump` (no semantics, schema loads)

`CREATE TRIGGER`, `CREATE FUNCTION` (incl. `$$…$$` body),
`CREATE PROCEDURE`, `CREATE AGGREGATE`, `CREATE RULE`, `CREATE OPERATOR`,
`CREATE CAST`, `CREATE LANGUAGE`, `CREATE MATERIALIZED VIEW`,
`ALTER TYPE / DOMAIN`, `ALTER TABLE … ATTACH PARTITION` /
`DETACH PARTITION`, `ALTER … OWNER TO`, `\restrict`/`\unrestrict`
psql metacommands, `pg_catalog.set_config(...)`. Partitioned tables
load: the parent's `PARTITION BY` clause is stripped, partition
children load as independent tables, and pg_dump's per-child INSERTs
land where they should.

### Known gaps (by design or deferred)

- No PL/pgSQL execution (CREATE FUNCTION's body is silently accepted
  but never invoked).
- No `LISTEN` / `NOTIFY`.
- FK `ON DELETE CASCADE` / `SET NULL` / `SET DEFAULT` rejected at DDL
  (RESTRICT is the default and only supported action).
- Single `public` schema — `CREATE SCHEMA` silently accepted but no-op.
- Cursor materialization is eager (entire result set held in memory).
- No deferrable constraints.
- Generated columns parse but aren't enforced.
- ENUM ordering is lexicographic (string storage) rather than PG's
  declaration-order. `ORDER BY` on enum-typed columns may diverge.
- Partitioned tables: data lives in the children, not the parent (pg-
  datahike doesn't model the partition relation). Live workloads that
  need partition-aware routing into the parent table are out of scope.
- **Constraint enforcement is one-directional in 0.1.** SQL constraints
  declared via DDL — `NOT NULL`, `CHECK`, `UNIQUE`, foreign-key
  child-side and parent-side `RESTRICT`, DOMAIN CHECK / NOT NULL,
  ENUM membership — are enforced by the pgwire handler at write time
  (via a `:db.fn/call` wrapper that runs against the speculative
  txdb, mirroring PG IMMEDIATE constraint semantics). Direct
  `(d/transact)` writes from Clojure bypass these checks because
  Datahike's schema does not yet carry the corresponding constraint
  vocabulary. Use the SQL path for constrained inserts, or validate
  explicitly before transacting. A future release will lift enforcement
  into Datahike's tx layer so both paths are gated.

## Development

```bash
# One-time: compile Java sources.
bb prep

# Run unit tests (requires Java classes built):
bb test

# SQL-conformance suite:
bb sqllogictest

# Format / lint:
bb format           # check
clojure -M:ffix     # auto-fix
bb lint

# Start a dev server on :15432 + nREPL on :15433 for dual access:
clojure -A:test -M test/integration/start_pgwire.clj

# pgjdbc conformance:
test/integration/pgjdbc/setup.sh    # once, clones pgjdbc@REL42.7.5
test/integration/pgjdbc/run.sh      # ~5 min
```

See `doc/integration-testing.md` for the full three-layer testing
model and ORM-harness setup. The [beta-exit campaign](doc/beta-exit.md) records
the release gates, remaining blockers, and deliberately unsupported surface.

## Architecture

```
SQL → sql.classify   (tokenize + structural routing)
    → sql.shape      (structural matcher for catalog-probe SELECTs)
    → sql.rewrite    (token-driven source rewrites before JSqlParser)
    → JSqlParser
    → sql.*          (translate AST → Datalog / tx-data)
    → server         (handler dispatch, wire protocol)
    → Datahike
```

Module inventory (`src/datahike/pg/`):

| Module | Role |
|---|---|
| `sql` | Top-level dispatch: preprocess + parse-sql + result-type routing. |
| `sql.classify` | Token-based routing for SET, SAVEPOINT, system fns, etc. |
| `sql.rewrite` | Span-based source rewriter for SQL shapes JSqlParser rejects |
| `sql.shape` | Structural SELECT probe matcher for catalog queries |
| `sql.{ddl,stmt,expr,ctx,catalog,fns,params,coerce,oid_infer}` | AST → Datalog / tx-data translation |
| `sql.template` | Lexical INSERT-VALUES templater (literal → `?` rewrite + typed substitute) for parse-result reuse on bulk inserts |
| `sql.database` | `CREATE`/`DROP DATABASE` token-parser + provisioning helpers |
| `sql.types` | `CREATE TYPE … AS ENUM` / `CREATE DOMAIN` parsers (bypass JSqlParser); registry-entity model |
| `sql.copy` / `sql.copy.{text_format,csv_format}` | `COPY … FROM stdin` parser + format decoders |
| `schema` | Virtual-table derivation + `:datahike.pg/*` hint support |
| `server` | Handler reify, wire dispatch, constraint enforcement |
| `dump` | Reverse direction: walk schema + data, emit pg_dump-shaped SQL |
| `main` | Standalone CLI (`serve` / `dump` subcommands) |
| `errors` | Exception → SQLSTATE classification at the wire boundary |
| `types` / `arrays` / `jsonb` / `window` | Runtime value model + post-processing |

## License

Copyright © 2026 Christian Weilbach and contributors.

Released under the [PostgreSQL License](LICENSE) — the same license as
PostgreSQL itself. Embed it, fork it, ship it commercially and closed-source;
no CLA is required to contribute (see [CONTRIBUTING.md](CONTRIBUTING.md)).

That covers pg-datahike's own code. As with PostgreSQL — whose shipped
binaries link GPL-3 readline into `psql` and LGPL-2.1 libsystemd into the
server — the assembled product includes copyleft components. Here that is
[datahike](https://github.com/replikativ/datahike) and the storage stack
beneath it, under the Eclipse Public License 1.0: file-level copyleft, so you
can build proprietary software on top and only need to publish changes to
those libraries' own files. `NOTICE` has the full breakdown for the
standalone uberjar.
