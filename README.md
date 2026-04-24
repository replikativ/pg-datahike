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

(def cfg {:store {:backend :mem :id (random-uuid)}
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

One `start-server` call can serve many Datahike connections; clients
route on the JDBC URL's database name:

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

**Honest footnote for 0.1**: writes always land on the connection's
default branch, even when `SET datahike.branch` is active — pgwire
reads respect the pinned branch, but the transaction writer is
conn-wide. Users who need to write to a specific branch open a
second connection on `/<db>:<branch>` or use the Clojure API
(`datahike.versioning/branch!`, `merge!`, …). Post-0.1 item.

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
ROW LEVEL SECURITY, CREATE EXTENSION, COPY) with SQLSTATE
`0A000 feature_not_supported`. Most ORMs emit some of these
unconditionally — accept them silently per-feature or by named preset:

```clojure
;; silently accept every auth/RLS/extension no-op (Hibernate, Odoo, Rails)
(pg/make-query-handler conn {:compat :permissive})

;; accept specific kinds only
(pg/make-query-handler conn {:silently-accept #{:grant :policy}})
```

See `datahike.pg.server/compat-presets` for the preset bundles.

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

## Compatibility

Tested against:
- **pgjdbc** 42.7.x — `ResultSetTest` 80/80
- **psql** / libpq
- **psycopg2** (Python)
- **Hibernate** / Spring (via pgjdbc)
- **Rails ActiveRecord** / Ecto (via `pg` gems)
- **Odoo** 17 / 19 boot + module loading + TestORM suite
- **Flyway** / **Alembic** migrations (via advisory locks)

Known gaps (by design or deferred):
- No PL/pgSQL / stored functions
- No triggers / rules
- No `LISTEN` / `NOTIFY`
- No `COPY FROM/TO STDIN` (rejected with 0A000)
- FK `ON DELETE CASCADE` / `SET NULL` / `SET DEFAULT` rejected at DDL
  (RESTRICT is the default and only supported action)
- Single `public` schema — `CREATE SCHEMA` silently accepted but no-op
- Cursor materialization is eager (entire result set held in memory)
- No deferrable constraints
- Generated columns parse but aren't enforced

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
model and ORM-harness setup.

## Architecture

```
SQL → classify      (tokenize + structural routing)
    → shape         (structural matcher for catalog-probe SELECTs)
    → rewrite       (token-driven source rewrites before JSqlParser)
    → JSqlParser
    → sql.*         (translate AST → Datalog / tx-data)
    → server        (handler dispatch, wire protocol)
    → Datahike
```

Module inventory (`src/datahike/pg/`):

| Module | Role |
|---|---|
| `classify` | Token-based routing for SET, SAVEPOINT, system fns, etc. |
| `rewrite` | Span-based source rewriter for SQL shapes JSqlParser rejects |
| `shape` | Structural SELECT probe matcher for catalog queries |
| `sql.*` | AST → Datalog / tx-data translation (expr, stmt, ctx, ddl, catalog, fns, params) |
| `schema` | Virtual-table derivation + `:datahike.pg/*` hint support |
| `server` | Handler reify, wire dispatch, constraint enforcement |
| `errors` | Exception → SQLSTATE classification at the wire boundary |
| `types` / `jsonb` | Type coercion + JSONB helpers |
| `window` | Window-function post-processing |

## License

Copyright © 2026 Christian Weilbach and contributors.

Released under the Eclipse Public License 2.0. See `LICENSE`.
