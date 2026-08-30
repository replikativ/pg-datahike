(ns datahike.pg
  "Public API for pg-datahike — PostgreSQL access for Datahike.

   pg-datahike embeds a PG-compatible adapter inside a Datahike process:
   wire protocol, SQL translator, virtual pg_* / information_schema
   catalogs, constraint enforcement, schema hints. Clients that speak PG
   (pgjdbc, psql, psycopg2, Rails AR, Hibernate, Odoo, Flyway, …) talk
   to a Datahike database without a PG install. This namespace is the
   stable surface; `datahike.pg.*` sub-namespaces hold implementation
   details that may be reorganized without warning.

   ## Starting a server

   Single database:

     (require '[datahike.pg :as pg]
              '[datahike.api :as d])

     (d/create-database {:store {:backend :mem :id \"demo\"}
                         :schema-flexibility :write})
     (def conn (d/connect {:store {:backend :mem :id \"demo\"}}))

     (def srv (pg/start-server conn {:port 5432}))
     ;; … clients connect to localhost:5432 …
     (pg/stop-server srv)

   Multiple databases served from one server — clients route via the
   JDBC URL's database name (jdbc:postgresql://host:5432/prod):

     (def srv (pg/start-server {\"prod\"    prod-conn
                                \"staging\" staging-conn}
                               {:port 5432}))

   Unknown database names are rejected with SQLSTATE 3D000.

   Handlers are created per incoming connection by the server. For
   direct in-process use (tests, REPL), bypass the wire layer:

     (def h (pg/make-query-handler conn))
     (.execute h \"SELECT 1\")

   ## Schema hints for native Datahike databases

   A pre-existing Datahike database works immediately — attributes in
   the schema become virtual tables. Hints let you customize that view
   without changing the underlying schema:

     (pg/set-hint! conn :person/full_name {:column \"name\"})
     (pg/set-hint! conn :person/ssn       {:hidden true})
     (pg/set-hint! conn :person/company   {:references :company/id})

   - `:column`     — rename the SQL column
   - `:hidden`     — exclude the attr from SELECT * / information_schema
   - `:references` — FK target for `:db.type/ref` attrs (makes
                     `JOIN c ON p.fk = c.pk` work without `db_id`)
   - `:table`      — rename the SQL table for the attr's namespace

   ## Compat / opt-out

   The handler rejects unsupported DDL (GRANT, REVOKE, POLICY,
   ROW LEVEL SECURITY, CREATE EXTENSION, COPY) by default with
   SQLSTATE 0A000. For apps that can't be retrofitted to avoid
   emitting those — most ORMs — accept them silently per-feature or
   by named preset:

     (pg/make-query-handler conn {:compat :permissive})
     (pg/make-query-handler conn {:silently-accept #{:grant :policy}})

   See `datahike.pg.server/compat-presets` for the preset bundles.

   ## Temporal queries

   Datahike's `as-of` / `since` / `history` are exposed through SET:

     SET datahike.as_of   = '2024-01-15T00:00:00Z';
     SET datahike.since   = '2024-01-01T00:00:00Z';
     SET datahike.history = 'true';
     RESET datahike.as_of;

   ## Extending catalog surfaces

   Libraries that expose app-level metadata (audit tables, health
   checks, per-tenant views) can register additional virtual catalog
   tables. Entries are {:schema [Datahike-schema entries including a
   row-marker attr] :data-fn (fn [user-schema cte-db] rows)}.

     (pg/register-catalog-table!
       \"app_metrics\"
       {:schema [{:db/ident :app_metrics/key …}
                 {:db/ident :app_metrics/val …}
                 {:db/ident (datahike.pg.schema/row-marker-attr
                             \"app_metrics\") …}]
        :data-fn (fn [_us _db] (current-metric-rows))})

   The table is then queryable via SQL: `SELECT * FROM app_metrics`."
  (:require [datahike.pg.schema :as schema]
            [datahike.pg.server :as server]
            [datahike.pg.sql :as sql]))

;; ---------------------------------------------------------------------------
;; Server lifecycle
;; ---------------------------------------------------------------------------

(def start-server
  "Start a pgwire TCP server. See datahike.pg.server/start-server for
   the full options map."
  server/start-server)

(def stop-server
  "Stop a running pgwire server."
  server/stop-server)

(def password-authenticator
  "Adapt a Clojure password-verifier function to pgwire's authentication
   interface. See datahike.pg.server/password-authenticator."
  server/password-authenticator)

(def users-authenticator
  "Build a password authenticator from a small deployment-owned user map."
  server/users-authenticator)

(def ssl-context-from-pkcs12
  "Load a server TLS context from a PKCS#12 keystore."
  server/ssl-context-from-pkcs12)

(def make-query-handler
  "Create an in-process QueryHandler. Bypasses the wire layer — use
   this for tests, REPL work, or applications that embed Datahike
   directly without a TCP boundary. See server/make-query-handler for
   options (:compat, :silently-accept, :on-query, :db-name,
   :registered-databases, :tx-wrap).

   The `:tx-wrap` option is `(fn [tx-data] -> tx-data)`. Called
   immediately before every INSERT/UPDATE/DELETE's `d/transact`. Lets
   framework consumers inject `[:db.fn/call validate tx-data]` so
   transactor-side validators fire for SQL writes too. Default is
   `identity` (no wrap)."
  server/make-query-handler)

(def make-query-handler-factory
  "Build a multi-DB-aware QueryHandlerFactory that routes incoming
   connections on their StartupMessage `database` param. For callers
   who want to construct a PgWireServer directly (custom host/port,
   container-managed lifecycle, etc.); `start-server` uses it
   internally."
  server/make-query-handler-factory)

;; ---------------------------------------------------------------------------
;; Schema hints (:datahike.pg/* meta-attrs for the SQL-side view)
;; ---------------------------------------------------------------------------

(def set-hint!
  "Upsert a hint entity. `target` is the ident the hint describes
   (e.g. `:person/company`). `hint` is a map with any of
   `:column` / `:hidden` / `:references` / `:table`. Idempotent."
  schema/set-hint!)

(def ensure-hint-schema!
  "Idempotently install the :datahike.pg/* hint schema attrs on conn.
   Called automatically by `set-hint!`; exposed for callers under
   `:schema-flexibility :read` who want to prime the schema eagerly."
  schema/ensure-hint-schema!)

(def schema-hints
  "Return {attr-ident → hint-map} for every hint entity currently in
   the db. Primarily a debugging aid."
  schema/schema-hints)

;; ---------------------------------------------------------------------------
;; Catalog extension registry
;; ---------------------------------------------------------------------------

(def register-catalog-table!
  "Register a virtual catalog table. See datahike.pg.sql's
   implementation for entry shape."
  sql/register-catalog-table!)

(def unregister-catalog-table!
  "Remove a previously-registered catalog table."
  sql/unregister-catalog-table!)

;; ---------------------------------------------------------------------------
;; Test-fixture helpers (exposed so app tests can reset server-wide state)
;; ---------------------------------------------------------------------------

(def reset-lock-registry!
  "Clear the server-wide row-lock registry. Intended for test fixtures;
   don't call from handler code — locks release on COMMIT / ROLLBACK /
   DISCARD ALL and on connection close in production."
  server/reset-lock-registry!)

(def reset-advisory-locks!
  "Clear the advisory-lock registry. Test-fixture helper."
  server/reset-advisory-locks!)
