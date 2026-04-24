# sql.clj split plan

`src/datahike/pg/sql.clj` was 7,484 lines. The split broke the monolith
into topical namespaces under `datahike.pg.sql.*`. Goals met: every
namespace ≤ ~2,000 lines (save `stmt`, which is 2,819 — select + DML +
CTE share call sites so they stay together), one responsibility per
file, no cyclic deps.

## Final state

```
datahike.pg.sql               top-level dispatch + preprocess-sql   ~800 lines
                              + catalog-cache machinery + re-exports
datahike.pg.sql.params        param substitution + OID inference    ~400 lines
datahike.pg.sql.fns           SQL helper fn wrappers + aggregates   ~305 lines
datahike.pg.sql.ctx           translation context primitives        ~280 lines
datahike.pg.sql.ddl           CREATE TABLE/SEQUENCE + constraints   ~620 lines
datahike.pg.sql.catalog       virtual catalog tables + system-query ~750 lines
datahike.pg.sql.expr          expression + predicate translation   ~1960 lines
datahike.pg.sql.stmt          SELECT / INSERT / UPDATE / DELETE    ~2820 lines
                              / CTE (kept together: translate-select
                              ↔ translate-recursive-cte ↔
                              translate-insert → translate-select
                              mutual recursion)
```

Core API surface unchanged: `datahike.pg.sql/...` re-exports
`filter-*`, `sql-+/-/*/div/mod`, `ParamRef`, `substitute-params`,
`make-ctx`, `col-var!`, `entity-var!`, `resolve-inherited-attr`,
`register-catalog-table!`, `unregister-catalog-table!`,
`extract-empty-catalog-shape`, `system-query?`,
`translate-predicate`, `eval-check-predicate`, `eval-update-expr`,
`coerce-insert-value`.

## Dependency graph (all edges downward — no cycles)

```
                 sql (top-level dispatch + preprocess + parse-sql)
              /    |      \        \        \
       catalog   ddl       stmt   expr
              \     |      /       /
               \    |     /       /
                \   +----+-------+
                                 |
                                 ctx    fns
                                   \   /
                                  params
```

`stmt → expr` for expression translation inside SELECT/DML bodies.
`expr → params` for placeholder records; `expr` consults
`ctx/*parse-sql*`-equivalent via `(:parse-sql ctx)` for subquery
recursion. `params/*parse-sql*` is bound by `parse-sql` to itself so
`stmt` entries seed it into `make-ctx` without a cyclic require.

## Guardrails discovered during the `params` extraction

Apply to every subsequent step.

- **Dynamic vars don't alias cleanly**. If an extracted namespace owns
  a `^:dynamic` var that older call sites use via `binding`, the older
  sites must switch to the qualified name (`params/*parse-db*`).
  Re-exports via `(def ...)` break `binding` semantics.

- **Non-dynamic names re-export fine**. Fns, records, maps → just
  `(def re-exported other-ns/src)` at the top of `sql.clj`. Keep
  external API stable for `server.clj` + tests.

- **Private fns exported for cross-ns reach**: if an extracted ns
  needs a fn originally marked `defn-`, promote it to `defn` (or
  add a `(def ^:private re-exported ...)` in the consumer). Small
  cost, keeps tooling honest.

- **cljfmt drift after bulk moves**: run `clj -M:ffix` at the end of
  each step. `bb format` CI job will catch it otherwise.

- **clj-kondo reference count**: after every split step, watch for
  new "unused import" warnings — each new ns copies its own
  `:import` list.

## Exit criterion met

`sql.clj` is ~800 lines: ns header, re-exports, catalog-cache +
preprocess-sql + parse-sql dispatch (the preprocess-sql regex tail and
parse-sql's CTE/derived-table handling still live here because they
coordinate across all the topical namespaces). Every `translate-*`
body lives in its topical namespace. `bb test` 235/960 green,
`bb sqllogictest` 59/59 green, `bb format` clean.
