# Routines and triggers

Status: implementation contract for the compatibility campaign.

The goal is not to make a few PostgreSQL regression files appear green. It is
to add a coherent routine and trigger subsystem that applications can depend
on and that can be extended without replacing its foundations. We will spend
more effort where completing a category produces a simpler design than a
special case.

The machine-readable scope and exact PostgreSQL 17 evidence ranges live in
[`routine-trigger-capabilities.edn`](../test/integration/postgres-regress/routine-trigger-capabilities.edn).
This document explains the invariants behind that ledger.

## Product boundary

The campaign targets:

- complete SQL-language functions over pg-datahike's supported SQL surface;
- a useful, deliberately bounded PL/pgSQL interpreter;
- ordinary-table triggers across INSERT, UPDATE, DELETE, TRUNCATE, COPY, and
  `INSERT ... ON CONFLICT`;
- routine-backed casts, operators, and aggregates once routine execution is
  shared;
- PostgreSQL-compatible persistent metadata, lookup, dependencies, error
  classes, and transaction behavior for those categories.

The first implementation does not load PostgreSQL native shared libraries.
View `INSTEAD OF` triggers, transition tables, deferred constraint triggers,
partition trigger cloning, event triggers, and advanced PL/pgSQL form separate
categories. Routine security, `ENABLE REPLICA`/`ENABLE ALWAYS`,
`session_replication_role`, and MERGE trigger integration are also later
categories. Ordinary local enable/disable remains in the current table-trigger
target. Deferred categories must fail explicitly until implemented; metadata
must not claim that they work.

## Foundation

### Object addresses

Persistent SQL objects use one class-qualified address:

```clojure
{:class-oid 1255       ; pg_proc, pg_class, pg_trigger, ...
 :object-oid 16384
 :sub-id 0
 :kind :function
 :namespace-oid 2200
 :name "calculate_total"
 :owner-oid 10
 :revision 3}
```

OIDs come from a transactional, database-global user-OID allocator. Allocation
must not scan a subset of catalogs for `max + 1`: that both permits collisions
between object classes and races concurrent DDL. Namespace identity and
`search_path` are persistent rather than inferred from dotted strings.

Dependencies refer to complete object addresses. `DROP ... RESTRICT`, cascade,
rename, replacement, dump order, and cache invalidation all consume the same
dependency graph. Routine arguments are ordered child records; a map cannot
represent repeated types, modes, names, defaults, and variadic position
faithfully.

### One statement executor

The pgwire adapter is a protocol boundary, not an internal API. Nested SQL from
a function or trigger enters a shared statement executor directly. An execution
context carries at least:

- transaction and statement snapshots;
- the currently visible speculative database value;
- parameters, local variables, and row bindings;
- namespace and `search_path`;
- recursion depth and resource limits;
- deterministic effects, command results, notices, and deferred events.

The executor returns structured command results. The wire layer alone converts
those results into PostgreSQL protocol messages.

An effectful statement is evaluated once. It records primitive transaction
effects and commit preconditions; commit must not re-run a volatile expression,
function, or trigger. This rule applies equally to a top-level statement,
nested function SQL, trigger SQL, COPY, and conflict handling.

`IMMUTABLE` and `STABLE` functions read the statement snapshot. `VOLATILE`
functions see changes already made by the current command and may write. A
volatile or effectful routine call must not be hidden inside a Datalog binding
whose evaluator may reorder or repeat it.

COPY is one statement. Its batching may bound memory, but no batch may become
durable before the complete COPY succeeds.

## Routine model

Builtin and user-defined routines expose the same descriptor shape. Parsing,
catalog projection, expression typing, overload resolution, prepared-statement
metadata, and execution all consume that descriptor; separate builtin tables
must not drift.

Routine identity follows PostgreSQL's input-argument signature. Resolution
accounts for schema qualification and `search_path`, exact matches, implicit
coercions, preferred types, unknown literals, variadic arguments, defaults,
and polymorphic families. Ambiguity is an error, not an arbitrary first match.

`CREATE OR REPLACE` preserves identity when PostgreSQL permits replacement and
increments the routine revision. Plans and compiled bodies are cached by
object OID, revision, argument types, namespace/search path, and any other
semantic input. DDL invalidates dependent entries transactionally.

SQL-language bodies support both string bodies and SQL-standard parsed bodies.
They are not normalized into the same stored representation because PostgreSQL
assigns them different parse-analysis and dependency timing. Execution covers
scalar, set, table, composite, and void results, plus multi-statement bodies
and writes over the SQL subset the normal executor supports.

PL/pgSQL is implemented as a small direct interpreter over a structural AST.
SCI is unnecessary: PL/pgSQL is not Clojure, and translating it into another
general-purpose language would add a second semantic and security boundary.
The core category includes blocks, declarations, assignments, IF/CASE, loops,
return forms, static SQL, PERFORM, SELECT INTO, RAISE, FOUND, and trigger
variables. Resource limits bound nesting, steps, produced rows, and recursive
trigger depth.

## Trigger model

Ordinary-table triggers are a complete category:

- BEFORE and AFTER;
- ROW and STATEMENT;
- INSERT, UPDATE, DELETE, and TRUNCATE;
- multiple events, `UPDATE OF`, `WHEN`, arguments, enable state, and
  deterministic name ordering;
- OLD, NEW, and the standard `TG_*` variables;
- BEFORE-row replacement and suppression;
- statement triggers even when a command affects no rows;
- nested DML, recursion limits, RETURNING, COPY, and ON CONFLICT.

DML uses a shared row pipeline. BEFORE-statement events run once, BEFORE-row
events transform or suppress candidate rows, constraints and conflict checks
see the transformed row, the primitive mutation is recorded, AFTER-row events
are queued in order, and AFTER-statement events run once. UPDATE and DELETE
retain stable row identity throughout the pipeline.

For `ON CONFLICT`, BEFORE INSERT runs before conflict detection because it may
change the key. If conflict handling chooses UPDATE, the UPDATE trigger path is
then applied with PostgreSQL's corresponding OLD and NEW values. RETURNING
captures the direct operation's row after BEFORE-trigger transformation.
Nested DML from an AFTER trigger may subsequently change that row, but all
AFTER work must succeed before the result is published.

Any error unwinds the whole statement, including all nested effects and queued
events. Transaction-control statements are rejected inside functions and
triggers where PostgreSQL rejects them.

## Verification and admission

Each subsystem change follows the same evidence chain:

1. Read PostgreSQL parser, catalog, executor, and regression sources for the
   category being implemented.
2. Add focused, pure tests for parsing, metadata, resolution, ordering, and
   error behavior.
3. Add differential tests against PostgreSQL for observable rows, update
   counts, result metadata, SQLSTATE, structured error fields, side effects,
   and transaction state.
4. Exercise pgjdbc and representative asynchronous, ORM, migration, and
   Pagila paths when their behavior depends on the category.
5. Run persistence/reconnect and dump/restore checks for every new persistent
   object.
6. Promote an exact upstream source range to `:admitted` only when the ledger
   names a live focused gate and the test declares
   `^{:postgres-evidence :evidence-id}` metadata.
7. Run the complete unit, integration, driver, and compatibility gates before
   merging a subsystem PR.

Security/resource review is required for every language-handler change.
Persistent catalog changes also require a dump/restore and upgrade review.
No regression admission may rely on accepting a silent wrong answer, dropping
an effect, weakening a SQLSTATE, or replacing unsupported behavior with a no-op.
