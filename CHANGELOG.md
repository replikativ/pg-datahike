# Changelog

All notable changes to pg-datahike.

## [Unreleased]

### A field of a composite value

`(expr).field` was not translated at all — every spelling raised `expression of type RowGetExpression is not supported`, including the whole-row form `(t).col` and the `(f(x)).n` a record-returning function is read with:

```
SELECT (row(1,2)).f1      →  1        SELECT (rc).a FROM rc   →  5
SELECT (row(1,2)).f3      →  42703 could not identify column "f3" in record data type
```

An anonymous ROW's fields are named `f1`, `f2`, … as PostgreSQL names them; a record that carries field names is selected by those, which is what `information_schema._pg_expandarray(…)` will need. A record still does not survive being projected through a derived table (it renders as its Java object), so `(result.KEYS).x` — the outer half of pgjdbc's primary-key query — is not reachable yet; that is recorded on plan item 0.7.

### A parameter inside a derived table or a CTE

```
SELECT * FROM (SELECT id FROM t WHERE id = ?) x          0 rows, for every binding
WITH x AS (SELECT id FROM t WHERE id = ?) SELECT * FROM x      same
```

A relation the translator has to **materialise** — a derived table, a CTE, a set operation — is produced by running its body and storing the rows in a speculative db. That happens while the statement is *parsed*, which under the extended protocol is before Bind: the body saw placeholders rather than values, matched nothing, and the relation came out empty. No error, just no rows.

Literals were fine, which is why it survived: it takes a bound parameter *and* a materialised relation together, and most tests use one or the other. Metabase's column introspection uses both, and got nothing back.

Such a statement is now marked while parsing and re-parsed at Execute with `params/*bound-params*` in scope, where the translator resolves the placeholders inline — the mechanism runtime subqueries already use. The parse-time plan is not cached, since the relation in it is empty.

### NOT NULL reaches the catalog

`name TEXT NOT NULL` came back as `pg_attribute.attnotnull` **false** and `information_schema.columns.is_nullable` **YES**: the catalog derived not-nullness from the primary key alone. Those two columns are what a client reads to decide whether a column may be omitted from an INSERT, and what Hibernate and SQLAlchemy reflect a schema through.

DDL records the constraint as `:pg/not-null` on the column's schema entity, and `(:schema db)` surfaces only the `:db/*` keys — the same blind spot `:pg/type` has, with the same fix: collect it in `schema-hints` and carry it on the column map. `information_schema` also counts a unique-identity column as NOT NULL, which it always was.

Found by a golden file that recorded the wrong answer: `pgjdbc-getColumns-person` had `[]` in it — the empty result the deleted field-metadata probe produced — and regenerating it against the real SQL path showed `name` nullable where PostgreSQL says it is not.

### The column-metadata probe is gone; the catalog answers it

pgjdbc's `ResultSetMetaData` query — the five-way join over `pg_class`, `pg_namespace`, `pg_attribute`, `pg_type` and a LEFT JOIN onto `pg_attrdef` — was recognised by shape and answered by a handler that regexed the `(oid, attnum)` pairs out of the inline `UNION ALL` and resolved them against the Datahike schema. It ran as SQL for the first time today and answers **exactly** what PostgreSQL answers, including `attidentity != '' OR pg_get_expr(d.adbin, d.adrelid) LIKE '%nextval(%'` finding a serial column's default and yielding NULL, not false, where the column has none.

What was keeping it a probe was the LEFT JOIN with a two-column ON, which multiplied one row per column into twenty — fixed in the outer-join work. Second of the three probes (plan item 0.7).

pgjdbc's suite, which drives this query through `ResultSetMetaData`, is unchanged at 275 passed / 0 failed.

### The session functions work in an expression

`pg_backend_pid()` and `txid_current()` were answerable only as a **whole statement** — `classify` matched the sole projection and a handler answered it — so `SELECT pg_backend_pid(), 1` and `WHERE pid = pg_backend_pid()` raised 42883 on a server where `SELECT pg_backend_pid()` works. The same shape the value functions had before they were translated.

Both now translate like any other call and read the session-state atom, which is what a Datalog function running off the connection's thread can still reach: the session carries its backend pid, and a thunk for the transaction id so a prepared statement re-executed later reports the transaction it *runs* in rather than the one it was planned in. The plan is session-dependent, so it never serves another session.

`pg_sleep`, `pg_notify` and the advisory locks stay whole-statement handlers: they need the connection itself at execution, not a value read from it. (Plan item 0.6.)

### A window column keeps its place in a derived table

`SELECT * FROM (SELECT a, row_number() OVER (…) AS rn, b FROM t) x` answered the columns **a, b, rn** — the window value last, shifting every column after it. The window executor appends its values to each row, and the derived-table path read them back in that order; each spec carries the position it had in the SELECT list, and the top-level path has always restored them by it.

Metabase's column introspection is exactly this shape (`row_number() OVER (PARTITION BY a.attrelid ORDER BY a.attnum) AS attnum` in the middle of sixteen columns), so every column after it was one place out.

`window-projection-indices` moves to `datahike.pg.window`, where both paths read it.

### A LIKE pattern that is not a literal, and ESCAPE

In value position the pattern was compiled to a regex at **translate** time, whatever it was. A column, a parameter or NULL arrives there as a logic variable or the null sentinel, and `(str …)` of those compiles to a regex that matches their printed form and nothing else:

```
SELECT s LIKE p FROM t     false for every row, whatever p held
SELECT 'a' LIKE NULL       false, where PostgreSQL answers NULL
```

The WHERE path has always deferred a non-literal pattern to a per-row matcher — pgjdbc rewrites every pattern literal into a parameter under the extended protocol, so it had to. Value position now does the same, three-valued: NULL on either side is NULL, not false.

**`ESCAPE` escaped the wrong character.** JSqlParser hands the clause over as a `StringValue` whose `toString` keeps the quotes, so taking its first character gave `'` — every `ESCAPE x` escaped the quote rather than `x`. `'a%c' LIKE 'a$%c' ESCAPE '$'` answered false, and the same predicate in WHERE answered no rows. Both paths read it through one helper now.

### A LEFT JOIN onto a ref target keeps every left row

`SELECT p.name, c.name FROM person p LEFT JOIN company c ON p.company = c.db_id` answered **no rows at all** when `person/company` is a `:db.type/ref` — an INNER join over the same columns was right, so the relation was there.

The lowering was written for the other direction, the ref's owner on the joined side (`FROM transaction LEFT JOIN posting ON posting.transaction = t.db_id`), and took the joined alias for the ref's owner either way. In this direction that emitted `[?c_eid :person/company ?c_eid]` — entity and value the same variable — and left the right side's own patterns outside the join, where they filtered.

The ON clause now records which side owns the ref, and this direction has its own lowering: one pattern, `[?p_eid :person/company ?c_eid]`, says the whole join; the right side is read through the joined entity variable; and the left row's own ref column stays bound outside, so a row whose ref exists but fails a further ON condition is null-extended rather than dropped.

Reachable only from a Datahike-native schema, which is why no SQL-created fixture and no fuzzer corpus covered it.

### FULL JOIN answers both sides, every time

Three defects, one query:

- **The rewrite mutated a cached AST.** A FULL JOIN is lowered by rewriting it into a LEFT JOIN in place (`setFull false`), and the AST cache hands out the same parsed object for the same SQL — so the FIRST execution answered a FULL JOIN and every later one a plain LEFT JOIN, silently dropping the right-only rows. Which answer you got depended on whether anything had run the statement before. Such a statement now gets its own AST.
- **The halves were combined by subtraction.** The second half ran as a full LEFT JOIN over the swapped tables, and rows equal to one from the first half were removed — which loses a right-only row that happens to equal a left row's projection, and loses duplicates outright. The second half now contributes exactly the rows with no match, so the halves are disjoint by construction.
- **The extended protocol got no RowDescription.** `describeResult` knew `:select` and `:set-operation` but not `:full-join`, so a client that Describes before Execute met DataRows with no description — `Received resultset tuples, but no field structure for them` — and **every later statement on that connection failed too**. It now describes from the left half, as set operations do.

An **aggregate, `DISTINCT`, `LIMIT` or `OFFSET` over a FULL JOIN is now refused with 0A000** rather than answered per half: `count(*)` returned two rows, `5` and `4`, where PostgreSQL returns `7`. Lifting that needs the combination to happen before aggregation — one query with two branches rather than two queries.

With these, the fuzzer's join surface goes from 47 disagreements to 10 — all of them that refusal, registered as expected — so **`:join` is now part of `bb fuzz all`**: INNER, LEFT, RIGHT and FULL, over both wire protocols, gate every change from here.

### A condition over an outer join's nullable side

`SELECT na.id, nb.w FROM na LEFT JOIN nb ON (na.id = nb.id) WHERE nb.w IS NOT NULL` raised **`Bad format for entity-id in pattern`** as XX000, and the same condition inside the ON raised `Cannot resolve any more clauses`. The column read was emitted as a `get-else` on the right *entity* variable, outside the or-join — the variable the unmatched branch grounds to `:__null__`.

The read now happens inside the join, where the matched branch binds it and the unmatched branch nulls it; the predicate stays outside, filtering the null-extended rows, which is how PostgreSQL reduces an outer join to an inner one. The join surface of the differential fuzzer drops from 47 disagreements to 27 — every one that remains is a FULL JOIN, which has its own items.

### The differential fuzzer joins

A new `join` surface generates a join type (`JOIN`, `LEFT`, `RIGHT`, `FULL`), an ON clause of one to three conjuncts around an equality — another equality, a comparison between the relations, a test over one side only — and a projection that may read the nullable side, then runs each sample over **both** wire protocols and compares the rows with a real PostgreSQL 17. `bb fuzz join [n] [seed]`.

Its join classes were one single-condition equi-join over `JOIN`/`LEFT JOIN`, which is why a LEFT JOIN that answered rows matching neither side of a two-condition ON lived here for as long as it did. On its first run the new surface found that FULL JOIN loses every right-only row, and that a condition over the nullable side leaks a datalog error as XX000 — both recorded in the plan. INNER joins are clean.

It is not in the `bb fuzz all` gate yet: it reports 47 known disagreements, and listing them as expected divergences would be a manifest larger than the manifest. It joins the gate when those are fixed (plan item 0.11).

### An outer join applies every condition of its ON clause

The lowering kept one. Each condition `reset!` a single `ref-info` map, so `LEFT JOIN b ON (b.x = a.x AND b.y = a.y)` joined on `y` alone — and **answered rows that satisfy neither pair**, with no error. Two more holes sat in the same construction:

- `ON (b.x = a.x AND b.y = 1)` **dropped** the unmatched left row instead of null-extending it: the unmatched branch negated the key pattern alone, so a left row whose key matched but whose predicate did not fell out of both branches.
- `ON (b.x = a.x AND b.y > a.y)` **answered nothing at all**: `a.y` was not in the or-join's head, so inside the branch it was a fresh unbound variable. Any ON condition reading a left column that is not a join key did this.
- A ref-based outer join read `ref-var`/`ref-attr` and nothing else, so a second condition was discarded silently.

Now every condition becomes part of the matched branch, the or-join's head carries each left variable a condition reads, and the unmatched branch negates the whole condition rather than the key. RIGHT and FULL are rewritten into this path and follow. One condition, and INNER JOIN with the same ON, are unchanged.

This is the join under pgjdbc's column-metadata query (`LEFT JOIN pg_attrdef d ON (d.adrelid = a.attrelid AND d.adnum = a.attnum)`), which multiplied one row per column into twenty — and which the field-metadata catalog probe has been hiding.

### A constraint-name query is answered from pg_constraint

`SELECT fk.conname AS name FROM pg_constraint fk WHERE …` was recognised by shape and answered `fk_<hash of the SQL>` — a name no catalog has, and the same answer for a table with no foreign key at all. The constraints are in `pg_constraint` under their real names, so the query now runs like any other: `child_pid_fkey`, or no row.

First of the three catalog probes (plan item 0.7). The other two are blocked: the field-metadata probe covers a LEFT JOIN whose multi-condition ON returns wrong rows, and the primary-key probe needs `information_schema._pg_expandarray`, composite field selection (`(x).n`) and `pg_index.indkey` as an array.

### A function name resolves against what this server implements

The scalar-function translator ended in a fallback: a name `clojure.core/resolve` could resolve was emitted as a Datalog clause, "so a caller reaches a Clojure fn we did not enumerate". Every public of `clojure.core` was therefore a function of this server, for anyone who can connect — `slurp('/etc/passwd')` read the file, `spit('/tmp/x','y')` wrote it, `inc(1)` answered 2, and `deref(1)` leaked a ClassCastException as XX000. A name is now resolved against what we implement and is **42883** otherwise, worded as `ParseFuncOrColumn` words it, argument types and hint included: `function nosuchfn(unknown, integer) does not exist`.

The decorated call forms resolved no better:

- **`f(x) OVER (…)`** built a window spec for any name at all, so an unknown one failed at execution with 0A000 "not supported" and a scalar one with datalog's own "Cannot parse :find" under XX000. Now 42883 for an unknown name, and **42809** — `OVER specified, but upper is not a window function nor an aggregate function` — for a function of the wrong kind, which `pg_proc` decides, as PostgreSQL does.
- **`f(x) FILTER (WHERE …)`** fell through to a default aggregate, so `SELECT nosuchfn(a) FILTER (WHERE true) FROM t` **answered a COUNT** where PostgreSQL raises. Now 42883, and 42809 for a scalar function.
- **A FROM-clause function** took the last dot-separated segment as its name, so `nosuchschema.unnest(ARRAY[1,2])` returned rows.

**One qualifier rule, for every path**: a qualifier is dropped only when it names the schema the function is in — `pg_catalog` for the builtins, `public` for the pgvector ones — so `public.upper('a')` is 42883 here as it is in PostgreSQL. The aggregate and window paths read the name for themselves and did not follow it: `pg_catalog.count(*)` reported that `count` does not exist on a server where `count(*)` answers, and `pg_catalog.max(a)` fell through to a per-row max — a silently wrong column.

Two further locks, behind that one: the projection interpreter resolves a literal symbol the way the engine itself would rather than through `clojure.core/resolve`, and a started server installs `datahike.query.resolve/safe-symbol-resolver` for the process, with the namespaces this translator emits symbols from registered. A query reaching the engine can name a function, and Datahike's default resolver — right for a process that writes its own queries — reaches every `clojure.core` public and any class method by reflection. That install is process-wide and outlives `stop-server`; an application issuing its own Datalog queries in the same process registers its functions with `register-fn!` / `register-ns!`, as it would against Datahike's own server.

One name goes the other way: `name('x')` used to answer `x`, by accident, through `clojure.core/name`. It is a real PostgreSQL function — one of the type-name cast functions (`text(…)`, `int4(…)`, `bool(…)`), none of which this server implements — so it is now 42883 with the rest of that family.

### ANY / ALL read their elements with the other side's type

`'{1,2}'` on the right of `= ANY` is an untyped array literal: PostgreSQL reads its elements with the input function of the type the comparison resolves to (`parse_coerce.c`). Three separate copies of the reader split the text on commas and kept the pieces as STRINGS, so an integer column was compared against `"1"`:

- `id = ANY('{1,2}')` matched nothing (it only ever matched through string coercion), `id > ANY('{1,2}')` matched nothing, `id <> ALL('{1,2}')` excluded nothing, and a numeric element never matched at all.
- The quoted form now answers like `ARRAY[1,2]` and `'{1,2}'::int[]`, in WHERE and in value position, and the literal is parsed as an array (quoting, `NULL` elements, nested braces) rather than by splitting on commas.

**A subquery on the right is now accepted**: `x = ANY (SELECT …)` is `x IN (SELECT …)` and `x <> ALL (SELECT …)` is `x NOT IN (SELECT …)` — the same sublink spelled with a quantifier (`transformAExprIn`). Both raised before; they are rewritten once, ahead of both predicate translators, so the existing IN path handles them.

### JOIN … USING and NATURAL JOIN are joins

Both were **cross products**: the columns they name were never turned into a join condition, so `a JOIN b USING (id)` returned every pair of rows (4 where PostgreSQL returns 2), and the merged name was reported ambiguous rather than answered. PostgreSQL synthesises the equality before planning and merges each pair into one output column (`parse_clause.c`).

- `USING (…)` and NATURAL now produce the equality, against the relations to the left of the join.
- The merged column is one column: an unqualified reference resolves to it, and `SELECT *` emits it once and first, followed by each relation's remaining columns — PostgreSQL's order. A qualified reference still names its own side.
- An OUTER join's merged column is `COALESCE` of the two sides, which this does not build yet, so it stays ambiguous rather than answering the left side's NULL.

### Names resolve level by level

PostgreSQL searches the innermost query level first and stops at the first level that has the name; ambiguity is only possible *within* a level (`colNameToVar`). Only QUALIFIED outer references were collected here, and the rest was patched per case:

- **An unqualified outer column in a subquery that has its own FROM** was 42703. `SELECT (SELECT count(*) FROM y WHERE b IS NULL) FROM z` now reads `z.b`, and two outer relations exposing the name raise 42702 rather than picking one.
- **A LATERAL item could not reference the relation beside it by ALIAS** — `FROM t x, LATERAL (SELECT x.c)` raised 42P01 where the same query by table name worked, because the alias was treated as a namespace. The reference now resolves like any other column, so renamed and inherited columns work too. An unqualified outer name inside a LATERAL resolves as well.
- **Two FROM items visible under one name** raise **42712** ("table name … specified more than once") when the namespace is built, as PostgreSQL reports it, instead of 42702 later at the column. A quoted name stays distinct from a folded one.

### The value functions are the translator's, not statement shortcuts

A SELECT whose projection was a single system call was answered by a hand-written handler instead of being translated. `version()`, `now()`, `current_schema`, `current_database()` and `pg_get_keywords()` no longer take that path:

- **`SELECT version(), 1` did not work**: it raised 42883, because only the whole-statement shortcut knew the name.
- **`SELECT (now() AT TIME ZONE 'UTC')`** answered a `timestamptz` column called `now`; PostgreSQL gives a `timestamp`. The shortcut matched the call and ignored what surrounded it.
- **`SELECT pg_get_keywords()`** answered one empty row called `string_agg`. It now raises, and the real set-returning function in FROM position — the form pgjdbc uses — is unaffected.

Column names and types are unchanged for the forms that already worked (`version` text, `now` timestamptz, `current_schema` and `current_database` name). `set_config`, `setval` and the `datahike.*` functions keep their handlers: they write session, sequence or branch state rather than returning a value.

### Assigning to an enum or domain column is type-checked

DDL lowers an enum column to text and a domain column to its base type, so the column's OID could not tell one enum from another: every enum column accepted any text, and the complaint came later from the enum's input function (22P02) — or not at all, for a label that happened to be valid. The assignment itself is now checked, as `transformAssignedExpr` does it, by the declared type's NAME.

- `SET enumcol = 'x'::text`, `= text_col`, `= 42` raise **42804**; `= 'ok'`, `= 'ok'::the_enum`, `= another_column_of_that_enum` are accepted.
- The message names the user type on either side: `column "m" is of type mood but expression is of type text`, and `column "i" is of type integer but expression is of type mood`.
- A domain is named rather than its base type (`column "di" is of type dint …`), and still takes whatever its base type takes.
- A source this layer cannot name — a subquery over an enum column reads as text here — is left to the base check rather than rejected, which is what PostgreSQL accepts too.

### One evaluator for DML

`eval-update-expr` and `eval-update-cond` are deleted, with the machinery that served them — about 1,200 lines. Every UPDATE (plain, with FROM, with a WITH clause) and every ON CONFLICT DO UPDATE now computes its values through the SELECT translator, so an expression means the same thing in a SET list as in a SELECT list.

Two fixes fell out of it:
- **A joined relation that does not exist** is `relation "x" does not exist` (42P01), PostgreSQL's message, rather than "missing FROM-clause entry" for whichever column first named it. Only the first FROM item was checked.
- **A schema-qualified name never resolves to a CTE**, as in PostgreSQL, where a CTE lives in no schema. This is what lets `WITH t AS (…) UPDATE t SET …` reach the table while the subqueries see the CTE.

### Multi-column SET from a subquery

`SET (a, b) = (SELECT x, y …)` was refused with 0A000. It now assigns both columns: a correlated row subquery reads the row being updated, no row gives every column NULL, more than one row is an error, and each column takes its own assignment cast. PostgreSQL evaluates the subquery once per row and this evaluates it once per column, which differs only for a volatile subquery; a set-operation subquery is still refused.

### ON CONFLICT DO UPDATE and CTE-backed UPDATE use the SELECT translator

- **ON CONFLICT DO UPDATE SET** is one projection over the conflicting row and `excluded`, as PostgreSQL's ExecOnConflictUpdate binds both tuples, instead of an expression at a time through the second evaluator. An unqualified column on the right of SET, or in the WHERE, is **42702** — PostgreSQL's answer, since `excluded` has every column of the target; it used to resolve to the target and answer. Aggregates are 42803 and window functions 42P20.
- **An UPDATE with a WITH clause** carries it into the query that computes the SET values, so the CTE is a relation like any other. `WITH d AS (…) UPDATE t SET n = d.n FROM d WHERE d.id = t.id` matched nothing and reported `UPDATE 0`; a CTE read by a subquery in SET raised "relation does not exist". WITH RECURSIVE UPDATE goes through the same path as every other UPDATE now.

### UPDATE ... FROM is one joined query

The source relation is joined into the query that computes the SET values, as PostgreSQL plans it, instead of running the target matcher once per source row.

- The SET list is evaluated for **every** joined pair, so `SET x = 100 / s.v` raises 22012 from a pair that loses, and a target row several pairs match is updated once.
- Sources that were refused with 0A000 now work: several relations, an explicit JOIN, a subquery, and a source that is the target itself. `FROM (VALUES …)` no longer requires literal cells.
- A FROM relation visible under the target's own name raises **42712**, as PostgreSQL does; the target's name is free once the target is aliased (`UPDATE t q SET … FROM t WHERE t.id = q.id`).
- A derived, VALUES or function source is materialised per execution rather than kept in the statement's plan, so a second execution reads the current rows.

RETURNING that references a source column still raises 42P01, and an unqualified column both relations have resolves to the target where PostgreSQL raises 42702. The per-source-row evaluator now runs only for a CTE-backed UPDATE.

### Type resolution: all-unknown is text, and the json family keeps column order

- **A construct whose inputs are all untyped literals resolves to TEXT**, as `select_common_type` does: `CASE WHEN … THEN NULL END`, `COALESCE(NULL,NULL)`, `NULLIF('a','b')`, `GREATEST`/`LEAST(NULL,NULL)`. Text is then a real type for what follows, so `SET int_col = CASE WHEN … THEN NULL END` raises 42804 instead of storing NULL, and `1 = (CASE WHEN true THEN NULL END)` raises 42883. One typed input still decides, and an expression we merely fail to type is unaffected.
- **`row_to_json` / `to_json` emit the table's column order**; they sorted keys like `jsonb`, which only agrees with PostgreSQL when the names happen to sort the same. `to_jsonb` keeps its sorted output, which was already right.
- **`json_agg` is no longer `jsonb_agg`.** It kept the jsonb family's key sorting and object punctuation, and dropped the line feed PostgreSQL writes before each composite element.

### A statement is checked against the catalog it was lowered against

- **`ERROR: catalog changed while statement was being executed` no longer fires without a reason.** The check a write makes before it commits compared the whole global catalog, so two things aborted unrelated statements: the OID allocator's counter, which every `CREATE` anywhere bumps, and other sessions' temp tables, which are global rows here and are dropped when a connection closes. Neither is visible to a statement in PostgreSQL, and both are now out of the comparison. A churn reproduction went from 303 errors and no successes to none; concurrent DDL on unrelated *permanent* tables still aborts.
- **A session value no longer leaks between sessions.** `current_schema` and `current_database()` are read through the session that translated the statement, and translated plans are shared server-wide, so one session could answer another's `search_path` — or one database's name to a session connected to another. Statements that read a session value are no longer cached across sessions.

### UPDATE computes its SET list in a query over the target

Plain UPDATE (no FROM) is planned as PostgreSQL plans it: a query over the target whose target list computes the new values, `SELECT db_id, (e1), … FROM target WHERE …`, run by the SELECT executor. The hand-written per-row evaluator remains only for UPDATE … FROM and CTE-backed UPDATE until they move too.

- The values follow PostgreSQL:
  - subqueries read the pre-statement rows;
  - WHERE filters before SET is evaluated (`SET x = 100 / y WHERE y <> 0`);
  - `now()` is one value per statement, and volatile functions (`random()`, `gen_random_uuid()`, `clock_timestamp()`) are evaluated per row, in SELECT too, where only `random` was;
  - `SET col = DEFAULT` advances a sequence default per row (it raised 0A000).
- The assignment cast is chosen from the expression's static type, so `SET int_col = '1.6'::text` or `SET bool_col = 1` raises 42804 even when no row matches. Aggregates (42803) and window functions (42P20) are rejected in SET. An unknown target column reads `column "c" of relation "t" does not exist`.
- Fixed along the way:
  - arrays written by INSERT … SELECT or UPDATE were stored as the text `[7, 8]`;
  - `||` on an array column concatenated text, and now follows array_cat/array_append's NULL rules, with an untyped operand taking the array's type;
  - a boolean JSON or array operator (`@>`, `?`, `&&`) is accepted as a CASE condition;
  - `num_col + $1` types the parameter as the column's type, so pgbench `-M prepared` no longer aborts;
  - SQL nested in a statement never takes the protocol-level system-call shortcut, so `(SELECT nextval('s'))` is not answered as a top-level `SELECT nextval` (it raises until sequence calls in subqueries are supported).

### Row-level expressions share the SELECT translator's scoping

The translator can now treat a single written row as a relation in scope, a "row scope". Its columns are bound to placeholders typed by their declared OIDs, it takes column metadata from its table, and subqueries correlate against it with the translator's own name resolution. CHECK, domain CHECK, ON CONFLICT … WHERE and RETURNING are evaluated through it. This follows how PostgreSQL projects over a tuple (ExecCheck, ExecQual, ExecProcessReturning).

- **RETURNING no longer uses the hand-written UPDATE evaluator.** Fixed:
  - `s || '[1]'` merged JSON whenever an operand parsed as JSON;
  - a templated literal (`RETURNING i + 1`) evaluated to NULL;
  - values rendered without their type: timestamptz lost `+00`, a date printed as a timestamp;
  - inherited columns are read from the table that declares them.

  Describe and Execute report the same OIDs, those of the compiled projection. Subqueries read the pre-statement snapshot, as in PostgreSQL. The row is in scope under its visible name only: `RETURNING t` is the row itself, a name outside it is 42P01, aggregates are 42803 and window functions 42P20, and `RETURNING *` handles names that need quoting. `RETURNING now()` is an expression over the row rather than the sole-call shortcut. `nextval()` in RETURNING raises 0A000 until the projection resolves sequence markers.
- **CHECK and ON CONFLICT evaluation drop the text substitution of 0.2.** Column references are no longer rewritten to `CAST($n AS type)`. ON CONFLICT … WHERE sees `excluded` as a scope of its own, on the target's level: an unqualified column both have is 42702, and `excluded.nosuch` is 42703 instead of NULL.
- **EXISTS in value position** (a projection, RETURNING, a CHECK) is evaluated as a correlated scalar subquery over the same WITH, FROM, WHERE, grouping and OFFSET. An ungrouped aggregate always yields a row and LIMIT 0 none. FETCH and a computed LIMIT raise 0A000. It raised "not supported" before.
- **An outer column in `IS NULL` or as a bare boolean** inside a subquery reads the outer value. It scanned the subquery's relation for an attribute of the outer table.
- **The fuzzer compares RETURNING rows,** and gains a RETURNING class.

### Constraint names are per table, and dropped with the table

- CHECK and FOREIGN KEY constraints are identified by table and name (`:pg/constraint-key`), not by name alone. Two tables may each declare a constraint called `pos`.
- DROP TABLE drops the table's own CHECK and FOREIGN KEY constraints. Left behind, they were enforced against a later table of the same name, which rejected rows its own definition allowed.
- Dropping a table that other tables' foreign keys reference is refused (2BP01, naming each dependent constraint). With CASCADE, those foreign keys are dropped. Before, they were left pointing at nothing and attached to a later table of the same name.
- Constraints are named as PostgreSQL names them:
  - An explicit inline `CONSTRAINT <name> CHECK` keeps its name.
  - An unnamed CHECK is `<table>_<column>_check` when it references one column, else `<table>_check`.
  - A generated name is numbered past any CHECK or FOREIGN KEY name the table already has (`<table>_check1`, `<table>_a_fkey2`) and shortened to 63 bytes as `makeObjectName` does.
  - Two explicit constraints of one table with the same name raise 42710.
  - One remaining difference: column-level CHECKs are named before table-level ones, where PostgreSQL follows the text order.
- Databases written before this change are migrated when the server starts. Their name-keyed constraint entities are re-keyed by table and name, and those of tables that no longer exist are removed.

### Datahike 0.8.1895

- Retracting an attribute no longer leaves an empty schema entry behind
  (datahike #1089). Every DROP TABLE left one per column: after 400
  CREATE/DROP cycles the schema map held 2527 entries, 2400 of them
  empty, against 127 now.

### CHECK constraints are evaluated like SELECT

CHECK constraints, domain checks and ON CONFLICT ... WHERE are evaluated by the SELECT translator (`datahike.pg.sql.row-eval`). The expression's columns become typed parameters of a one-row SELECT, which is translated once per expression. The interpreter it replaces is deleted. That interpreter treated every shape it did not know as satisfied, and compared only numbers with `<`/`>`. As a result:

- `CHECK (name > 'm')`, `CHECK (d < '2030-01-01')`, `CHECK (s LIKE 'a%')`, regex, `IS DISTINCT FROM`, jsonb operators and array subscripts accepted every row.
- Errors inside a CHECK (`x / 0`) were swallowed.
- `ON CONFLICT DO UPDATE ... WHERE s LIKE 'a%'` never updated.

Violations report PostgreSQL's messages: `new row for relation "t" violates check constraint "..."` and `value for domain d violates check constraint "..."`.

Other fixes in this change:

- Columns are typed exactly as declared: modifiers (`bit(3)`, `char(3)`), enum types and quoted identifiers such as Django's `"age"` are respected. A subscript's index may itself be a column. Shapes that can't be evaluated per row raise 0A000 instead of being evaluated wrongly.
- CREATE TABLE refuses a subquery in a CHECK (`cannot use subquery in check constraint`), as PostgreSQL does.
- UPDATE validates CHECK, domain and enum constraints through the same plan INSERT uses. The server's second copy of that code is removed.
- Enums compare in declaration order in value position (`SELECT 'happy'::mood > 'sad'`, CHECKs), not by their labels' text.
- An enum label outside the enum (`m > 'xyz'`) raises 22P02 when the statement is read, as PostgreSQL's parser does, in WHERE and in value position alike. `COLLATE` inside a CHECK, `timestamptz(3)`/`time(2)` columns and quoted mixed-case enum names evaluate correctly. CREATE DOMAIN refuses a subquery in its CHECK.
- Writes keep a coerced `false`, and SQL NULL from `INSERT ... SELECT`. Six write paths wrapped the column coercion in `(or (coerce v) v)`: a coerced `false` fell back to its input text, and a NULL fell back to the query engine's internal sentinel, which Datahike then rejected.
- A plan that is only a chain of function calls (a FROM-less one-row SELECT) runs without `d/q`, so per-row CHECKs cost about what the rest of the insert does.
- A cast of a bound temporal value (`CAST($1 AS date)` over a `java.util.Date`) became NULL, because the fold read `Date.toString`. It now converts the value.
- An untyped literal compared with a `time`/`timetz` expression is read as that type.
- DEFAULT:
  - A numeric literal keeps its text, so `numeric DEFAULT 1.50` keeps its scale.
  - The server's second `eval-default`, which used the wall clock for `now()`, is removed.
  - `now() AT TIME ZONE` is folded into `now()` only for UTC. Other zones are refused (0A000) instead of silently storing the wrong time.

### Text is read by PostgreSQL's input functions

`datahike.pg.input` ports the input functions of bool, int2/int4/int8, oid, float4/float8 and uuid (boolin, pg_strtoint*, uint32in_subr, float4in/float8in, uuid_in). numeric keeps `coerce-numeric`, which already matched `numeric_in`. Every path from text to these types now goes through them, with PostgreSQL's SQLSTATEs and messages:

- An untyped literal compared with a typed operand. `WHERE b = 'false'` found nothing, because a successfully read `false` was mistaken for "no coercion". `WHERE i = 'abc'` answered no rows instead of 22P02.
- Casts from text. `'1.5'::int` rounded to 2 instead of raising 22P02, and the error named `numeric`.
- INSERT/UPDATE, COPY and `pg_input_is_valid`/`pg_input_error_info`, which reports 22003 for out-of-range values.
- Array elements. Integer arrays keep their declared width (`int2[]` was read as int8) and are range-checked.
- Text-format Bind parameters. The Java decoder read every bool except t/true/1 as false. Invalid numbers were XX000.

The accepted syntax is PostgreSQL 16+'s:
- `0x`/`0o`/`0b` integers and `_` separators;
- `-1` as an oid (4294967295);
- `inf`, hex floats, and float underflow as 22003.

The duplicated bool and uuid parsers in `coerce`, `copy` and `PgParamCodec` are gone. Two new fuzzer classes, `litcmp` and `scalarin`, exercise these paths.
### PostgreSQL's catalog is generated, not transcribed

- `src/datahike/pg/pg_catalog.edn` holds the pg_type, pg_cast, pg_proc and
  pg_operator rows of PostgreSQL 17.7, generated from the release's
  `src/include/catalog/*.dat` by `bb gen-catalog`. Type lengths,
  categories, preferred types, collations, array types, implicit and
  explicit casts, and aggregate signatures are now derived from it instead
  of hand-written tables. A test fails when the file is edited by hand or
  disagrees with the pinned source.
- Two hand-copied facts were wrong: `"char"` is category Z, not S. And
  `time -> timetz` is an implicit cast; it stays excluded from resolution,
  in a named set, until comparisons convert the time operand (with it,
  `time = timetz` would answer false instead of raising 42883).

### Aggregates resolve their argument types

- Aggregate and window-function calls are resolved against every
  `pg_proc.dat` aggregate/window signature (generated from the pinned
  catalog). A call with no matching overload raises 42883, and an untyped
  literal that fits several candidates raises 42725, as in PostgreSQL.
  `sum(text)` used to fail with an internal ClassCastException, `sum(text)
  OVER ()` with 0A000, and `bool_and(int)` answered false.

### Values render and parse by their SQL type

- One output function (`types/->pg-text`) serves the wire, `::text`, `||`,
  `concat`, array elements and record fields, and dispatches on the value's
  TYPE before its JVM class. money prints `$1,234.50`; time keeps its seconds
  (`10:00:00`); fractional seconds print PostgreSQL's way (`.12`, not
  `.120`); timestamp array elements no longer use java.util.Date.toString in
  the JVM's zone; ARRAY and ROW take element/field types from the
  expressions.
- `AT TIME ZONE` converts (it returned its operand unchanged) and swaps
  timestamp/timestamptz as PostgreSQL does; numeric zones are POSIX-style.
- time/timetz and money input validate (22007/22008/22P02) instead of
  passing text through, on casts and on writes; stored time values are
  canonical.
- A bare `(a, b)` is a row (it leaked query variable names); `f((a, b))`
  passes one row argument.
- `timetz` columns are typed as such (CREATE TABLE had its own copy of the
  type-hint table, which lacked the short name).
- SQL NULL never renders as the internal `:__null__` sentinel.
- Tests that asserted the old rendering are corrected to PostgreSQL's
  expected output, including four admitted money.sql strict slices.
- The differential fuzzer gains type-directed classes over a second table.

### Differential fuzzing is a per-commit gate

- `datahike.fuzz.differential` (`bb fuzz`) generates SELECT, prepared and
  DML samples, runs them on PostgreSQL 17.7 and pg-datahike, and compares
  rows, or SQLSTATEs when both fail. The `differential-fuzz` CI job runs a
  fixed seed against a 17.7 sidecar and gates deployment. Known divergences
  are listed with reasons in `test/integration/fuzz/expected-divergences.edn`.
- `bb fncov` measures function breadth by calling every buildable
  `pg_catalog` overload on both servers.
- `doc/integration-testing.md` maps each behaviour area to the harnesses
  that cover it, and records the PostgreSQL regression-suite status.

### Silent wrong answers found by SQLSTATE-exact fuzzing

- `INSERT ... VALUES` evaluates non-literal expressions as PostgreSQL does.
  It used to store the SQL text of anything it could not evaluate (`'a' ||
  NULL`, `CASE`, a bare identifier), the internal `:__null__` sentinel for
  NULL results, and `now()` for any `AT TIME ZONE`. Expressions over
  prepared-statement parameters are evaluated at Bind (they reached the
  column as text such as `$3 + 1`).
- A `SELECT` without `FROM` raises 42703 / 42P01 for column references
  instead of answering no rows or implicitly adding the table. asyncpg's
  `test_prepare_02` now passes.
- Casts with no pathway in PostgreSQL (`date::int`, `bool::numeric`, ...)
  raise 42846 at parse instead of 22P02 at run time, using the pinned
  `pg_cast.dat`.

### PostgreSQL identity and loud refusals

- The server reports PostgreSQL 17.7 (`server_version` 17.7,
  `server_version_num` 170007, `version()`), the release the semantics and
  the regression campaign are pinned to. It reported 15.0. All four
  reporting paths read one constant in `PgWireServer`.
- `CREATE TABLE … PARTITION BY` is refused with 0A000 instead of silently
  creating an unpartitioned table. The `:pg-dump` preset keeps loading the
  parent as a plain empty table (new reject-kind `:partitioned-table`).
- The pagila `pg_dump` round-trip now also compares the partition tables
  (21 tables, previously 14): the partition filter had excluded them.

### Beta-exit verification

- Added an executable 0.2.0 beta-exit ledger covering per-commit, manual-release,
  and planned gates, with evidence paths and explicit non-goals. CI validates
  that every required job exists and blocks deployment.
- The PostgreSQL regression inventory now has a non-destructive setup command
  for its pinned `REL_17_7` source. CI verifies all 222 scheduled tests remain
  classified and all strict source slices still point to executable tests.
- The default-format Pagila `pg_dump` round-trip now blocks deployment rather
  than running as a non-release check.
- Fixed the lint task's false-green exit behavior. It now fails when clj-kondo
  reports errors; three pre-existing static-field invocation errors were
  corrected so the stricter gate starts green.
- Recorded the first local beta-exit baseline, including the environment-dependent
  asyncpg divergence and four distinct pgjdbc `BatchExecuteTest` failure classes.
- Deleting a row inserted earlier in the same transaction now cancels its buffered
  insert and update writes instead of asking Datahike to retract a tempid. All four
  pgjdbc `testMixedBatch` variants now pass.
- Text-format Bind parameters now reject embedded NUL and malformed UTF-8 instead
  of accepting Java replacement characters. The error uses PostgreSQL's `22021`
  `character_not_in_repertoire` SQLSTATE, and all four pgjdbc embedded-NUL batch
  variants now pass.
- Explicit casts around prepared INSERT parameters are now deferred until Bind
  supplies a value instead of trying to cast the `ParamRef` placeholder during
  Parse. All four pgjdbc alternating-parameter-type batch variants now pass.

### Password authentication and TLS

- The pgwire server can now request and verify PostgreSQL cleartext-password
  authentication through a deployment-owned callback or a small fixed `:users`
  map. Authentication failures use PostgreSQL's `28P01` response without
  revealing whether a user exists.
- PostgreSQL `SSLRequest` can now upgrade the existing connection to native
  TLS through an injected `SSLContext` or a PKCS#12 keystore. The pre-upgrade
  reader does not buffer or consume bytes past `SSLRequest`, matching
  PostgreSQL's protection against TLS buffer-stuffing attacks.
- Non-loopback `start-server` binds now require both password authentication
  and TLS, and reject plaintext startup before requesting a password. Existing
  loopback development listeners remain compatible and unauthenticated by
  default.
- JDBC coverage exercises accepted and rejected passwords,
  `sslmode=require`, hostname/certificate verification with
  `sslmode=verify-full`, and plaintext rejection. SCRAM and PostgreSQL MD5
  authentication are not part of this change.

### Licensing

- **pg-datahike is now under the PostgreSQL License**, replacing the Eclipse
  Public License 2.0. A PostgreSQL-compatible server should not be harder to
  build on than PostgreSQL: the new terms let anyone embed, fork, or ship it
  commercially and closed-source. Contributions are accepted under a DCO
  sign-off rather than a CLA, so contributed code arrives already licensed for
  commercial use — see `CONTRIBUTING.md`. Every line in the repository is the
  work of a single copyright holder, so no consent had to be collected.

  This governs pg-datahike's own code. Distributed artifacts — notably the
  standalone uberjar — still bundle datahike and the replikativ storage stack
  under EPL-1.0, which is file-level copyleft: proprietary work on top is
  fine, and only modifications to those libraries' own files must be
  published. The new `NOTICE` file records the full third-party breakdown and
  is now packaged into both jars at `META-INF/`.

### PostgreSQL conformance (issues #18–#22)

- **An empty query now answers `EmptyQueryResponse` instead of a parse
  error (#18).** A query string holding no statement — `""`, `";"`,
  `"  ;  ;  "`, a comment-only string — is an *empty query* in PostgreSQL:
  one `I` message and nothing else, per `exec_simple_query`'s
  `if (!parsetree_list) NullCommand(dest)`. `splitStatements` correctly
  discarded every blank fragment and then handed the *raw* string back, so
  `";"` reached JSqlParser and came back `42601`. It now reports zero
  statements, which routes to the `parsed == null` path that already
  existed. Two neighbours went with it: the statement loop's blank test
  missed comment-only fragments (comment stripping yields `" "`, not
  `""`), and `Parse(";")` errored in the extended protocol where PG
  accepts it. Verified message-for-message against PostgreSQL 17.10 on
  both protocols.

- **Math functions follow PostgreSQL semantics rather than
  `java.lang.Math`'s (#22).** `sqrt(-42)` returned `NaN`; PG raises
  `2201F`. The cause was structural — SQL functions were mapped straight
  onto their Java namesakes, which differ three separate ways:
  *domain errors* (Java returns NaN/Infinity where PG raises: `sqrt` of a
  negative `2201F`, `ln(0)`/`ln(-x)` `2201E`, `power(0,-x)` and
  `power(-x, 0.5)` `2201F`, `asin`/`acos` outside `[-1,1]` `22003`,
  overflow **and underflow** `22003`); *a different function under the
  same name* — SQL `log(x)` is base-10 while `Math/log` is natural, so
  `log(100)` silently returned `4.605` instead of `2`, a wrong answer
  rather than an error; and *different tie-breaking* — `Math/round`
  rounds halves toward positive infinity, so `round(-2.5)` gave `-2`
  where PG gives `-3`. Argument counts now resolve at translate time as
  `42883` instead of leaking Clojure's `ArityException` as `XX000`.
  Adds `ln`, `log10`, `log(b, x)`, `cbrt`, `asin`, `acos`, `atan2`,
  `sinh`/`cosh`/`tanh`, `asinh`/`acosh`/`atanh`, `degrees`, `radians`,
  `cot`, `trunc`, `gcd`, `lcm`, `width_bucket`, `pi`. PG's own
  inconsistencies are mirrored deliberately: `sinh`/`cosh` overflow to
  Infinity without error while `exp` raises, `cot(0)` is Infinity,
  `atanh(±1)` is Infinity, float8 `sign(NaN)` is `0`, and
  `width_bucket`'s argument failures are `2201G`, not the `22003` the
  surrounding float code uses.

- **`CREATE`/`ALTER SEQUENCE` are parsed in full (#21).**
  `CREATE SEQUENCE … INCREMENT 20 START WITH 400` failed to parse. The
  reported token was one hole in a grammar that is a strict subset of
  PG's: `START 400` (no `WITH`), `INCREMENT -1` (signed), `AS bigint`,
  `IF NOT EXISTS`, `NO MINVALUE` and every form of `ALTER SEQUENCE` also
  failed. Two pre-parse rewrite rules existed to delete the offending
  tokens, and that only moved the problem — the option *values* were then
  recovered by regex over the re-rendered SQL
  (`increment\s+by\s+(\d+)`), which cannot see a negative increment and
  silently dropped `MINVALUE`/`MAXVALUE`/`CACHE`/`CYCLE`, so
  `CREATE SEQUENCE s MAXVALUE 10 CYCLE` reported success and produced an
  unbounded non-cycling sequence. Sequence DDL is now token-classified in
  full and never reaches JSqlParser; both rewrite rules are deleted rather
  than extended. Defaults and validation mirror `init_params` including
  its *order*, so a statement with several problems reports the one PG
  reports — covering the direction-dependent defaults (`INCREMENT -1`
  gives min type-min, max `-1`, start `-1`), the `AS`-type bounds, and
  `NO MINVALUE` meaning "recompute the default" rather than "unbounded".
  The options are now honoured at runtime: `CYCLE` wraps to `MINVALUE`
  ascending and `MAXVALUE` descending (not to `START`), and exhaustion
  without `CYCLE` raises `2200H`. `ALTER SEQUENCE` revalidates against the
  sequence's current parameters; `RESTART` moves the counter without
  changing `START`.

- **`bit` / `bit varying` are real types (#19).** `SELECT 0::bit`
  returned a text column and `pg_typeof(0::bit)` answered `text`. The
  digits were already right — the value was a bare string, so both type
  paths fell through to text. Bit values are now a wrapper carrying the
  two things a string cannot: the **width**, which is part of the value
  (PG compares bit strings by content then length, so `B'101'` is *not*
  equal to `B'10100000'` and `B'0' < B'00' < B'000'`), and the
  `bit`/`bit varying` distinction (OIDs 1560/1562, with different width
  coercion — `bit(n)` zero-pads on the right, `bit varying(n)` truncates
  but never pads). Also fixes cases where treating a bit as its text gave
  a *wrong answer*: hex input expands to four bits per digit including
  leading zeros (`X'1F'` is the 8-bit `00011111`, not 5 bits, which
  changes `length`, `octet_length` and sort position); `int → bit(n)`
  keeps the rightmost `n` bits and sign-extends on the left, so
  `(-44)::bit(12)` is `111111010100`; and `bit → int` reinterprets the
  bits rather than reading the digits as decimal, so `'101'::bit(3)::int`
  is `5`, not `101`. `pg_typeof` also now reports its own return type as
  `regtype` (2206) rather than `text` — that is the OID quoted in the
  issue as the expected bit type.

- **`TRUNCATE TABLE` (#20)** was fixed earlier on this branch and simply
  had not shipped; v0.1.58 predates it.

### CAST consolidation

- **One implementation of `CAST`.** Cast semantics were written out four
  times — the table-free literal fast path, `translate-cast` (both its
  compile-time fold and its runtime binding), `apply-sql-cast`, and the
  INSERT coercion path — each a dispatch over the same type categories
  that had drifted from the others. Which copy ran depended on the
  *shape* of the expression rather than its meaning, so one cast could
  behave three ways: `29::bit(4)` was correct, `(-44)::bit(12)` passed
  the value through untouched, and `'101'::bit(3)::int` read the digits
  as decimal. Issue #12 hit this for `'1'::boolean` and was fixed by
  patching four call sites; #19 hit it again. The value-level semantics
  now live in one namespace and the scalar paths delegate to it; callers
  keep only their own surrounding logic. Unifying exposed differences the
  copies had accumulated, resolved toward the more complete behaviour:
  `apply-sql-cast` had no `:date`, `:time` or `:numeric` branch at all
  and returned such casts unchanged. Also fixes `length()` /
  `char_length()` / `octet_length()` on a bit string, which were bare
  `count` and so reported the wrapper's field count rather than the bit
  width.

### jsonb fidelity

- **jsonb is now canonicalized on ingest, so it behaves like PostgreSQL
  `jsonb` rather than `json`.** Previously a jsonb value was stored as its
  raw input text, so `'{"a":1,"b":2}'::jsonb = '{"b":2,"a":1}'::jsonb` was
  `false` (two different strings) where Postgres returns `true`, and
  duplicate keys survived. jsonb writes now recursively sort object keys,
  strip insignificant whitespace, and collapse duplicate keys to the last —
  so `=`, `DISTINCT` and `GROUP BY` compare by structure, and the value a
  client reads back is canonical. Array element order is preserved (only
  object keys sort); the text-faithful `json` type is left untouched. The
  jsonb-ness of a column (`:pg/type`, which lives as an ident-entity fact,
  not in datahike's `:db/*` schema) is now surfaced into the INSERT coercion
  path. Numeric normalization is Jackson's, not Postgres's, so a few numeric
  edge cases (`1.00`, `1e3`) may not match PG's exact jsonb numeric form —
  structural canonicalization is exact. This is the correctness prerequisite
  for indexing jsonb; index acceleration (a GIN-like secondary index) is
  tracked separately.

### Bulk-insert performance

- **Pagila replay: 274s → 12s (23×).** Cumulative across five
  changes layered on the wire path:
  - **Deferred-CC INSERT batching** in both Simple Query (`Q`) and
    Extended Query (`Bind/Execute … Sync`) so multiple INSERTs in
    one sync group commit through a single `d/transact`. dc/with at
    append time keeps constraint errors synchronous (matches PG
    IMMEDIATE semantics); only system-level / cross-connection
    failures land deferred.
  - **Parse-sql LRU cache** + **JSqlParser AST cache** keyed on the
    SQL string so repeated SQL (pgjdbc unnamed prepared statements,
    ORM-generated select-by-id, repeated INSERT shapes) skips re-
    parsing.
  - **Lexical INSERT-VALUES templater** (`datahike.pg.sql.template`)
    rewrites `INSERT INTO t (cols) VALUES (lit, …)` to
    `(? , …)` and captures literals. The templated SQL hits the
    cache; per-row work is a typed-substitute walk (~10 µs vs
    ~1 ms full parse). Bails on ON CONFLICT, INSERT … SELECT,
    SQL with existing `?` placeholders, and any non-templatable
    token shape — slow path stays correct.
  - **`now()` / `current_timestamp` family marker-ised** like the
    existing `nextval` marker so the cached parsed map doesn't
    bake a parse-time `Date`. Resolved per-execute; identity-
    tracked so the same marker appearing in multiple parts of
    tx-data resolves once per logical use.
  - **`describeParams` infers OIDs for column-less INSERTs** by
    falling back to `pgs/column-info`'s declared column order. Fixes
    pgjdbc's `executeBatch` with positional `INSERT INTO t VALUES
    (?, ?, ?)` (was raising `Can't change resolved type for param`).
- **Throughput at 1000 rows/connection:**
  - JDBC `PreparedStatement.executeBatch`: ~5 k r/s
  - Simple-Query multi-stmt (`psql -f`, `pg_dump` replay): ~4 k r/s
  - Explicit `BEGIN; INSERT*; COMMIT`: ~1.4 k r/s
  - Single-stmt-per-call (default JDBC): ~370 r/s (bound by
    per-call commit cost in Datahike).

### Migration & pg_dump interop

- **`dump` tool + CLI** — `datahike.pg.dump/dump` walks any Datahike
  database (SQL- or Datalog-created) and emits pg_dump-shaped SQL.
  Output replays into either pg-datahike or real PostgreSQL via
  `psql`. CLI: `java -jar pg-datahike.jar dump --data-dir DIR --db
  NAME [--out FILE] [--inserts|--copy] [--schema-only|--data-only]
  [--exclude-table NAME] [--config CONFIG.edn]`. The `--config`
  escape hatch reads a full Datahike config EDN, so any konserve
  backend (file, jdbc, s3, redis, lmdb, …) is reachable; store-id
  is auto-discovered from the persisted `:db` branch.
- **Native Datahike databases dump cleanly** — without any setup, a
  database created via `d/transact` exports as valid PG SQL:
  `:db.unique/identity` → `PRIMARY KEY`, `:db.unique/value` → `UNIQUE`,
  `:db.cardinality/many T` → `T[]` with PG array literals,
  `:db.type/ref` → `bigint` (entity-id). FK constraints opt-in via
  `set-hint!` `:datahike.pg/references`.
- **pg_dump-import via psql** — `pg_dump` output replays into pg-
  datahike with the new `:compat :pg-dump` preset. Coverage:
  `CREATE TABLE` with `DEFAULT nextval('s'::regclass)` (incl. schema-
  qualified seq names), `CREATE SEQUENCE … NO MINVALUE/MAXVALUE/
  CYCLE`, multi-row `INSERT`, `COPY … FROM stdin` (text + CSV),
  `CREATE TYPE … AS ENUM`, `CREATE DOMAIN`, partitioned tables
  (parent + children), `\restrict`/`\unrestrict` psql metacommands,
  `pg_catalog.set_config(...)`. Triggers, functions, materialized
  views, ALTER OWNER, ATTACH PARTITION are silently accepted under
  `:pg-dump`.
- **`:compat :pg-dump` preset** — superset of `:permissive` that
  bundles the per-feature reject-kinds pg_dump emits and we don't
  model: `:trigger :function :procedure :aggregate :rule :operator
  :cast :language :materialized-view :attach-partition :alter-type
  :alter-domain :type` (non-ENUM CREATE TYPE forms).
- **Validated round-trip** against real PostgreSQL: Chinook (15.6 k
  rows / 11 tables / FKs / NUMERIC / TIMESTAMP) byte-identical
  per-row equality at every leg; Pagila (50 k rows / 22 tables / ENUM
  / DOMAIN / partitioning / triggers / functions) schema and data
  load end-to-end.

### First-class type system additions

- **ENUM** — `CREATE TYPE … AS ENUM (…)` bypasses JSqlParser via a
  custom parser (`datahike.pg.sql.types`) and lands as a registry
  entity (`:datahike.pg.enum/{name,values,values-ordered}`). Columns
  declared with the enum lower to `:db.type/string` + a
  `:datahike.pg/enum-of` tag so the dump re-emits the column with
  the original enum type, not `text`.
- **DOMAIN** — `CREATE DOMAIN [name] AS [base] [CHECK (…)]`. Same
  registry-entity architecture (`:datahike.pg.domain/{name,base-type,
  check-expr,not-null,…}`). Column resolution lowers to the base
  type with `:datahike.pg/domain-of` for re-emission.
- **DOMAIN / ENUM runtime enforcement** — INSERTs into a DOMAIN- or
  ENUM-typed column are validated against the registry at txdb time:
  - DOMAIN CHECK violations raise `23514` ("value for domain X violates
    check constraint Y"). PG 3VL: NULL → unknown → satisfied.
  - DOMAIN NOT NULL raises `23502` ("domain X does not allow null
    values").
  - ENUM non-members raise `22P02` ("invalid input syntax for type
    {enum}"). NULL is allowed unless the column is also NOT NULL.
  Implementation reuses the existing `:db.fn/call` wrapper layered on
  INSERT tx-data — `apply-column-constraints` already runs at txdb-
  time for NOT NULL / CHECK / FK; we add a sibling pass for domain/
  enum. CHECK ASTs are pre-parsed at cache-build time, ENUM value-
  sets frozen, both memoised per (schema, table). Tables without
  domain- or enum-typed columns pay zero overhead. Pagila replay
  (which has both `year` DOMAIN with CHECK and `mpaa_rating` ENUM)
  stays at ~4 k rows/s.
- **`consume-name` parses quoted-identifier domain names** including
  `public."bıgınt"` (Turkish dotless-i in Pagila's schema), and the
  symmetric `"schema"."name"` form. The existing rule only matched
  bare-alphanumeric `schema.name`.

### `nextval` / sequence handling

- **`DEFAULT nextval('seq')` parses** — token-driven rewrite wraps
  `DEFAULT <fn>(…)` in extra parens for `nextval`/`currval`/`lastval`
  so JSqlParser accepts the form. Identical AST to the parenthesised
  form. Fixes `pg_dump`'s SERIAL/IDENTITY emit.
- **`nextval()` in INSERT VALUES resolved** — sibling-pass
  architecture in `params.clj`: tx-data flows through
  `substitute-params` (Bind-time) and then `resolve-nextvals!`
  (Execute-time, against the live conn). PG-correct non-transactional
  semantics: nextval advances stick across rollback, concurrent
  callers get distinct values via CAS-retry. `nextval!` core
  extracted from `handle-nextval` and shared by both call sites.
- **Schema-qualified sequence names** (`public.foo_id_seq`) accepted
  by `nextval`/`currval`/`setval` and by `DEFAULT nextval`.
- **`<table>_seq` no longer false-matches** as the IDENTITY sequence
  for table `<table>`. The matcher requires a non-empty `<col>`
  between prefix and suffix.

### Other server fixes

- `splitStatements` filters whitespace-only chunks (after stripComments
  turns trailing comments into spaces); `handleParse` (extended-query
  path) applies `stripComments` before JSqlParser — both fix trailing-
  comment handling.
- `translate-create-sequence` unquotes the sequence name (was storing
  literal quotes for `CREATE SEQUENCE "x"`, breaking subsequent
  `setval`).
- `:set-config` added to `system-result-metadata` — fixes "Received
  resultset tuples, but no field structure for them" error pgjdbc
  raised on `SELECT pg_catalog.set_config(...)` from pg_dump preludes.
- `database/tokenize` recognises multi-char operators (`>=`, `<=`,
  `<>`, `!=`, `||`) and single chars `<`/`>`/`!`/`~`/`^`/`|` (was
  silently dropping them in `:else`). DOMAIN CHECK round-trips
  correctly as a result.
- `parse-timestamp-string` accepts PG's `Y/M/d` slash-date format
  (used by Chinook's employee hire-dates).
- `string-value-text` helper reproduces PG's `N'...'` (national-
  character) trailing-space trimming for Chinook fidelity.
- Token rewrite rule `partition-by-rule` strips `PARTITION BY
  <strategy> (<col>)` from CREATE TABLE so partitioned tables parse.
- Token rewrite rule `create-sequence-no-clause-rule` strips
  `NO MINVALUE/MAXVALUE/CYCLE` two-token groups.
- Dump output preserves source column declaration order via
  `pgs/column-order-from-db`; composite-PK tuple attrs no longer
  emitted as phantom columns.

### Earlier in this branch (pre-Pagila work)

- Renamed from `pgwire-datahike` to `pg-datahike` — the project is a
  PostgreSQL adapter for Datahike, not just a wire-protocol server.
  Namespaces (`datahike.pg.*`) and the `PgWireServer` Java class are
  unchanged. Clojars coord: `org.replikativ/pg-datahike`.
- Extended Query RowDescription for system queries — pgjdbc's default
  `preferQueryMode=extended` now works for `current_database()`,
  `now()`, `version()`, advisory locks, `nextval`, etc.
- FK-via-ref JOIN rewrite: `JOIN c ON p.fk = c.pk` resolves correctly
  on native Datahike schemas where refs store target entity-ids.
- `:datahike.pg/*` schema hints (column rename, hidden attr, FK target,
  table rename) — let users customize the SQL view of a native
  Datahike database without DDL.
- Multi-DB registry: `start-server` accepts `{name → conn}`; clients
  route via the JDBC URL's database name, virtual `pg_database`
  catalog enumerates the registry, unknown names get 3D000.
- Initial extraction from datahike's `pg-server/` subtree.
- Public API facade `datahike.pg` (start-server, stop-server,
  make-query-handler, register-catalog-table!, unregister-catalog-table!,
  reset-lock-registry!, reset-advisory-locks!).
- Token-driven SQL classification (`datahike.pg.sql.classify`) — routes
  statements to the right handler before JSqlParser sees them.
- Structural SELECT shape matcher (`datahike.pg.sql.shape`) — identifies
  pgjdbc/Odoo catalog probes without substring matching.
- Token-driven source rewriter (`datahike.pg.sql.rewrite`) — inline
  REFERENCES stripping, CREATE INDEX anonymous-name injection,
  SELECT-FROM empty-projection injection.
- Constraint enforcement (NOT NULL, DEFAULT, CHECK, FK child-side +
  parent-side RESTRICT on DELETE and key-UPDATE).
- Extension seam for virtual catalog tables (`register-catalog-table!`).
- `:compat :permissive` / `:silently-accept` handler options for
  tolerating ORM-emitted no-op DDL (GRANT, REVOKE, POLICY, RLS,
  CREATE EXTENSION).
- Advisory-lock support (pg_advisory_lock, pg_try_advisory_lock,
  pg_advisory_xact_lock, pg_advisory_unlock, pg_advisory_unlock_all)
  with proper per-session / per-tx lifecycles — needed for every
  serious migration tool (Flyway, Alembic, Ecto, Rails, Liquibase).
- Session introspection (pg_backend_pid, txid_current, pg_sleep).
- SAVEPOINT / RELEASE / ROLLBACK TO with correct PG error codes
  (25P01 outside-tx, 3B001 missing-savepoint).
- Maintenance no-ops (VACUUM, REINDEX, CLUSTER, CREATE SCHEMA).
- `pg_extension` as an always-empty virtual table for framework feature
  probes.

### Integration

- pgjdbc ResultSetTest: 80/80 passing.
- Unit test suite: 703 tests / 1952 assertions.
- Real-PG round-trip: Chinook end-to-end with byte-identical per-row
  equality; Pagila schema + data load.
