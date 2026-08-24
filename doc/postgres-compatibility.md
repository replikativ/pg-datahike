# PostgreSQL compatibility map

The compatibility target is PostgreSQL's application-facing SQL and wire
boundary, not its storage engine, planner implementation, administration,
procedural languages, or replication machinery. Upstream regression files are
discovery corpora first; a file becomes a strict gate only after its required
surface has been admitted.

## Source-to-test map

| Boundary | PostgreSQL authority | Upstream regression corpus | pg-datahike coverage |
|---|---|---|---|
| Expression parsing, parameter typing, and lowering | `src/backend/parser/parse_expr.c`, `parse_oper.c`, `parse_coerce.c`; `src/include/catalog/pg_proc.dat`, `pg_operator.dat` | `expressions.sql`, focused statements in type suites | expression/OID tests and extended-query pgjdbc tests |
| Set-returning functions | overloads in `pg_proc.dat`; integer implementations in `src/backend/utils/adt/int.c` | `rangefuncs.sql` | SRF, lateral-SRF, and extended-query tests |
| Type lookup and casts | `src/backend/parser/parse_type.c`, `parse_coerce.c`; `src/include/catalog/pg_type.dat` | per-type suites and `type_sanity.sql` | type-resolution, cast, enum/domain, and DDL-fidelity tests |
| Arrays and array input | `src/backend/utils/adt/arrayfuncs.c` (`array_in`, `ReadArrayStr`) | `arrays.sql` plus each element type's suite | array codec, SQL array, and array-column tests |
| Catalog row descriptions | catalog headers such as `pg_type.h`, `pg_attribute.h`, `pg_class.h` | `type_sanity.sql` and driver metadata queries | catalog tests plus pgjdbc/ORM probes |
| Relation resolution and errors | `src/backend/parser/parse_relation.c` | broadly exercised across the suite | unknown-table/column and catalog tests |
| EXPLAIN grammar/API | `src/backend/parser/gram.y`, `src/backend/commands/explain.c` | `explain.sql` | accepted-option and unsupported-feature tests |
| `money` | `src/backend/utils/adt/cash.c`; money entries in `pg_type.dat`, `pg_cast.dat`, and `pg_operator.dat` | `money.sql` | core OID/cast/DDL tests; full suite not admitted |

## Issues 86–95 audit

| Issue | Root boundary | PostgreSQL reference | Status |
|---|---|---|---|
| #86 unary minus over a declared parameter | Parse-message types must feed expression lowering | `parse_expr.c`, `exprType`/parameter analysis | covered by declared-OID parse binding |
| #87 bitwise operator on NULL | built-in operators are strict unless catalogued otherwise | integer functions and `pg_proc.proisstrict` | covered for all supported bitwise operations |
| #88 `money` result type | a distinct fixed-scale type, OID 790 | `cash.c`, `pg_type.dat`, `money.sql` | core type admitted; operators, locale input/output, and binary codec remain |
| #89 `pg_type.typname` metadata | catalog fields declared as `NameData` use OID 19 | catalog headers | covered systematically for all materialised NameData fields |
| #90 `pg_typeof($1)` | declared parameter OID participates in parse analysis | `pg_proc.dat` (`any -> regtype`) | covered over extended query protocol |
| #91 parameterised `generate_series` | overload resolution at Parse, cardinality after Bind | `pg_proc.dat`, `rangefuncs.sql` | int4 target-list form covered over extended query protocol |
| #92 UUID-array whitespace | whitespace outside an item is syntax, inside quotes is data | `arrayfuncs.c` | covered in codec and exact live query |
| #93 `EXPLAIN (COSTS OFF)` | parenthesised utility options in grammar | `gram.y`, `explain.sql` | fixed before this audit |
| #94 `pg_catalog.pg_databases` | only real relations are catalog relations; missing RTE is 42P01 | `parse_relation.c`, singular `pg_database.h` | covered with 42P01 |
| #95 cast to a nonexistent type | TypeName lookup fails during parse analysis | `parse_type.c` | covered with 42704 while preserving registered enum/domain/composite types |

## Regression-suite baseline (2026-08-24)

The following unmodified upstream files were run independently against a clean
server. None is a strict slice yet:

| Test | Target errors | Internal signatures | Dominant prerequisite |
|---|---:|---:|---|
| `expressions` | 38 | 0 | unsupported point/CREATE FUNCTION causes 35 aborted-transaction cascades |
| `arrays` | 184 | 0 | missing array functions, vector types, fixtures, and parser forms |
| `money` | 21 | 8 | locale-aware input plus money operator/function lowering |
| `rangefuncs` | 403 | 1 | CREATE FUNCTION and fixture dependencies cause 344 aborted cascades |
| `explain` | 40 | 0 | suite helper function and unsupported EXPLAIN forms |

Raw counts are not a progress metric: transaction cascades and missing suite
fixtures dominate them. The useful order is:

1. Remove every internal failure or silent wrong answer from a selected file.
2. Extract dependency-light statements into focused differential tests.
3. Bootstrap required API fixtures or classify PostgreSQL-only prerequisites.
4. Admit coherent subsets as strict `pg_regress` gates.
5. Expand from scalar expressions/types into relational queries, then catalog
   and protocol behavior used by actual drivers.

The next high-value slices from this baseline are the eight `money.sql`
class-cast failures, the single `rangefuncs.sql` unknown-variable failure, and
dependency-light statements from `arrays.sql`. The larger CREATE FUNCTION and
PostgreSQL-only type dependencies should be classified before implementation;
they should not drive compatibility shims by accident.
