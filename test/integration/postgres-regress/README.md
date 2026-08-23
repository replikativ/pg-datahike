# PostgreSQL regression baseline

This harness runs PostgreSQL's own `pg_regress` driver and unmodified
regression SQL against an existing pg-datahike server. It is a compatibility
baseline and triage tool, not an all-green gate.

Start pg-datahike on port 15432, then run one or more named tests:

```bash
bb pg-regress jsonb
bb pg-regress jsonb expressions
```

The default PostgreSQL source checkout is `../postgres`. Override paths and
the target endpoint when needed:

```bash
POSTGRES_SOURCE=/path/to/postgres \
PG_REGRESS_PORT=15436 \
PG_REGRESS_DB=datahike \
bb pg-regress jsonb
```

The script uses PostgreSQL 17's installed `pg_regress` and `psql` by default.
Override them with `PG_REGRESS_BIN`, `PG_REGRESS_BINDIR`, or
`PG_REGRESS_MAJOR`.

Artifacts are written below `.internal/pg-regress/`: PostgreSQL's complete
result output, unified diff, and summary. A normal mismatch (`pg_regress`
status 1) is reported but does not fail the task. Harness failures remain
fatal. Set `PG_REGRESS_STRICT=1` when an admitted test is expected to be fully
green and should gate on any diff.

## Reading the baseline

The raw diff includes harmless differences such as shorter error diagnostics,
alongside unsupported features and genuine wrong answers. The runner therefore
also prints:

- total diff lines as a coarse trend;
- the most frequent target-side errors;
- internal-looking signatures such as class casts, unknown Datalog variables,
  and lost connections.

Do not reduce the diff count by merely matching diagnostic wording. Promote
high-value behavior into focused unit or SQLLogic tests, and treat silent wrong
answers and internal failures ahead of explicitly unsupported surface.
