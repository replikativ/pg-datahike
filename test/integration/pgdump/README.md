# pg_dump round-trip

Takes a **non-trivial** database (pagila: 15 tables, ~46k rows, views,
sequences, a partitioned table, bytea and timestamptz columns) through
PostgreSQL's own `pg_dump` and restores the result into pg-datahike.

## Why the dump is generated rather than vendored

The repo already carried a real `pg_dump` of pagila — but taken with
`--inserts`. The **default** `pg_dump` format is COPY, and nothing
exercised it, so two decoding bugs survived:

- `timestamptz` in PostgreSQL's output form —
  `2022-01-28 17:58:52.222594-08`, a space separator and an hour-only
  offset — was rejected 380 times with `invalid timestamp`;
- `bytea` hex (`\x1e3d…`) reached the transactor as a string.

A default-format restore loaded **zero rows**. Generating the dump here
means the suite tracks whatever `pg_dump` the CI image ships, so the
next format drift fails rather than waiting for someone to regenerate a
fixture.

## What it asserts

Per-table row counts must **match the source PostgreSQL**, plus no COPY
block may fail.

It deliberately does **not** assert "zero errors". pagila carries
triggers, functions, `ATTACH PARTITION`, a materialized view and an
aggregate, and a partitioned parent — all rejected on purpose, ~50 statements' worth. Asserting
zero would mean either faking them or disabling the suite. A *data*
error is different and is always a failure.

Partitioned parents are refused (`PARTITION BY` raises 0A000) and are
excluded from the comparison. pg_dump emits each partition as a plain
`CREATE TABLE` plus `ATTACH PARTITION` (also refused), so the rows land
in the partitions, and the partitions are compared like any other table.

## Running locally

    export PATH=/usr/lib/postgresql/17/bin:$PATH
    PGDUMP_SRC_HOST=/tmp/pgo2 PGDUMP_SRC_PORT=15998 \
    PGDUMP_SRC_USER=pgtest PGDUMP_SRC_DB=postgres \
      bash test/integration/pgdump/run.sh

Needs pg-datahike on `localhost:15432` and a source PostgreSQL. Outputs
`last-restore.log` (the restore's own errors) and
`pagila-default-format.sql` (the generated dump); both are kept as CI
artifacts because the console prints only counts.

## Schema and data export into native PostgreSQL

`schema-export.sh` checks the reverse direction with stock PostgreSQL 18
client binaries on `PATH`. It creates the same ordinary schema fixture in
pg-datahike and in native PostgreSQL, dumps the former, and restores it into
a second native database. It compares the restored schema, including the
default and nullability added by ALTER TABLE, and checks the view and
constraint behavior. It then exports rows via `COPY TO STDOUT` and checks
that native `COPY FROM STDIN` restores the same values, including escaped
text and a NULL foreign key.

PostgreSQL 18 `pg_dump` queries `pg_get_sequence_data` for dumps that
include data. That function and sequence-state round-tripping remain open.

Set `PG_EXPORT_SOURCE`, `PG_EXPORT_ORACLE`, and `PG_EXPORT_RESTORE` to three
empty disposable database URLs. Use an 18.6 candidate as the source and make
the pgvector extension available to the restore server. Set
`PG_EXPORT_RESTORE_OPTIONS` if the extension lives in a test directory on
that server. Run `bash test/integration/pgdump/schema-export.sh` from the
repository root. The script prints the path of the dump and comparison
artifacts.
