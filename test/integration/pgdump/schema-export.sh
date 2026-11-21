#!/usr/bin/env bash
# Stock pg_dump from pg-datahike, followed by a native PostgreSQL restore.
set -euo pipefail

: "${PG_EXPORT_SOURCE:?Set PG_EXPORT_SOURCE to a disposable pg-datahike database URL}"
: "${PG_EXPORT_ORACLE:?Set PG_EXPORT_ORACLE to a disposable native PostgreSQL database URL}"
: "${PG_EXPORT_RESTORE:?Set PG_EXPORT_RESTORE to a different empty native PostgreSQL database URL}"
if [[ "$PG_EXPORT_SOURCE" == "$PG_EXPORT_ORACLE" ||
      "$PG_EXPORT_SOURCE" == "$PG_EXPORT_RESTORE" ||
      "$PG_EXPORT_ORACLE" == "$PG_EXPORT_RESTORE" ]]; then
  echo 'Use three distinct disposable database URLs.' >&2
  exit 2
fi

artifacts=${PG_EXPORT_ARTIFACTS:-$(mktemp -d)}
mkdir -p "$artifacts"
echo "Schema export artifacts are in $artifacts"

run_psql() {
  psql -X -v ON_ERROR_STOP=1 -d "$@"
}

cat > "$artifacts/fixture.sql" <<'SQL'
CREATE TABLE client_parent (id int PRIMARY KEY, name text NOT NULL);
CREATE TABLE client_child (id int PRIMARY KEY, parent_id int REFERENCES client_parent, amount int CHECK (amount > 0));
CREATE INDEX client_child_amount_idx ON client_child (amount);
CREATE VIEW client_view AS SELECT id FROM client_child;
SQL

cat > "$artifacts/schema.sql" <<'SQL'
SELECT c.relname, c.relkind, a.attnum, a.attname,
       pg_catalog.format_type(a.atttypid,a.atttypmod), a.attnotnull
  FROM pg_class c
  JOIN pg_namespace n ON n.oid = c.relnamespace
  JOIN pg_attribute a ON c.oid = a.attrelid
 WHERE n.nspname = 'public'
   AND c.relname IN ('client_parent','client_child','client_view')
   AND a.attnum > 0 AND NOT a.attisdropped
 ORDER BY c.relname,a.attnum;

SELECT c.relname, p.conname, p.contype, pg_catalog.pg_get_constraintdef(p.oid)
  FROM pg_constraint p JOIN pg_class c ON p.conrelid = c.oid
 WHERE c.relname IN ('client_parent','client_child')
 ORDER BY c.relname,p.conname;

SELECT i.relname, pg_catalog.pg_get_indexdef(i.oid)
  FROM pg_index x
  JOIN pg_class t ON t.oid = x.indrelid
  JOIN pg_class i ON i.oid = x.indexrelid
 WHERE t.relname IN ('client_parent','client_child')
 ORDER BY i.relname;

SELECT pg_catalog.pg_get_viewdef('public.client_view'::regclass);
SQL

cat > "$artifacts/behavior.sql" <<'SQL'
BEGIN;
INSERT INTO client_parent VALUES (1, 'parent');
INSERT INTO client_child VALUES (2, 1, 10);
SELECT * FROM client_view;
DO $$
BEGIN
  BEGIN
    INSERT INTO client_parent VALUES (1, 'second parent');
    RAISE EXCEPTION 'restored primary key failed to reject a duplicate';
  EXCEPTION WHEN unique_violation THEN NULL;
  END;
  BEGIN
    INSERT INTO client_parent VALUES (3, NULL);
    RAISE EXCEPTION 'restored NOT NULL failed to reject NULL';
  EXCEPTION WHEN not_null_violation THEN NULL;
  END;
  BEGIN
    INSERT INTO client_child VALUES (4, 404, 10);
    RAISE EXCEPTION 'restored foreign key failed to reject a missing parent';
  EXCEPTION WHEN foreign_key_violation THEN NULL;
  END;
  BEGIN
    INSERT INTO client_child VALUES (5, 1, -1);
    RAISE EXCEPTION 'restored check failed to reject a negative amount';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
END $$;
ROLLBACK;
SQL

run_psql "$PG_EXPORT_SOURCE" -q -f "$artifacts/fixture.sql"
run_psql "$PG_EXPORT_ORACLE" -q -f "$artifacts/fixture.sql"
pg_dump --dbname="$PG_EXPORT_SOURCE" --schema-only --no-owner --no-privileges \
  --file="$artifacts/schema-dump.sql"
PGOPTIONS=${PG_EXPORT_RESTORE_OPTIONS:-${PGOPTIONS:-}} \
  run_psql "$PG_EXPORT_RESTORE" -q -f "$artifacts/schema-dump.sql" \
  > "$artifacts/restore.log"

run_psql "$PG_EXPORT_ORACLE" -Atq -f "$artifacts/schema.sql" > "$artifacts/oracle-schema.txt"
run_psql "$PG_EXPORT_RESTORE" -Atq -f "$artifacts/schema.sql" > "$artifacts/restored-schema.txt"
diff -u "$artifacts/oracle-schema.txt" "$artifacts/restored-schema.txt"

run_psql "$PG_EXPORT_RESTORE" -Atq -f "$artifacts/behavior.sql" > "$artifacts/behavior.txt"
printf '2\n' | diff -u - "$artifacts/behavior.txt"
echo 'Schema, view, and constraint checks passed.'
