#!/usr/bin/env bash
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
echo "Data export artifacts are in $artifacts"

cat > "$artifacts/fixture.sql" <<'SQL'
CREATE SEQUENCE quiet_seq START 10 INCREMENT 3;
CREATE SEQUENCE advanced_seq AS integer START 20 INCREMENT 2;
CREATE SEQUENCE reset_seq START 2 INCREMENT 5;
SELECT nextval('advanced_seq');
SELECT setval('reset_seq', 42, false);
CREATE TABLE export_parent (id integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY, label text);
CREATE TABLE export_child (id serial PRIMARY KEY, parent_id int REFERENCES export_parent, label text);
INSERT INTO export_parent (label) VALUES ('first'), ('second');
INSERT INTO export_child (parent_id, label) VALUES
  (1, 'a' || chr(9) || chr(10) || 'b'), (NULL, NULL);
SQL

cat > "$artifacts/check.sql" <<'SQL'
SELECT id, label FROM export_parent ORDER BY id;
SELECT id, parent_id, encode(convert_to(label, 'UTF8'), 'hex') FROM export_child ORDER BY id;
SELECT c.relname, format_type(s.seqtypid, NULL), s.seqstart, s.seqincrement,
       s.seqmin, s.seqmax, s.seqcache, s.seqcycle, d.last_value, d.is_called
  FROM pg_class c JOIN pg_sequence s ON c.oid = s.seqrelid,
       pg_get_sequence_data(s.seqrelid) d
  ORDER BY c.relname;
SELECT table_name, column_name, is_identity, identity_generation
  FROM information_schema.columns
  WHERE table_name IN ('export_parent', 'export_child') AND column_name = 'id'
  ORDER BY table_name;
SELECT pg_get_serial_sequence('export_parent', 'id'), pg_get_serial_sequence('export_child', 'id');
SELECT nextval('quiet_seq'), nextval('advanced_seq'), nextval('reset_seq');
INSERT INTO export_parent (label) VALUES ('new parent') RETURNING id;
INSERT INTO export_child (parent_id, label) VALUES (3, 'new child') RETURNING id;
SQL

psql -X -q -v ON_ERROR_STOP=1 -d "$PG_EXPORT_SOURCE" -f "$artifacts/fixture.sql" > "$artifacts/source-setup.log"
psql -X -q -v ON_ERROR_STOP=1 -d "$PG_EXPORT_ORACLE" -f "$artifacts/fixture.sql" > "$artifacts/oracle-setup.log"
pg_dump --dbname="$PG_EXPORT_SOURCE" --no-owner --no-privileges --file="$artifacts/full-dump.sql"
PGOPTIONS=${PG_EXPORT_RESTORE_OPTIONS:-${PGOPTIONS:-}} \
  psql -X -q -v ON_ERROR_STOP=1 -d "$PG_EXPORT_RESTORE" -f "$artifacts/full-dump.sql" > "$artifacts/restore.log"

psql -X -Atq -v ON_ERROR_STOP=1 -d "$PG_EXPORT_ORACLE" -f "$artifacts/check.sql" > "$artifacts/oracle.txt"
psql -X -Atq -v ON_ERROR_STOP=1 -d "$PG_EXPORT_RESTORE" -f "$artifacts/check.sql" > "$artifacts/restored.txt"
diff -u "$artifacts/oracle.txt" "$artifacts/restored.txt"
echo 'Stock pg_dump restored rows, sequence state, and subsequent generated values.'
