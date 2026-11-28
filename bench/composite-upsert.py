"""Emit a psql workload for Odoo-style composite-key INSERT and replay."""

import argparse

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--rows", type=int, default=1000)
args = parser.parse_args()
if args.rows < 1:
    parser.error("--rows must be positive")

values = ",\n".join(
    f"('base','state_{i}','res.country.state',{i},false)"
    for i in range(1, args.rows + 1)
)
statement = f"""INSERT INTO upsert_bench (module,name,model,res_id,noupdate)
VALUES {values}
ON CONFLICT (module,name) DO UPDATE SET (model,res_id,write_date) =
  (excluded.model,excluded.res_id,now() AT TIME ZONE 'UTC')
WHERE upsert_bench.res_id != excluded.res_id OR upsert_bench.model != excluded.model
RETURNING module,name,model,res_id,create_date,write_date;"""

print(r"""\set ON_ERROR_STOP on
CREATE TABLE upsert_bench (module text, name text, model text, res_id integer,
                           noupdate boolean, create_date timestamp, write_date timestamp);
CREATE UNIQUE INDEX upsert_bench_key ON upsert_bench (module,name);
\timing on
\o /dev/null
\echo initial insert""")
print(statement)
print(r"\echo unchanged replay")
print(statement)
print(r"""\o
\timing off
SELECT count(*) AS stored_rows FROM upsert_bench;
DROP TABLE upsert_bench;""")
