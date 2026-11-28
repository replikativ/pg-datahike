"""Measure Odoo catalog probes against a disposable database using libpq settings."""

import statistics
import time
from collections import defaultdict

import psycopg2


QUERIES = {
    "columns": """
        SELECT column_name, udt_name, character_maximum_length, is_nullable
        FROM information_schema.columns
        WHERE table_name=%s AND table_schema=current_schema""",
    "constraint": """
        SELECT COALESCE(d.description, pg_get_constraintdef(c.oid))
        FROM pg_constraint c JOIN pg_class t ON t.oid=c.conrelid
        LEFT JOIN pg_description d ON c.oid=d.objoid
        WHERE t.relname=%s AND conname=%s AND t.relnamespace=current_schema::regnamespace""",
    "relation": """
        SELECT c.relname FROM pg_class c WHERE c.relname IN (%s)
        AND c.relkind IN ('r','v','m') AND c.relnamespace=current_schema::regnamespace""",
}


def run(conn):
    samples = defaultdict(list)
    with conn.cursor() as cursor:
        for i in range(64):
            cursor.execute(
                f"CREATE TABLE bench_{i} (id int, name varchar(40) NOT NULL, enabled bool, "
                f"CONSTRAINT bench_{i}_key UNIQUE (id,name))"
            )
        conn.commit()
        for i in range(15):
            phase = "warm" if i < 9 else "after_ddl"
            if phase == "after_ddl":
                cursor.execute(f"CREATE TABLE fresh_{i} (x int)")
            table = f"bench_{i % 3}"
            for name, sql in QUERIES.items():
                args = (table, table + "_key") if name == "constraint" else (table,)
                start = time.perf_counter()
                cursor.execute(sql, args)
                rows = sorted(cursor.fetchall())
                elapsed_ms = (time.perf_counter() - start) * 1000
                if i == 0:
                    print(name, rows)
                if i >= 3:
                    samples[phase, name].append(elapsed_ms)
        conn.rollback()
    for (phase, name), values in samples.items():
        print(phase, name, "median_ms", round(statistics.median(values), 3),
              "samples_ms", [round(value, 2) for value in values])


if __name__ == "__main__":
    conn = psycopg2.connect("")
    try:
        run(conn)
    finally:
        conn.close()
