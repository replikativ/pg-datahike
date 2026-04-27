# Odoo ↔ pg-datahike conformance harness

End-to-end test that boots Odoo against pg-datahike, runs `--init=base`
to load the core module, and runs the `TestORM` suite from
`addons/base/tests/test_orm.py`. This is the highest-fidelity proof
that the catalog, DDL, locking, and transactional surface all work for
a real PostgreSQL ORM.

## Layout

```
odoo/
  setup.sh           # one-time: pip install Odoo's requirements.txt + plugins
  run.sh             # boot pgwire + run odoo init/tests
  odoo.conf          # generated config (gitignored)
  expected-skips.md  # what's covered / known failures
  README.md          # this file
  last-run.log       # (gitignored) odoo + pgwire stdout for triage
```

## Prerequisites

- A clone of Odoo 19.0 at `$ODOO_ROOT` (default
  `/home/christian-weilbach/Development/odoo`). The repo's `.venv`
  with Odoo's `requirements.txt` installed is used by default; set
  `ODOO_VENV` to override.
- Python 3.12 in the venv.
- `clojure` on PATH (so the harness can boot pgwire).

The harness does **not** require a real PostgreSQL — it boots
pg-datahike on `:15432` and points Odoo at it.

## Running

```
./setup.sh    # one-time: install Odoo's pip requirements into the venv
./run.sh      # ~5 min wall-clock — JIT, init, TestORM
```

`run.sh` exits 0 when the assertion holds (≥ N passing TestORM cases,
where N is set per the prior baseline). On failure, the tail of
`last-run.log` is dumped to stderr.

## What the harness asserts

A passing run proves that:

1. pgwire accepts Odoo's connection (auth, version, encoding).
2. `--init=base` succeeds — Odoo creates its bootstrap schema
   (~600 tables, including `ir_module_module`, `res_partner`, etc.)
   over wire DDL.
3. The `TestORM` test class runs to completion. Per the prior baseline
   from `feature/postgres-wire-protocol`'s exploratory work, **9/11
   passing** is the floor; below that is a regression.

The 2 known-failing tests are tracked in `expected-skips.md`.

## Re-running after pg-datahike changes

Subsequent `./run.sh` invocations are JVM + Python warmup bound. Logs
accumulate in `last-run.log` (truncated at start of each run).

If you change `setup.sh`, run it once before `run.sh`.
