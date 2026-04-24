#!/usr/bin/env python3
"""
SQLAlchemy integration test for Datahike PgWire server.

Tests real ORM patterns: table creation, CRUD, relationships, aggregates,
transactions with rollback, and prepared statement parameters.

Prerequisites:
  pip install sqlalchemy psycopg2-binary

Usage:
  1. Start the PgWire server: clj -A:test -M test/integration/start_pgwire.clj
  2. Run this script: python3 test/integration/test_sqlalchemy.py

Expected: all tests pass or report specific failures for unsupported features.
"""

import sys
import traceback
from sqlalchemy import (
    create_engine, Column, Integer, String, Float, ForeignKey,
    func, text, select, inspect
)
from sqlalchemy.orm import (
    DeclarativeBase, Session, relationship, selectinload, joinedload
)

# Import Datahike dialect (registers "datahike" URL scheme)
import os
sys.path.insert(0, os.path.dirname(__file__))
import datahike_dialect  # noqa: F401

# Connect to Datahike PgWire using the custom dialect
ENGINE_URL = "datahike+psycopg2://datahike:@127.0.0.1:15432/datahike"

class Base(DeclarativeBase):
    pass

class Department(Base):
    __tablename__ = "department"
    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    budget = Column(Float)
    employees = relationship("Employee", back_populates="department")

class Employee(Base):
    __tablename__ = "employee"
    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    salary = Column(Float)
    dept_id = Column(Integer, ForeignKey("department.id"))
    department = relationship("Department", back_populates="employees")

# Test results tracking
passed = 0
failed = 0
errors = []

def test(name, fn):
    global passed, failed, errors
    try:
        fn()
        passed += 1
        print(f"  PASS: {name}")
    except Exception as e:
        failed += 1
        errors.append((name, str(e)))
        print(f"  FAIL: {name}: {e}")
        if "--verbose" in sys.argv:
            traceback.print_exc()

def run_tests():
    global passed, failed, errors

    engine = create_engine(ENGINE_URL, echo=("--echo" in sys.argv))

    print("\n=== Phase 1: DDL — Create Tables ===")

    def test_create_tables():
        Base.metadata.create_all(engine)
    test("CREATE TABLE via ORM metadata", test_create_tables)

    print("\n=== Phase 2: Basic CRUD ===")

    def test_insert():
        with Session(engine) as session:
            eng = Department(id=1, name="Engineering", budget=500000.0)
            sales = Department(id=2, name="Sales", budget=200000.0)
            session.add_all([eng, sales])
            session.commit()

            session.add_all([
                Employee(id=1, name="Alice", salary=90000.0, dept_id=1),
                Employee(id=2, name="Bob", salary=80000.0, dept_id=1),
                Employee(id=3, name="Carol", salary=70000.0, dept_id=2),
            ])
            session.commit()
    test("INSERT via session.add + commit", test_insert)

    def test_select_all():
        with Session(engine) as session:
            emps = session.execute(select(Employee)).scalars().all()
            assert len(emps) == 3, f"Expected 3 employees, got {len(emps)}"
    test("SELECT all employees", test_select_all)

    def test_select_filter():
        with Session(engine) as session:
            alice = session.execute(
                select(Employee).where(Employee.name == "Alice")
            ).scalar_one()
            assert alice.name == "Alice"
            assert alice.salary == 90000.0
    test("SELECT with WHERE filter (parameterized)", test_select_filter)

    def test_update():
        with Session(engine) as session:
            alice = session.execute(
                select(Employee).where(Employee.name == "Alice")
            ).scalar_one()
            alice.salary = 95000.0
            session.commit()

        with Session(engine) as session:
            alice = session.execute(
                select(Employee).where(Employee.name == "Alice")
            ).scalar_one()
            assert alice.salary == 95000.0, f"Expected 95000, got {alice.salary}"
    test("UPDATE via attribute assignment + commit", test_update)

    def test_delete():
        with Session(engine) as session:
            carol = session.execute(
                select(Employee).where(Employee.name == "Carol")
            ).scalar_one()
            session.delete(carol)
            session.commit()

        with Session(engine) as session:
            remaining = session.execute(select(Employee)).scalars().all()
            assert len(remaining) == 2, f"Expected 2 after delete, got {len(remaining)}"
    test("DELETE via session.delete + commit", test_delete)

    print("\n=== Phase 3: Relationships ===")

    def test_join_load():
        with Session(engine) as session:
            # joinedload: single query with LEFT JOIN
            emps = session.execute(
                select(Employee).options(joinedload(Employee.department))
            ).unique().scalars().all()
            for emp in emps:
                assert emp.department is not None, f"{emp.name} has no department"
                assert emp.department.name in ("Engineering", "Sales")
    test("Relationship via joinedload (LEFT JOIN)", test_join_load)

    def test_selectin_load():
        with Session(engine) as session:
            # selectinload: two queries (SELECT ... WHERE id IN (...))
            depts = session.execute(
                select(Department).options(selectinload(Department.employees))
            ).scalars().all()
            eng = next(d for d in depts if d.name == "Engineering")
            assert len(eng.employees) >= 1
    test("Relationship via selectinload (IN subquery)", test_selectin_load)

    print("\n=== Phase 4: Aggregates ===")

    def test_count():
        with Session(engine) as session:
            count = session.execute(
                select(func.count(Employee.id))
            ).scalar()
            assert count == 2, f"Expected 2, got {count}"
    test("COUNT(*)", test_count)

    def test_sum_group():
        with Session(engine) as session:
            results = session.execute(
                select(Department.name, func.sum(Employee.salary))
                .join(Employee, Employee.dept_id == Department.id)
                .group_by(Department.name)
            ).all()
            assert len(results) >= 1
    test("SUM + GROUP BY + JOIN", test_sum_group)

    print("\n=== Phase 5: Transactions ===")

    def test_rollback():
        with Session(engine) as session:
            session.begin()
            session.execute(
                text("INSERT INTO employee (id, name, salary, dept_id) VALUES (99, 'Temp', 10000, 1)")
            )
            session.rollback()

        with Session(engine) as session:
            temp = session.execute(
                select(Employee).where(Employee.name == "Temp")
            ).scalars().all()
            assert len(temp) == 0, f"Expected 0 after rollback, got {len(temp)}"
    test("ROLLBACK discards INSERT", test_rollback)

    def test_commit_persist():
        with Session(engine) as session:
            session.begin()
            session.execute(
                text("INSERT INTO employee (id, name, salary, dept_id) VALUES (100, 'Persist', 20000, 2)")
            )
            session.commit()

        with Session(engine) as session:
            p = session.execute(
                select(Employee).where(Employee.name == "Persist")
            ).scalars().all()
            assert len(p) == 1, f"Expected 1 after commit, got {len(p)}"
    test("COMMIT persists INSERT", test_commit_persist)

    print("\n=== Phase 6: Raw SQL ===")

    def test_raw_parameterized():
        with Session(engine) as session:
            result = session.execute(
                text("SELECT name FROM employee WHERE salary > :threshold"),
                {"threshold": 50000}
            ).all()
            assert len(result) >= 1
    test("Raw parameterized query (:name params)", test_raw_parameterized)

    def test_order_limit():
        with Session(engine) as session:
            result = session.execute(
                text("SELECT name FROM employee ORDER BY salary DESC LIMIT 1")
            ).scalar()
            assert result is not None
    test("ORDER BY + LIMIT", test_order_limit)

    print("\n=== Phase 7: Schema Introspection ===")

    def test_inspect_tables():
        insp = inspect(engine)
        tables = insp.get_table_names()
        assert "employee" in tables, f"employee not in {tables}"
        assert "department" in tables, f"department not in {tables}"
    test("Inspector.get_table_names()", test_inspect_tables)

    def test_inspect_columns():
        insp = inspect(engine)
        cols = insp.get_columns("employee")
        col_names = [c["name"] for c in cols]
        assert "name" in col_names, f"name not in {col_names}"
    test("Inspector.get_columns('employee')", test_inspect_columns)

    # Summary
    print(f"\n{'='*50}")
    print(f"Results: {passed} passed, {failed} failed")
    if errors:
        print("\nFailed tests:")
        for name, err in errors:
            print(f"  - {name}: {err}")
    print(f"{'='*50}")

    return failed == 0

if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)
