package io.replikativ.test;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.cfg.Configuration;
import org.junit.jupiter.api.*;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for Datahike's PostgreSQL wire protocol with Hibernate ORM.
 *
 * Prerequisites:
 *   1. Start PgWire server: clj -A:test -M test/integration/start_pgwire.clj
 *   2. Run: cd test/integration/hibernate-app && mvn test
 *
 * Tests are ordered to build up state progressively.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class DatahikeHibernateTest {

    private static SessionFactory sessionFactory;

    @BeforeAll
    static void setup() {
        try {
            sessionFactory = new Configuration()
                .configure("hibernate.cfg.xml")
                .buildSessionFactory();
            System.out.println("SessionFactory created successfully");
        } catch (Exception e) {
            System.err.println("SessionFactory creation failed: " + e.getMessage());
            e.printStackTrace();
            throw e;
        }
    }

    @AfterAll
    static void teardown() {
        if (sessionFactory != null) {
            sessionFactory.close();
        }
    }

    // ========================================================================
    // Phase 1: DDL — Schema creation (hbm2ddl=create)
    // ========================================================================

    @Test
    @Order(1)
    void testSchemaCreated() {
        // If we got here, hbm2ddl.auto=create worked — tables were created.
        // Hibernate sent: CREATE SEQUENCE, CREATE TABLE, etc.
        assertNotNull(sessionFactory, "SessionFactory should be created");
    }

    // ========================================================================
    // Phase 2: Basic CRUD
    // ========================================================================

    @Test
    @Order(2)
    void testInsert() {
        try (Session session = sessionFactory.openSession()) {
            Transaction tx = session.beginTransaction();

            Department eng = new Department("Engineering", 500000.0);
            Department sales = new Department("Sales", 200000.0);
            session.persist(eng);
            session.persist(sales);

            Employee alice = new Employee("Alice", 90000.0);
            Employee bob = new Employee("Bob", 80000.0);
            Employee carol = new Employee("Carol", 70000.0);
            eng.addEmployee(alice);
            eng.addEmployee(bob);
            sales.addEmployee(carol);
            session.persist(alice);
            session.persist(bob);
            session.persist(carol);

            tx.commit();

            assertNotNull(eng.getId(), "Department should have generated ID");
            assertNotNull(alice.getId(), "Employee should have generated ID");
            System.out.println("Inserted: " + eng + ", " + alice);
        }
    }

    @Test
    @Order(3)
    void testSelectById() {
        try (Session session = sessionFactory.openSession()) {
            // Hibernate generates: SELECT e.id, e.name, ... FROM employee e WHERE e.id=$1
            Employee emp = session.find(Employee.class, 1L);
            // May be null if ID doesn't match — depends on sequence starting value
            // Just verify no exception
            System.out.println("Find by ID: " + emp);
        }
    }

    @Test
    @Order(4)
    void testSelectAll() {
        try (Session session = sessionFactory.openSession()) {
            List<Employee> employees = session.createQuery(
                "FROM Employee", Employee.class
            ).getResultList();
            assertEquals(3, employees.size(), "Should have 3 employees");
            System.out.println("All employees: " + employees);
        }
    }

    @Test
    @Order(5)
    void testSelectWithWhere() {
        try (Session session = sessionFactory.openSession()) {
            List<Employee> result = session.createQuery(
                "FROM Employee e WHERE e.salary > :minSalary", Employee.class
            ).setParameter("minSalary", 75000.0).getResultList();
            assertEquals(2, result.size(), "Should have 2 employees with salary > 75000");
        }
    }

    @Test
    @Order(6)
    void testUpdate() {
        try (Session session = sessionFactory.openSession()) {
            Transaction tx = session.beginTransaction();
            List<Employee> alices = session.createQuery(
                "FROM Employee e WHERE e.name = :name", Employee.class
            ).setParameter("name", "Alice").getResultList();
            assertFalse(alices.isEmpty(), "Alice should exist");
            Employee alice = alices.get(0);
            alice.setSalary(95000.0);
            tx.commit();
        }

        // Verify update persisted
        try (Session session = sessionFactory.openSession()) {
            List<Employee> alices = session.createQuery(
                "FROM Employee e WHERE e.name = :name", Employee.class
            ).setParameter("name", "Alice").getResultList();
            assertEquals(95000.0, alices.get(0).getSalary(), 0.01);
        }
    }

    @Test
    @Order(7)
    void testDelete() {
        try (Session session = sessionFactory.openSession()) {
            Transaction tx = session.beginTransaction();
            List<Employee> carols = session.createQuery(
                "FROM Employee e WHERE e.name = :name", Employee.class
            ).setParameter("name", "Carol").getResultList();
            if (!carols.isEmpty()) {
                session.remove(carols.get(0));
            }
            tx.commit();
        }

        try (Session session = sessionFactory.openSession()) {
            List<Employee> all = session.createQuery(
                "FROM Employee", Employee.class
            ).getResultList();
            assertEquals(2, all.size(), "Should have 2 employees after delete");
        }
    }

    // ========================================================================
    // Phase 3: Relationships
    // ========================================================================

    @Test
    @Order(8)
    void testEagerJoinFetch() {
        try (Session session = sessionFactory.openSession()) {
            // Hibernate generates: SELECT ... FROM employee e LEFT JOIN department d ON d.id=e.department_id
            List<Employee> employees = session.createQuery(
                "FROM Employee e JOIN FETCH e.department", Employee.class
            ).getResultList();
            assertFalse(employees.isEmpty());
            for (Employee emp : employees) {
                assertNotNull(emp.getDepartment(), emp.getName() + " should have department");
                System.out.println(emp.getName() + " → " + emp.getDepartment().getName());
            }
        }
    }

    @Test
    @Order(9)
    void testLazyLoading() {
        try (Session session = sessionFactory.openSession()) {
            Department dept = session.createQuery(
                "FROM Department d WHERE d.name = :name", Department.class
            ).setParameter("name", "Engineering").getSingleResult();
            // Accessing employees triggers lazy load (separate SELECT)
            List<Employee> emps = dept.getEmployees();
            assertTrue(emps.size() > 0, "Engineering should have employees");
            System.out.println("Engineering employees: " + emps);
        }
    }

    // ========================================================================
    // Phase 4: Aggregates (HQL)
    // ========================================================================

    @Test
    @Order(10)
    void testCountAggregate() {
        try (Session session = sessionFactory.openSession()) {
            Long count = session.createQuery(
                "SELECT COUNT(e) FROM Employee e", Long.class
            ).getSingleResult();
            assertEquals(2L, count, "Should have 2 employees");
        }
    }

    @Test
    @Order(11)
    void testSumAggregate() {
        try (Session session = sessionFactory.openSession()) {
            Double totalSalary = session.createQuery(
                "SELECT SUM(e.salary) FROM Employee e", Double.class
            ).getSingleResult();
            assertNotNull(totalSalary);
            assertTrue(totalSalary > 0, "Total salary should be positive");
            System.out.println("Total salary: " + totalSalary);
        }
    }

    // ========================================================================
    // Phase 5: Transactions
    // ========================================================================

    @Test
    @Order(12)
    void testRollback() {
        // Insert and rollback
        try (Session session = sessionFactory.openSession()) {
            Transaction tx = session.beginTransaction();
            session.persist(new Employee("TempWorker", 10000.0));
            tx.rollback();
        }

        // Verify rollback worked
        try (Session session = sessionFactory.openSession()) {
            List<Employee> temps = session.createQuery(
                "FROM Employee e WHERE e.name = :name", Employee.class
            ).setParameter("name", "TempWorker").getResultList();
            assertEquals(0, temps.size(), "TempWorker should not exist after rollback");
        }
    }

    // ========================================================================
    // Phase 6: Native SQL
    // ========================================================================

    @Test
    @Order(13)
    void testNativeQuery() {
        try (Session session = sessionFactory.openSession()) {
            List<?> result = session.createNativeQuery(
                "SELECT name, salary FROM employee WHERE salary > :threshold",
                Object[].class
            ).setParameter("threshold", 50000.0).getResultList();
            assertFalse(result.isEmpty(), "Should find employees with salary > 50000");
        }
    }
}
