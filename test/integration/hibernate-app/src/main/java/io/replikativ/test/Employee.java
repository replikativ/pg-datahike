package io.replikativ.test;

import jakarta.persistence.*;
import java.time.Instant;

@Entity
@Table(name = "employee")
public class Employee {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @Column(nullable = false)
    private String name;

    private Double salary;

    @Column(name = "hire_date")
    private Instant hireDate;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "department_id")
    private Department department;

    public Employee() {}

    public Employee(String name, Double salary) {
        this.name = name;
        this.salary = salary;
        this.hireDate = Instant.now();
    }

    public Long getId() { return id; }
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public Double getSalary() { return salary; }
    public void setSalary(Double salary) { this.salary = salary; }
    public Instant getHireDate() { return hireDate; }
    public Department getDepartment() { return department; }
    public void setDepartment(Department department) { this.department = department; }

    @Override
    public String toString() {
        return "Employee{id=" + id + ", name=" + name + ", salary=" + salary + "}";
    }
}
