package io.replikativ.test;

import jakarta.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "department")
public class Department {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @Column(nullable = false)
    private String name;

    private Double budget;

    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Employee> employees = new ArrayList<>();

    public Department() {}

    public Department(String name, Double budget) {
        this.name = name;
        this.budget = budget;
    }

    public Long getId() { return id; }
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public Double getBudget() { return budget; }
    public void setBudget(Double budget) { this.budget = budget; }
    public List<Employee> getEmployees() { return employees; }

    public void addEmployee(Employee emp) {
        employees.add(emp);
        emp.setDepartment(this);
    }

    @Override
    public String toString() {
        return "Department{id=" + id + ", name=" + name + ", budget=" + budget + "}";
    }
}
