package com.springvuegradle.Model;

import javax.persistence.*;

@Entity
public class Email {

    public Email() {};

    public Email(String address, Long id) {
        this.address = address;

    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    @Id
    @GeneratedValue
    private long id;

    @Column(unique=true, nullable=false)
    private String address;


    private boolean isPrimary = false;

    public String getAddress() {
        return address;
    }

    public void setAddress(String email) {
        this.address = email;
    }


    public Email(String address){
        this.address = address;
    }

    public Email(String address, boolean isPrimary){
        this.address = address;
        this.isPrimary = isPrimary;
    }

    public void setPrimary(boolean isPrimary) {
        this.isPrimary = isPrimary;
    }

    public boolean isPrimary() {
        return this.isPrimary;
    }
}
