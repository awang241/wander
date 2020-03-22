package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;

import javax.persistence.*;
import java.util.Objects;

@Entity
public class Email {

    public Email() {};

    public Email(String address){
        this.address = address;
    }

    /**
     * Creates an Email object.
     * @param address value we want to assign to the email address
     * @param isPrimary indicates whether the email should be primary or not
     */
    public Email(String address, boolean isPrimary){
        this.address = address;
        this.isPrimary = isPrimary;
    }

    public Email(String address, boolean isPrimary, Profile profile) {
        this.address = address;
        this.isPrimary = isPrimary;
        this.profile = profile;
    }

    @Id
    @GeneratedValue
    private long id;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "profile_id")
    @JsonBackReference
    private Profile profile;

    @Column(unique=true, nullable=false)
    private String address;

    @Column(nullable=false)
    private boolean isPrimary = false;

    public Profile getProfile() {
        return profile;
    }

    public void setProfile(Profile profile) {
        this.profile = profile;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String email) {
        this.address = email;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public void setPrimary(boolean isPrimary) {
        this.isPrimary = isPrimary;
    }

    public boolean isPrimary() {
        return this.isPrimary;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Email email = (Email) o;
        return Objects.equals(address, email.address);
    }

    @Override
    public int hashCode() {
        return Objects.hash(address);
    }
}