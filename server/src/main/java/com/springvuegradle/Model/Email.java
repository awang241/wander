package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;

import javax.persistence.*;

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
}
