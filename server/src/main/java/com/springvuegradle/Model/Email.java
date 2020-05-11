package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;

import javax.annotation.processing.Generated;
import javax.persistence.*;
import java.util.Objects;

import static javax.persistence.GenerationType.IDENTITY;
import static javax.persistence.GenerationType.SEQUENCE;

/**
 * Email class.
 * Needed in order for a profile object to be associated with multiple emails.
 */
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

    /**
     * Creates an Email object and links it to a given Profile object.
     * @param address value we want to assign to the email address
     * @param isPrimary indicates whether the email should be primary or not
     * @param profile object we want the email to be associated with
     */
    public Email(String address, boolean isPrimary, Profile profile) {
        this.address = address;
        this.isPrimary = isPrimary;
        this.profile = profile;
    }

    /**
     * Holds the automatically generated email id assigned when the object is saved to the database.
     */
    @Id
    @GeneratedValue(strategy=IDENTITY, generator="email_id_seq")
    private long id;

    /**
     * Each email object is associated with one and only one profile object.
     */
    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "profile_id")
    @JsonBackReference
    private Profile profile;

    /**
     * Holds the email address as a string. Each email address must be unique and cannot be null.
     */
    @Column(unique=true, nullable=false)
    private String address;

    /**
     * To indicate whether the email is primary or additional. Intitially set to false.
     */
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

    /**
     * Overrive of the equals method for testing if two email objects are equal but different objects
     * @param o the second email object being checked
     * @return boolean of the results of the equality
     */
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
