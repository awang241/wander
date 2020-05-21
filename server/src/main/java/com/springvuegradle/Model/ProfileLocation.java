package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.*;

/**
 * Location class.
 * Needed in order for a profile object to be associated with a location.
 */
@Entity
public class ProfileLocation {
    public ProfileLocation(){};

    @JsonCreator
    public ProfileLocation(
            @JsonProperty("country") String country,
            @JsonProperty("city") String city,
            @JsonProperty("state") String state){
       this.country = country;
       this.city = city;
       this.state = state;
    }

    /**
     * Holds the automatically generated location id assigned when the object is saved to the database.
     */
    @Id
    @GeneratedValue
    private long id;

    /**
     * Each location object is associated with one and only one profile object.
     */
    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "profile_id")
    @JsonBackReference
    private Profile profile;

    /**
     * Holds the country as a string.
     */
    @Column()
    private String country;

    /**
     * Holds the city as a string.
     */
    @Column()
    private String city;

    /**
     * Holds the state as a string.
     */
    @Column()
    private String state;

    public Profile getProfile() {
        return profile;
    }

    public void setProfile(Profile profile) {
        this.profile = profile;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(long id) { this.country = country; }

    public String getCity() {
        return city;
    }

    public void setCity(long id) { this.city = city; }

    public String getState() {
        return state;
    }

    public void setState(long id) { this.state = state; }
}
