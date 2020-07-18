package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.*;

/**
 * Location class.
 * Needed in order for a profile object to be associated with a location.
 */
@Entity
public class ProfileLocation {
    public ProfileLocation() { }

    ;

    @JsonCreator
    public ProfileLocation(
            @JsonProperty("country") String country,
            @JsonProperty("city") String city,
            @JsonProperty("state") String state) {
        this.country = country;
        this.city = city;
        this.state = state;
    }

    /**
     * Holds the automatically generated location id assigned when the object is saved to the database.
     */
    @Id
    @JsonIgnore
    @GeneratedValue()
    private Long id;

    /**
     * Each location object is associated with one and only one profile object.
     */
    @JsonIgnore
    @OneToOne(mappedBy = "location")
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

    public Long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public String getCity() {
        return city;
    }

    public void setCity(String city) {
        this.city = city;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public void update(ProfileLocation newLocation) {
        this.city = newLocation.city;
        this.state = newLocation.state;
        this.country = newLocation.country;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof ProfileLocation) {
            ProfileLocation other = (ProfileLocation) o;
            return this.city.equals(other.city) &&
                    this.state.equals(other.state) &&
                    this.country.equals(other.country);

        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
