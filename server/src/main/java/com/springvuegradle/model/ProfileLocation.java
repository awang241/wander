package com.springvuegradle.model;

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

    @JsonCreator
    public ProfileLocation(
            @JsonProperty("country") String country,
            @JsonProperty("city") String city,
            @JsonProperty("state") String state,
            @JsonProperty("longitude") Long longitude,
            @JsonProperty("latitude") Long latitude,
            @JsonProperty("address") String address) {
        this.country = country;
        this.city = city;
        this.state = state;
        this.longitude = longitude;
        this.latitude = latitude;
        this.address = address;
    }

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

    /**
     * Holds the longitude and latitude coordinates used to point to the exact location on the map.
     */
    @Column()
    private Long longitude;

    @Column()
    private Long latitude;

    /**
     * Holds the address as a string.
     */
    @Column()
    private String address;

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

    public void update(ProfileLocation newLocation) {
        this.city = newLocation.city;
        this.state = newLocation.state;
        this.country = newLocation.country;
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

    public Long getLongitude() {
        return longitude;
    }

    public void setLongitude(Long longitude) {
        this.longitude = longitude;
    }

    public Long getLatitude() {
        return latitude;
    }

    public void setLatitude(Long latitude) {
        this.latitude = latitude;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof ProfileLocation) {
            ProfileLocation other = (ProfileLocation) o;
            return this.city.equals(other.city) &&
                    this.state.equals(other.state) &&
                    this.country.equals(other.country) &&
                    this.longitude.equals(other.longitude) &&
                    this.latitude.equals(other.latitude) &&
                    this.address.equals(other.address);

        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
