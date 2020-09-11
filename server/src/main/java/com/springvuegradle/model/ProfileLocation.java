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
            @JsonProperty("longitude") Double longitude,
            @JsonProperty("latitude") Double latitude,
            @JsonProperty("address") String address) {
        this.longitude = longitude;
        this.latitude = latitude;
        this.address = address;
    }

    public ProfileLocation(
            String city,
            String state,
            String country) {
        this.address = String.format("%s, %s, %s.", city, state, country);
        this.longitude = 0.0;
        this.latitude = 0.0;
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
     * Holds the longitude and latitude coordinates used to point to the exact location on the map.
     */
    @Column()
    private Double longitude;

    @Column()
    private Double latitude;

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
        this.longitude = newLocation.longitude;
        this.latitude = newLocation.latitude;
        this.address = newLocation.address;
    }


    public Double getLongitude() {
        return longitude;
    }

    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    public Double getLatitude() {
        return latitude;
    }

    public void setLatitude(Double latitude) {
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
            return  this.longitude.equals(other.longitude) &&
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
