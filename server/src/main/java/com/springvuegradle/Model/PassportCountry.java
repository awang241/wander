package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.*;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Entity
public class PassportCountry {

    @Id
    @GeneratedValue
    private long id;

    @Column
    @NotNull
    private String countryName;

    @Column(unique = true)
    @NotNull
    private String numericCode;

    @ManyToMany(mappedBy = "passport_countries")
    @JsonBackReference
    private Set<Profile> profiles = new HashSet<Profile>();

    public PassportCountry(){};

    public PassportCountry(String name){
        this.countryName = name;
    };

    @JsonCreator
    public PassportCountry(@JsonProperty("name") String countryName, @JsonProperty("numericCode") String code){
        this.countryName = countryName;
        this.numericCode = code;
    }

    public void setCountryName(String name){this.countryName = name;}
    public String getCountryName(){return this.countryName;}

    public String getNumericCode() {
        return numericCode;
    }

    public void setNumericCode(String code) {
        this.numericCode = code;
    }

    public Set<Profile> getProfile() { return profiles; }

    public void setProfiles(Set<Profile> newProfiles) { this.profiles = newProfiles;}

    /**
     * Adds the given profile to the country's list of profiles, given a profile with the same ID is there already.
     * @param profile The target profile to be added.
     */
    public void addProfile(Profile profile) {
        profiles.add(profile);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PassportCountry country = (PassportCountry) o;
        return countryName.equals(country.countryName) &&
                numericCode.equals(country.numericCode);
    }

    @Override
    public int hashCode() {
        return Objects.hash(countryName, numericCode);
    }
}
