package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.*;
import org.hibernate.annotations.Generated;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import static javax.persistence.GenerationType.IDENTITY;
import static javax.persistence.GenerationType.SEQUENCE;

@Entity
public class PassportCountry {

    /**
     * Holds the automatically generated passport country id obtained when the object is saved.
     */
    @Id
    @GeneratedValue(strategy=IDENTITY, generator="country_id_seq")
    private long id;

    /**
     * Holds the country name its referring to.
     */
    @Column
    @NotNull
    private String countryName;

    /**
     * Each country has a given numeric code, some countries don't have a numeric code so are given ones when objects are
     * generated. Hence, each is unique.
     */
    @Column(unique = true)
    @NotNull
    private String numericCode;

    /**
     * Each passport country object can have multiple profiles with the passport countries being referred.
     */
    @ManyToMany(mappedBy = "passports")
    @JsonBackReference
    private Set<Profile> profiles = new HashSet<Profile>();

    public PassportCountry(){};

    public PassportCountry(String name){
        this.countryName = name;
        this.numericCode = "Dummy Code";
    };

    @JsonCreator
    public PassportCountry(
            @JsonProperty("name") String countryName,
            @JsonProperty("numericCode") String code){
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
