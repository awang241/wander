package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonManagedReference;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity
public class PassportCountry {

    @Id
    @GeneratedValue
    private long id;

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = "profile_passport_country",
    joinColumns = @JoinColumn(name = "profile_id", referencedColumnName = "id"),
    inverseJoinColumns = @JoinColumn(name = "passport_country_id", referencedColumnName = "id"))
    @JsonBackReference
    private List<Profile> profiles = new ArrayList<>();

    private String countryName;

    public PassportCountry(){};
    public PassportCountry(String countryName){
        this.countryName = countryName;
    }

    public void setCountryName(String name){this.countryName = name;}
    public String getCountryName(){return this.countryName;}


    //public List<Profile> getProfile() { return profiles; }
    public void setProfile(List<Profile> newProfiles) { this.profiles = newProfiles;}
    public void addProfile(Profile profile) {this.profiles.add(profile);}
}
