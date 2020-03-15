package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.*;

import javax.management.AttributeList;
import javax.persistence.*;
import java.sql.Date;
import java.text.SimpleDateFormat;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Entity
public class Profile {

    @Id @GeneratedValue
    private Long id;

    private String firstname;
    private String lastname;
    private String middlename;
    private String nickname;
    private String email;
    private String password;
    private String bio;
    private Calendar date_of_birth;
    private String gender;
    private int fitness_level;

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = "profile_passport_country",
            inverseJoinColumns = @JoinColumn(name = "passport_country_id", referencedColumnName = "id"),
            joinColumns = @JoinColumn(name = "profile_id", referencedColumnName = "id"))
    private Set<PassportCountry> passport_countries;


    /**
     * No argument constructor for Profile, can be used for creating new profiles directly from JSON data.
     */
    public Profile() {}

    /**
     * Constructor for Profile.
     * @param firstname first name of user
     * @param lastname last name of user
     * @param middlename middle name of user
     * @param nickname nickname of user
     * @param email users email address
     * @param password (encrypted)
     * @param bio other information about the user that they wish to enter
     * @param date_of_birth (xxxx_xx_xx -> year_month_day)
     * @param gender (Male, Female, Other)
     */
    @JsonCreator
    public Profile(@JsonProperty("firstname") String firstname,
                   @JsonProperty("lastname") String lastname,
                   @JsonProperty("middlename") String middlename,
                   @JsonProperty("nickname") String nickname,
                   @JsonProperty("email") String email,
                   @JsonProperty("password") String password,
                   @JsonProperty("bio") String bio,
                   @JsonProperty("date_of_birth") Calendar date_of_birth,
                   @JsonProperty("gender") String gender,
                   @JsonProperty("fitness_level") int fitness_level,
                   @JsonProperty("passport_countries") String[] passport_countries) {
        this.firstname = firstname;
        this.lastname = lastname;
        this.middlename = middlename;
        this.nickname = nickname;
        this.email = email;
        this.password = password;
        this.bio = bio;
        this.date_of_birth = date_of_birth;
        this.gender = gender;
        this.fitness_level = fitness_level;
        this.passport_countries = new HashSet<>();
        for (String name: passport_countries) {
            addPassportCountry(new PassportCountry(name));
        }
    }

    /**
         * Constructor for Profile.
         * @param firstname first name of user
         * @param lastname last name of user
         * @param middlename middle name of user
         * @param nickname nickname of user
         * @param email users email address
         * @param password (encrypted)
         * @param bio other information about the user that they wish to enter
         * @param date_of_birth date of birth as an Calendar object.
         * @param gender (Male, Female, Other)
         */
    public Profile(String firstname, String lastname, String middlename, String nickname,
                   String email, String password, String bio, Calendar date_of_birth,
                   String gender) {
        this.firstname = firstname;
        this.lastname = lastname;
        this.middlename = middlename;
        this.nickname = nickname;
        this.email = email;
        this.password = password;
        this.bio = bio;
        this.date_of_birth = date_of_birth;
        this.gender = gender;
        this.passport_countries = new HashSet<>();

    }

    /** Series of Getters and Getters **/

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getFirstname() {
        return firstname;
    }

    public void setFirstname(String firstname) {
        this.firstname = firstname;
    }

    public String getLastname() {
        return lastname;
    }

    public void setLastname(String lastname) {
        this.lastname = lastname;
    }

    public String getMiddlename() {
        return middlename;
    }

    public void setMiddlename(String middlename) {
        this.middlename = middlename;
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getBio() {
        return bio;
    }

    public void setBio(String bio) {
        this.bio = bio;
    }

    public String getDate_of_birth() {
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
        format.setCalendar(date_of_birth);
        return format.format(date_of_birth.getTime());
    }

    public void setDate_of_birth(Calendar date_of_birth) {
        this.date_of_birth = date_of_birth;
    }

    public String getGender() {
        return gender;
    }

    public void setGender(String gender) {
        this.gender = gender;
    }

    public int getFitness_level(){return fitness_level;}

    public void setFitness_level(int fitness_level){this.fitness_level = fitness_level;}

    public List<String> getPassport_countries() {
        List<String> countryNames = new ArrayList<>();
        for (PassportCountry country : passport_countries){
            countryNames.add(country.getCountryName());
        }
        return countryNames;
    }

    public Set<PassportCountry> retrievePassportCountryObjects() {
        return this.passport_countries;
    }

    public void setPassport_countries(Set<PassportCountry> passport_countries) {
        this.passport_countries = passport_countries;
    }

    public void addPassportCountry(PassportCountry passportCountry) {
        passport_countries.add(passportCountry);
    }

    public void removePassportCountry(PassportCountry passportCountry) {
        passport_countries.remove(passportCountry);
    }
    // Helper methods for ProfileController

    /**
     * This method is used to update a profile with the given profile's details.
     * @param editedProfile is the profile that we want to take the updated data from to place in the db profile.
     */
    public void updateProfile(Profile editedProfile) {
        this.firstname = editedProfile.firstname;
        this.lastname = editedProfile.lastname;
        this.middlename = editedProfile.middlename;
        this.nickname = editedProfile.nickname;
        this.email = editedProfile.email;
        this.password = editedProfile.password;
        this.bio = editedProfile.bio;
        this.date_of_birth = editedProfile.date_of_birth;
        this.gender = editedProfile.gender;
        this.fitness_level = editedProfile.fitness_level;
        this.passport_countries = editedProfile.passport_countries;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof Profile) {
            Profile other = (Profile) o;
            return this.firstname.equals(other.firstname) &&
                    this.lastname.equals(other.lastname) &&
                    this.middlename.equals(other.middlename) &&
                    this.nickname.equals(other.nickname) &&
                    this.email.equals(other.email) &&
                    this.password.equals(other.password) &&
                    this.bio.equals(other.bio) &&
                    //this.getDate_of_birth() == other.getDate_of_birth() &&
                    this.gender.equals(other.gender) &&
                    this.fitness_level == other.fitness_level &&
                    this.passport_countries.equals(other.passport_countries);
        } else {
            return false;
        }


    }


}