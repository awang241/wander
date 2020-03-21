package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.*;

import javax.persistence.*;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.text.SimpleDateFormat;
import java.util.*;

@Entity
public class Profile {

    @Id @GeneratedValue
    private Long id;
    @NotNull
    private String firstname;
    @NotNull
    private String lastname;
    private String middlename;
    private String nickname;
    @NotNull @Size(min = 1, max = 5)
    @OneToMany(fetch = FetchType.EAGER, mappedBy = "profile")
    private Set<Email> emails = new HashSet<>();
    @NotNull
    private String password;
    private String bio;
    @NotNull
    private Calendar dateOfBirth;
    @NotNull
    private String gender;
    //@NotNull @Column(name = "fitness_level")@Min(value = 0) @Max(value = 4)
    private int fitnessLevel;

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = "profile_passport_country",
            inverseJoinColumns = @JoinColumn(name = "passport_country_id", referencedColumnName = "id"),
            joinColumns = @JoinColumn(name = "profile_id", referencedColumnName = "id"))
    private Set<PassportCountry> passportCountries;

    /**
     * No argument constructor for Profile, can be used for creating new profiles directly from JSON data.
     */
    public Profile() {}

    /**
     * Constructor for Profile. The way the JSONProperty is structured is how the getProfile method should display the
     * users details as well.
     * @param firstname first name of user
     * @param lastname last name of user
     * @param middlename middle name of user
     * @param nickname nickname of user
     * @param primaryEmail users primary email address
     * @param password (encrypted)
     * @param bio other information about the user that they wish to enter
     * @param dateOfBirth (xxxx_xx_xx -> year_month_day)
     * @param gender (Male, Female, Other)
     */
    @JsonCreator
    public Profile(@JsonProperty("id") Long id,
                   @JsonProperty("firstname") String firstname,
                   @JsonProperty("lastname") String lastname,
                   @JsonProperty("middlename") String middlename,
                   @JsonProperty("nickname") String nickname,
                   @JsonProperty("primary_email") String primaryEmail,
                   @JsonProperty("additional_email") String[] additionalEmails,
                   @JsonProperty("password") String password,
                   @JsonProperty("bio") String bio,
                   @JsonProperty("date_of_birth") Calendar dateOfBirth,
                   @JsonProperty("gender") String gender,
                   @JsonProperty("fitness_level") int fitness_level,
                   @JsonProperty("passport_countries") String[] passportCountries) {
        this.firstname = firstname;
        this.lastname = lastname;
        this.middlename = middlename;
        this.nickname = nickname;

        this.emails = new HashSet<>();

        System.out.println(addEmail(new Email(primaryEmail, true)));

        for (String email: additionalEmails) {
            System.out.println(addEmail(new Email(email)));
        }

        this.password = password;
        this.bio = bio;
        this.dateOfBirth = dateOfBirth;
        this.gender = gender;
        this.fitnessLevel = fitness_level;
        this.passportCountries = new HashSet<>();
        for (String name: passportCountries) {
            addPassportCountry(new PassportCountry(name));
        }
    }

    /**
     * Adds the email to the set. Does not check repository to see if the email address is alredy in use. Trying to keep
     * db related queries in Controller classes. Though, it does check if the email is already in the list of emails as
     * well as if the list of emails is already at the max capacity (5).
     * @param email
     * @return
     */
    public boolean addEmail(Email email) {
        boolean alreadyInEmails = false;
        for (Email tEmail: emails) {
            if (tEmail.getAddress() == email.getAddress()) {
                alreadyInEmails = true;
            }
        }
        if (alreadyInEmails || emails.size() >= 5) {
            return false;
        } else {
            email.setProfile(this);
            this.emails.add(email);
            return true;
        }
    }

    /**
     * This method removes an email from the set of emails given that it is already in the set of emails. Also checks to
     * make sure that the email is not a primary email, if it is then it does not remove the email and just returns false.
     * The method also returns false if it cannot find an email with the same address in the associated set of emails.
     * @param email that we want to remove.
     * @return true if email is removed, false otherwise.
     */
    public boolean removeEmail(Email email) {
        for (Email currentEmail: emails) {
            if (currentEmail.getAddress() == email.getAddress() && !currentEmail.isPrimary()) {
                emails.remove(currentEmail);
                return true;
            }
        }
        return false;
    }

    /**
     * Changes the primary email to the given email object given that is already in the set of emails.
     * @param newPrimary new Email object already in set we want to set primary.
     */
    public boolean changePrimary(Email newPrimary) {
        boolean primaryChanged = false;
        for (Email currentEmail: emails) {
            if (currentEmail.getAddress() == newPrimary.getAddress()) {
                currentEmail.setPrimary(true);
                primaryChanged = true;
            } else {
                currentEmail.setPrimary(false);
            }
        }
        return primaryChanged;
    }


    public Set<Email> retrieveEmails() {
        return emails;
    }

    public void setEmails(Set<Email> emails) {
        this.emails = emails;
    }


    public String getDateOfBirth() {
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
        format.setCalendar(dateOfBirth);
        return format.format(dateOfBirth.getTime());
    }



    /**
     * Gets the passport countries as a string of names instead of objects
     * @return list of country name strings
     */
    public List<String> getPassportCountryNames() {
        List<String> countryNames = new ArrayList<>();
        for (PassportCountry country : passportCountries){
            countryNames.add(country.getCountryName());
        }
        return countryNames;
    }

    public Set<PassportCountry> getPassportCountries() {
        return this.passportCountries;
    }

    public void setPassportCountries(Set<PassportCountry> passport_countries) {
        this.passportCountries = passport_countries;
    }

    public void addPassportCountry(PassportCountry passportCountry) {
        passportCountries.add(passportCountry);
    }

    public void removePassportCountry(PassportCountry passportCountry) {
        passportCountries.remove(passportCountry);
    }

    /**
     * This method is used to update a profile with the given profile's details.
     * @param editedProfile is the profile that we want to take the updated data from to place in the db profile.
     */
    public void updateProfile(Profile editedProfile) {
        this.firstname = editedProfile.firstname;
        this.lastname = editedProfile.lastname;
        this.middlename = editedProfile.middlename;
        this.nickname = editedProfile.nickname;
        this.emails = editedProfile.emails;
        this.password = editedProfile.password;
        this.bio = editedProfile.bio;
        this.dateOfBirth = editedProfile.dateOfBirth;
        this.gender = editedProfile.gender;
        this.fitnessLevel = editedProfile.fitnessLevel;
        this.passportCountries = editedProfile.passportCountries;
    }

    /**
     * Checks the equality of two profile objects by comparing all fields
     * @param o The profile object. other objects can be passed in but will return false for equality
     * @return boolean result - true if all fields are equal, false otherwise
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof Profile) {
            Profile other = (Profile) o;
            return this.firstname.equals(other.firstname) &&
                    this.lastname.equals(other.lastname) &&
                    this.middlename.equals(other.middlename) &&
                    this.nickname.equals(other.nickname) &&
                    this.getPrimary_email().equals(other.getPrimary_email()) &&
                    this.getAdditional_email().equals(other.getAdditional_email()) &&
                    this.password.equals(other.password) &&
                    this.bio.equals(other.bio) &&
                    this.getDateOfBirth().equals(other.getDateOfBirth()) &&
                    this.gender.equals(other.gender) &&
                    this.fitnessLevel == other.fitnessLevel &&
                    this.passportCountries.equals(other.passportCountries);
        } else {
            return false;
        }
    }
    /** Series of Getters and Getters **/

    public Long getId() {
        return id;
    }

    public void setDateOfBirth(Calendar date_of_birth) {
        this.dateOfBirth = date_of_birth;
    }

    public String getGender() {
        return gender;
    }

    public void setGender(String gender) {
        this.gender = gender;
    }

    public int getFitnessLevel(){return fitnessLevel;}

    public void setFitnessLevel(int fitness_level){this.fitnessLevel = fitness_level;}



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

    public Email retrievePrimaryEmail() {
        for (Email email: emails) {
            if (email.isPrimary()) {
                return email;
            }
        }
        return null;
    };

    public String getPrimary_email() {
        for (Email email: emails) {
            if (email.isPrimary()) {
                return email.getAddress();
            }
        }
        return null;
    };

    public Set<String> getAdditional_email() {
        Set<String> emailStrings = new HashSet<>();
        for (Email email: emails) {
            if (!email.isPrimary()) {
                emailStrings.add(email.getAddress());
            }

        }
        return emailStrings;
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

}