package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.*;

import javax.persistence.*;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.text.SimpleDateFormat;
import java.util.*;


/**
 * Profile class.
 * Profile objects are used to structure the user data and enable the user data to be saved in an organised way.
 */
@Entity
public class Profile {

    /**
     * Holds the user id. Generated and assigned when the object is saved in the database.
     */
    @Id
    @GeneratedValue
    private Long id;

    /**
     * Holds the user's firstname.
     */
    @NotNull
    private String firstname;

    /**
     * Holds the user's lastname.
     */
    @NotNull
    private String lastname;

    /**
     * Holds the user's middlename (optional).
     */
    private String middlename;

    /**
     * Holds the user's nickname (optional).
     */
    private String nickname;

    /**
     * Holds the user's emails, this includes primary and additional emails. Only one of these emails will have a
     * primary tag set to true and is necessary to be included in a valid profile object, hence, not null with min size
     * of 1. One to Many relationship also established as a user can have many emails but each email can only be assigned
     * to one user at a time.
     * Essentially a multi-valued attribute, hence why Email objects are used.
     */
    @NotNull @Size(min = 1, max = 5)
    @OneToMany(fetch = FetchType.EAGER, mappedBy = "profile")
    private Set<Email> emails = new HashSet<>();

    /**
     * The user's password (taken from JSON as plaintext and stored as a hash).
     */
    @NotNull
    private String password;

    /**
     * Holds the user's detailed description of what they are like.
     */
    private String bio;

    /**
     * Stores the user's date of birth as a Calender object.
     */
    @NotNull
    private Calendar dateOfBirth;

    /**
     * Stored the gender as a string, e.g. male, female, non-binary.
     */
    @NotNull
    private String gender;

    /**
     * Holds the fitness value of the user, where a 0 means the user does little to no fitness while 4 means that the
     * user exercises quite frequently.
     */
    @NotNull @Column(name = "fitness") @Min(value = 0) @Max(value = 4)
    private int fitness;

    /**
     * Holds the authority level of the user, the lower the number, the higher authority the user has.
     * level 0 - default admin
     * level 1 - regular admin
     * level 5 - regular user
     */
    @NotNull @Column(name = "authLevel") @Min(value = 0) @Max(value = 5)
    private int authLevel = 5;

    /**
     * Holds the user's passports and estabishes a Many to Many relationship as a Profile object can be associated with
     * multiple PassportCountry.
     */
    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = "profile_passport_country",
            inverseJoinColumns = @JoinColumn(name = "passport_country_id", referencedColumnName = "id"),
            joinColumns = @JoinColumn(name = "profile_id", referencedColumnName = "id"))
    private Set<PassportCountry> passports;

    /**
     * Holds the user's activityTypes and establishes a many to many relationship as a Profile object can be associated with
     * multiple ActivityType objects.
     */
    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = "profile_activity_type",
            inverseJoinColumns = @JoinColumn(name = "activity_type_id", referencedColumnName = "id"),
            joinColumns = @JoinColumn(name = "profile_id", referencedColumnName = "id"))
    private Set<ActivityType> activityTypes;

    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "location_id", referencedColumnName = "id")
    private ProfileLocation profileLocation;

    @JsonIgnore
    public Set<ActivityMembership> getActivities() {
        return activities;
    }

    public void setActivities(Set<ActivityMembership> activities) {
        this.activities = activities;
    }

    @OneToMany(fetch = FetchType.EAGER, mappedBy = "profile")
    private Set<ActivityMembership> activities = new HashSet<>();

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
                   @JsonProperty("fitness") int fitnessLevel,
                   @JsonProperty("passports") String[] passports,
                   @JsonProperty("activities") String[] activityTypes) {
        this.firstname = firstname;
        this.lastname = lastname;
        this.middlename = middlename;
        this.nickname = nickname;

        this.emails = new HashSet<>();

        addEmail(new Email(primaryEmail, true));

        for (String email: additionalEmails) {
            addEmail(new Email(email));
        }

        this.password = password;
        this.bio = bio;
        this.dateOfBirth = dateOfBirth;
//        this.dateOfBirth.add(Calendar.DATE, 1);
        this.gender = gender;
        this.fitness = fitnessLevel;
        this.passports = new HashSet<>();
        for (String name: passports) {
            addPassportCountry(new PassportCountry(name));
        }
        this.activityTypes = new HashSet<>();
        for (String activityType: activityTypes) {
            addActivityType(new ActivityType(activityType));
        }
        this.activities = new HashSet<>();
    }

    /**
     * Adds the email to the set. Does not check repository to see if the email address is already in use. Trying to keep
     * db related queries in Controller classes. Though, it does check if the email is already in the list of emails as
     * well as if the list of emails is already at the max capacity (5).
     * @param email
     * @return
     */
    public boolean addEmail(Email email) {
        boolean alreadyInEmails = false;
        for (Email tEmail: emails) {
            if (tEmail.getAddress().equals(email.getAddress())) {
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
            if (currentEmail.getAddress().equals(email.getAddress()) && !currentEmail.isPrimary()) {
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


    /**
     * Gets the email objects.
     * @return set of Email objects
     */
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
    public List<String> getPassports() {
        List<String> countryNames = new ArrayList<>();
        for (PassportCountry country : passports){
            countryNames.add(country.getCountryName());
        }
        return countryNames;
    }

    public List<String> getActivityTypes() {
        List<String> activityTypeNames = new ArrayList<>();
        for (ActivityType activityType : activityTypes){
            activityTypeNames.add(activityType.getActivityTypeName());
        }
        return activityTypeNames;
    }



    @JsonIgnore
    public Set<PassportCountry> getPassportObjects() {
        return this.passports;
    }

    public void setPassports(Set<PassportCountry> passport_countries) {
        this.passports = passport_countries;
    }

    public void addPassportCountry(PassportCountry passportCountry) {
        passports.add(passportCountry);
    }

    public void removePassportCountry(PassportCountry passportCountry) {
        passports.remove(passportCountry);
    }

    @JsonIgnore
    public Set<ActivityType> getActivityTypeObjects() {
        return this.activityTypes;
    }

    public void setActivityTypes(Set<ActivityType> activityTypes) {
        this.activityTypes = activityTypes;
    }

    public void addActivityType(ActivityType activityType) {
        activityTypes.add(activityType);
    }

    public void removeActivityType(ActivityType activityType) {
        activityTypes.remove(activityType);
    }

    /**
     * This method is used to update a profile with the given profile's details. Not used to update emails or password.
     * @param editedProfile is the profile that we want to take the updated data from to place in the db profile.
     */
    public void updateProfileExceptEmailsPassword(Profile editedProfile) {
        this.firstname = editedProfile.firstname;
        this.lastname = editedProfile.lastname;
        this.middlename = editedProfile.middlename;
        this.nickname = editedProfile.nickname;
        this.bio = editedProfile.bio;
        this.dateOfBirth = editedProfile.dateOfBirth;
        this.gender = editedProfile.gender;
        this.fitness = editedProfile.fitness;
        this.passports = editedProfile.passports;
        this.activityTypes = editedProfile.activityTypes;
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
                    this.fitness == other.fitness &&
                    this.passports.equals(other.passports) &&
                    this.activityTypes.equals(other.activityTypes);
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstname, lastname, middlename, nickname, emails, password, bio, dateOfBirth, gender, fitness, passports, activityTypes);
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

    public int getFitness(){return fitness;}

    public void setFitness(int fitness_level){this.fitness = fitness_level;}

    @JsonIgnore
    public int getAuthLevel(){return authLevel;}

    public void setAuthLevel(int authLevel){this.authLevel = authLevel;}



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

    public void setLocation(ProfileLocation location){
        this.profileLocation = location;
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
    }

    public String getPrimary_email() {
        for (Email email: emails) {
            if (email.isPrimary()) {
                return email.getAddress();
            }
        }
        return null;
    }

    public Set<String> getAdditional_email() {
        Set<String> emailStrings = new HashSet<>();
        for (Email email: emails) {
            if (!email.isPrimary()) {
                emailStrings.add(email.getAddress());
            }

        }
        return emailStrings;
    }

    @JsonIgnore
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

    public boolean addActivity(ActivityMembership membership) {
        return this.activities.add(membership);
    }

    public boolean removeActivity(ActivityMembership membership) {
        return this.activities.remove(membership);
    }

}