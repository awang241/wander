package com.springvuegradle.model;

import java.util.Objects;

/**
 * Wrapper class for passing search criteria of Profiles from controller to service classes.
 */
public class ProfileSearchCriteria {
    private String firstName;
    private String middleName;
    private String lastName;
    private String nickname;
    private String emailAddress;
    private String searchMethod;
    private String[] activityTypes;

    /**
     * Creates a blank ProfileSearchCriteria with all criteria set to null.
     */
    public ProfileSearchCriteria() {
        firstName = null;
        middleName = null;
        lastName = null;
        nickname = null;
        emailAddress = null;
        activityTypes = null;
        searchMethod = null;
    }

    /**
     * Creates a new ProfileSearchCriteria with the given criteria.
     */
    public ProfileSearchCriteria(String firstName, String middleName, String lastName, String nickname, String email) {
        this.firstName = firstName;
        this.middleName = middleName;
        this.lastName = lastName;
        this.nickname = nickname;
        this.emailAddress = email;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getMiddleName() {
        return middleName;
    }

    public void setMiddleName(String middleName) {
        this.middleName = middleName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public String getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(String email) {
        this.emailAddress = email;
    }

    public void setSearchMethod(String searchMethod) {
        this.searchMethod = searchMethod;
    }

    public void setActivityTypes(String[] activityTypes) {
        this.activityTypes = activityTypes;
    }

    public String[] getActivityTypes(){return this.activityTypes;}

    public String getSearchMethod(){return this.searchMethod;}


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ProfileSearchCriteria criteria = (ProfileSearchCriteria) o;
        return Objects.equals(firstName, criteria.firstName) &&
                Objects.equals(middleName, criteria.middleName) &&
                Objects.equals(lastName, criteria.lastName) &&
                Objects.equals(nickname, criteria.nickname) &&
                Objects.equals(emailAddress, criteria.emailAddress);
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstName, middleName, lastName, nickname, emailAddress);
    }
}
