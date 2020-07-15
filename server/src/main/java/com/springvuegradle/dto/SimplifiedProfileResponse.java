package com.springvuegradle.dto;

public class SimplifiedProfileResponse {

    /**
     * Holds the user id. Generated and assigned when the object is saved in the database.
     */
    private Long id;

    /**
     * Holds the user's firstname.
     */
    private String firstname;

    /**
     * Holds the user's lastname.
     */
    private String lastname;

    /**
     *
     */
    private String email;

    /**
     * Stored the gender as a string, e.g. male, female, non-binary.
     */
    private String gender;


    /**
     * Constructor for Profile. The way the JSONProperty is structured is how the getProfile method should display the
     * users details as well.
     *
     * @param id        id of user
     * @param firstname first name of user
     * @param lastname  last name of user
     * @param email     users primary email address
     * @param gender    (Male, Female, Other)
     */
    public SimplifiedProfileResponse(Long id, String firstname, String lastname, String email, String gender) {
        this.id = id;
        this.firstname = firstname;
        this.lastname = lastname;
        this.email = email;
        this.gender = gender;
    }

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

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getGender() {
        return gender;
    }

    public void setGender(String gender) {
        this.gender = gender;
    }
}
