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

    public SimplifiedProfileResponse(Long id, String firstname, String lastname, String email, String gender) {
        this.id = id;
        this.firstname = firstname;
        this.lastname = lastname;
        this.email = email;
        this.gender = gender;
    }

}
