package com.springvuegradle.dto.requests;

/**
 * Model Class for incoming email/password requests used for logging in
 */
public class LoginRequest {

    private String email;
    private String password;

    /**
     * Constructor for a loginrequest with parameters. for JSON parsing with spring requestmapping methods.
     * @param email the email address that the user is trying to log in with
     * @param password the password the user is trying to log in with
     */
    public LoginRequest(String email, String password){
        this.email = email;
        this.password = password;
    }

    public String getEmail() {
        return email;
    }

    public String getPassword() {
        return password;
    }


}
