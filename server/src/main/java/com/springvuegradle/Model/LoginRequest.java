package com.springvuegradle.Model;

import javax.persistence.Entity;

/**
 * Model Class for incoming email/password requests used for logging in
 */
public class LoginRequest {

    private String email;
    private String password;

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
