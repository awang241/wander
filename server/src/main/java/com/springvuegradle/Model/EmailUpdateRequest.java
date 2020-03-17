package com.springvuegradle.Model;

import javax.persistence.Entity;
import java.util.ArrayList;

/**
 * Model Class for incoming email/password requests used for logging in
 */
public class EmailUpdateRequest {

    private ArrayList<String> optionalEmails;
    private String primaryEmail;
    private int userId;

    public EmailUpdateRequest(ArrayList<String> optionalEmails, String primaryEmail, int userId){
        this.optionalEmails = optionalEmails;
        this.primaryEmail = primaryEmail;
        this.userId = userId;
    }

    public ArrayList<String> getOptionalEmails() {
        return optionalEmails;
    }

    public void setOptionalEmails(ArrayList<String> optionalEmails) {
        this.optionalEmails = optionalEmails;
    }

    public String getPrimaryEmail() {
        return primaryEmail;
    }

    public void setPrimaryEmail(String primaryEmail) {
        this.primaryEmail = primaryEmail;
    }

    public int getUserId() {
        return userId;
    }

    public void setUserId(int userId) {
        this.userId = userId;
    }

}
