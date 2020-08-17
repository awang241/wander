package com.springvuegradle.dto.requests;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class EmailAddRequest {
    private List<String> emails;

    /**
     * Constructor for a emailaddrequest with parameters. for JSON parsing with spring requestmapping methods.
     * @param emails an array of email strings
     */
    public EmailAddRequest(@JsonProperty("additional_emails") List<String> emails){
        this.emails = emails;
    }

    public List<String> getEmails() {
        return emails;
    }

    public void setEmails(List<String> emails) {
        this.emails = emails;
    }




}
