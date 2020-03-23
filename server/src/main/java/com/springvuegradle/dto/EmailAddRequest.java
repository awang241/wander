package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class EmailAddRequest {
    private List<String> emails;

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
