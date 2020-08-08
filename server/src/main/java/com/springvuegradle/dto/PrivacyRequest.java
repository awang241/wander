package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Model Class for incoming activity privacy requests used for changing an activity's privacy level.
 */
public class PrivacyRequest {
    private String privacy;

    /**
     * Constructor for a privacy request with parameter for JSON parsing with spring requestmapping methods.
     * @param privacy the privacy level that the user would like to change their activity visibility to.
     */
    public PrivacyRequest(@JsonProperty("privacy") String privacy){
        this.privacy = privacy;
    }

    public void setPrivacy(String privacy) { this.privacy = privacy; }
    public String getPrivacy() {
        return privacy;
    }

}


