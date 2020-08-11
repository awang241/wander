package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * Model Class for incoming activity privacy requests used for changing an activity's privacy level.
 */
public class PrivacyRequest {
    private String privacy;
    private List<MembersRequest> members;

    /**
     * Constructor for a privacy request with parameter for JSON parsing with spring requestmapping methods.
     * @param privacy the privacy level that the user would like to change their activity visibility to.
     */
    public PrivacyRequest(@JsonProperty("privacy") String privacy){
        this.privacy = privacy;
    }

    public void setPrivacy(String privacy) { this.privacy = privacy; }
    public void setMembers(List<MembersRequest> members) {
        this.members = members;
    }
    public String getPrivacy() {
        return privacy;
    }
    public List<MembersRequest> getMembers() {
        return members;
    }

}


