package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ActivityRequest {
    private long profileId;

    /**
     * Constructor for a ActivityRequest with a parameter for JSON parsing with spring request mapping methods.
     * @param profileId is the profile that is trying to view the activity.
     */
    public ActivityRequest(@JsonProperty("profileId") long profileId) { this.profileId = profileId; }

    public long getProfileId() {
        return profileId;
    }
    public void setProfileId(long profileId) {
        this.profileId = profileId;
    }
}
