package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ActivityRoleRequest {
    private String activityRole;

    /**
     * Constructor for a ActivityRoleRequest with a parameter for JSON parsing with spring request mapping methods.
     * @param activityRole a string of "admin" or "user" profile's permission that it needs to be edited to.
     */
    public ActivityRoleRequest(@JsonProperty("activityRole") String activityRole) { this.activityRole = activityRole; }

    public String getActivityRole() {
        return activityRole;
    }
    public void setRole(String activityRole) {
        this.activityRole = activityRole;
    }
}
