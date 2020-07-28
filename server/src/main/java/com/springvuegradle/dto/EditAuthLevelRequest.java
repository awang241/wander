package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.springvuegradle.enums.AuthLevel;

public class EditAuthLevelRequest {
    private String role;

    /**
     * Constructor for a EditAuthLevelRequest with a parameter for JSON parsing with spring requestmapping methods.
     * @param role a string of "admin" or "user" profile's permission that it needs to be edited to.
     */
    public EditAuthLevelRequest(@JsonProperty("role") String role) { this.role = role; }

    public String getRole() {
        return role;
    }
    public void setRole(String role) {
        this.role = role;
    }
}
