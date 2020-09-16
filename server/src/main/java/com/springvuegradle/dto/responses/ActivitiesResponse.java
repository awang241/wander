package com.springvuegradle.dto.responses;

import com.springvuegradle.model.Activity;

import java.util.List;

/**
 * DTO object to return data to client upon a successful GET request for activities.
 */
public class ActivitiesResponse {

    private List<Activity> results;
    String message;

    /**
     * Constructor for a ActivitiesResponse with parameters. for JSON parsing with spring requestmapping methods.
     * @param results list containing strings of all the activities listed in the database
     */
    public ActivitiesResponse(List<Activity> results) {
        this.results = results;
        this.message = null;
    }

    public ActivitiesResponse(String errorMessage) {
        this.results = null;
        this.message = errorMessage;
    }

    public String getMessage() { return message; }

    public void setMessage(String message) { this.message = message; }

    /**
     * Returns all
     * @return
     */
    public List<Activity> getAllActivities() {
        return results;
    }
}
