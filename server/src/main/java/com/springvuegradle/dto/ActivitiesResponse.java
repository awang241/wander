package com.springvuegradle.dto;

import java.util.List;

/**
 * DTO object to return data to client upon a successful GET request for activities.
 */
public class ActivitiesResponse {

    private List<String> allActivities;

    /**
     * Constructor for a ActivitiesResponse with parameters. for JSON parsing with spring requestmapping methods.
     * @param activities list containing strings of all the activities listed in the database
     */
    public ActivitiesResponse(List<String> activities) {
        this.allActivities = activities;
    }

    /**
     * Returns all
     * @return
     */
    public List<String> getAllActivities() {
        return allActivities;
    }
}
