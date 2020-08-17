package com.springvuegradle.dto;

import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityParticipation;

import java.util.List;

/**
 * DTO object to return data to client upon a successful GET request for all of a users
 * participation of an activity.
 */
public class ActivityParticipationSummariesResponse {

    private List<ActivityParticipation> results;
    String message;

    /**
     * Constructor for a ActivityParticipationListResponse with parameters. for JSON parsing with spring requestmapping methods.
     * @param results list containing strings of all the user's participation of an activity listed in the database
     */
    public ActivityParticipationSummariesResponse(List<ActivityParticipation> results){
        this.results = results;
        this.message = null;
    }

    public ActivityParticipationSummariesResponse(String message) {
        this.results = null;
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public List<ActivityParticipation> getResults() {
        return results;
    }
    /**
     * Returns all of a users participation of an activity.
     * @return
     */
    public List<ActivityParticipation> getAllActivityParticipation() {
        return results;
    }

}
