package com.springvuegradle.dto.responses;

import java.util.List;

public class ActivityTypesResponse {

    private List<String> allActivityTypes;

    /**
     * Constructor for a ActivityTypesResponse with parameters. for JSON parsing with spring requestmapping methods.
     * @param activityTypes list containing strings of all the activityTypes listed in the database
     */
    public ActivityTypesResponse(List<String> activityTypes) {
        this.allActivityTypes = activityTypes;
    }

    public List<String> getAllActivityTypes() {
        return allActivityTypes;
    }
}
