package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.OffsetDateTime;

/**
 * DTO for posting the details of activity participation
 */
@Data
public class ActivityParticipationRequest {
    private String outcome;
    private String details;
    private String startTime;
    private String endTime;

    /**
     * Constructor for a ActivityParticipationRequest
     * Constructor parameters come from front-end
     * @param outcome a string detailing the outcome of the users participation
     * @param details a string containing specific details of the users participation
     * @param startTime the time the user started their participation
     * @param endTime the time the user ended their participation
     */
    public ActivityParticipationRequest(String details,
                                        String outcome,
                                        String startTime,
                                        String endTime) {
        this.details = details;
        this.outcome = outcome;
        this.startTime = startTime;
        this.endTime = endTime;
    }
}