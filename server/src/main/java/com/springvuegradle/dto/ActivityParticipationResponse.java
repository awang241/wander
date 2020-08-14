package com.springvuegradle.dto;

import com.springvuegradle.model.ActivityParticipation;
import lombok.Data;

@Data
public class ActivityParticipationResponse {
    private String errorMessage;
    private ActivityParticipation participation;

    /**
     * Creates an ActivityParticipationResponse object containing just an error message to be sent to the frontend.
     * @param errorMessage the error message detailing what error has occurred.
     */
    public ActivityParticipationResponse(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    /**
     * Creates a ActivityParticipationResponse object containing the ActivityParticipation object for when an
     * ActivityParticipation object has been successfully found.
     * @param participation the ActivityParticipation object.
     */
    public ActivityParticipationResponse(ActivityParticipation participation) {
        this.participation = participation;
    }
}
