package com.springvuegradle.enums;

public enum ActivityParticipationMessage {
    PARTICIPATION_NOT_FOUND("No participation with that ID exists"),
    EDIT_SUCCESS("Participation edited successfully");

    private final String message;

    ActivityParticipationMessage(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public String toString() {
        return message;
    }
}
