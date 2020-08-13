package com.springvuegradle.enums;

public enum ParticipationErrorMessage {
    PARTICIPATION_NOT_FOUND("No participation with that ID exists");


    private final String message;

    ParticipationErrorMessage(String message) {
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
