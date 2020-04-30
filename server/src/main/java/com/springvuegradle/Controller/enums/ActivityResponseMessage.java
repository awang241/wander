package com.springvuegradle.Controller.enums;

public enum ActivityResponseMessage {
    EDIT_SUCCESS("Activity edited successfully"),
    MISSING_NAME("Activity name is blank or missing"),
    MISSING_TYPES("An activity must have at least one activity type"),
    INVALID_TYPE("Given activity types do not exist in the database"),
    INVALID_ACTIVITY("No activity with that ID exists in the database"),
    INVALID_FIELDS("Invalid fields");

    private String message;

    private ActivityResponseMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return message;
    }
}
