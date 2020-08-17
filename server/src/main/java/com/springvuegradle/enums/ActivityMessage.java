package com.springvuegradle.enums;

public enum ActivityMessage {
    SUCCESSFUL_CREATION("Successfully added new role."),
    SUCCESSFUL_DELETION("Role was successfully deleted from the activity"),
    ACTIVITY_NOT_FOUND("No activity with that ID exists"),
    INSUFFICIENT_PERMISSION("You do not have permission to do this action!"),
    EDITING_CREATOR("Editing the creator of an activity is not allowed!"),
    PROFILE_NOT_FOUND("No profile with that ID exists"),
    MEMBERSHIP_NOT_FOUND("The profiles membership was not found for this activity"),
    INVALID_ROLE("Role type is invalid"),
    UNSUCCESSFUL("Unknown error occurred."),
    PARTICIPATION_NOT_FOUND("No participation with that ID exists"),
    INVALID_PRIVACY("Invalid privacy level");


    private final String message;
    ActivityMessage(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public String toString() {
        return "ActivityMessage{" +
                "message='" + message + '\'' +
                '}';
    }
}
