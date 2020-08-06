package com.springvuegradle.enums;

public enum ProfileErrorMessage {
    INVALID_SEARCH_COUNT("Count must be a positive integer"),
    INVALID_AUTH_LEVEL("Auth level must be between 1 and 5"),
    PROFILE_NOT_FOUND("No profile with that ID exists"),
    INVALID_ROLE("Role type is invalid");

    private final String message;
    ProfileErrorMessage(String message) {
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
