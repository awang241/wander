package com.springvuegradle.enums;

public enum ProfileErrorMessage {
    INVALID_SEARCH_COUNT("Count must be a positive integer");

    private String message;
    ProfileErrorMessage(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public String toString() {
        return "ProfileErrorMessage{" +
                "message='" + message + '\'' +
                '}';
    }
}
