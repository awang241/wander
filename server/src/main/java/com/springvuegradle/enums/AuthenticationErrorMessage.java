package com.springvuegradle.enums;

public enum AuthenticationErrorMessage {
    AUTHENTICATION_REQUIRED("Authentication required."),
    INVALID_CREDENTIALS("Invalid session key.");

    private String message;

    private AuthenticationErrorMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return message;
    }

    public String getMessage() {
        return message;
    }
}