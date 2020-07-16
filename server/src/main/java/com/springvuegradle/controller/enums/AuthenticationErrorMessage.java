package com.springvuegradle.controller.enums;

public enum AuthenticationErrorMessage {
    AUTHENTICATION_REQUIRED("Authentication required."),
    INVALID_CREDENTIALS("Invalid session key.");

    private String message;

    private AuthenticationErrorMessage(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}