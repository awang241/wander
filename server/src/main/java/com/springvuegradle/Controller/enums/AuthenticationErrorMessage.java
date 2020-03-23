package com.springvuegradle.Controller.enums;

public enum AuthenticationErrorMessage {
    INVALID_CREDENTIALS("Invalid session key.");

    private String message;

    private AuthenticationErrorMessage(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}
