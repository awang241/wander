package com.springvuegradle.Controller.enums;

public enum ActivityResponseMessage {
    EDIT_SUCCESS("Activity edited successfully"),
    MISSING_NAME("Activity name is blank or missing"),
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
