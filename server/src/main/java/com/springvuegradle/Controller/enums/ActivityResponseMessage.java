package com.springvuegradle.Controller.enums;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public enum ActivityResponseMessage {
    EDIT_SUCCESS("Activity edited successfully"),
    MISSING_END_DATE("Non-continuous activities must have an end date"),
    MISSING_NAME("Activity name is blank or missing"),
    MISSING_START_DATE("Non-continuous activities must have a start date"),
    MISSING_TYPES("An activity must have at least one activity type"),
    INVALID_ACTIVITY("No activity with that ID exists in the database"),
    INVALID_DATES("The end date cannot be before the start date"),
    INVALID_FIELDS("Invalid fields"),
    INVALID_TYPE("Given activity types do not exist in the database");

    private String message;

    public static final Set<String> SEMANTIC_ERRORS = new HashSet<>(Arrays.asList(INVALID_ACTIVITY.toString(),
                                                                                    INVALID_DATES.toString(),
                                                                                    INVALID_TYPE.toString()));
    public static final Set<String> SYNTAX_ERRORS  = new HashSet<>(Arrays.asList(MISSING_END_DATE.toString(),
                                                                                MISSING_NAME.toString(),
                                                                                MISSING_START_DATE.toString(),
                                                                                MISSING_TYPES.toString()));

    private ActivityResponseMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return message;
    }
}
