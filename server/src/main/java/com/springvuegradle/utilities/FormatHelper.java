package com.springvuegradle.utilities;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.TemporalAccessor;

public class FormatHelper {

    private static final DateTimeFormatter DATE_TIME_FORMATTER = new DateTimeFormatterBuilder()
            .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
            .appendPattern("X")
            .toFormatter();

    private FormatHelper() {}

    public static OffsetDateTime parseOffsetDateTime(String text) {
        if (text == null) {
            return null;
        } else {
            TemporalAccessor rawTime = DATE_TIME_FORMATTER.parse(text);
            return OffsetDateTime.from(rawTime);
        }
    }

}
