package com.springvuegradle.Utilities;

import org.junit.jupiter.api.Test;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.temporal.TemporalAccessor;

import static org.junit.jupiter.api.Assertions.*;

class FormatHelperTest {

    @Test
    void parseOffsetDateTimeTest() {
        String dateText = "2020-02-20T08:00:00+1300";
        TemporalAccessor expectedTime = OffsetDateTime.of(2020, 2, 20, 8, 0,
                                            0, 0, ZoneOffset.ofHours(13));
        assertEquals(expectedTime, FormatHelper.parseOffsetDateTime(dateText));
    }

}