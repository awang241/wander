package com.springvuegradle.Model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Calendar;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

class ProfileTest {

    private Profile profile;

    @BeforeEach
    void setup() {
        profile = new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"});
    }

    @Test
    void addEmailToProfile() {

        boolean added = profile.addEmail(new Email("jimmy@edu.com"));
        assertTrue(added);

    }

    @Test
    void addMoreThan5EmailsToProfile() {
        assertEquals(2, profile.retrieveEmails().size());
        boolean added = profile.addEmail(new Email("jimmy@edu.com"));
        assertTrue(added);
        assertEquals(3, profile.retrieveEmails().size());
        added = profile.addEmail(new Email("jimmy1@edu.com"));
        assertTrue(added);
        assertEquals(4, profile.retrieveEmails().size());
        added = profile.addEmail(new Email("jimmy2@edu.com"));
        assertTrue(added);
        assertEquals(5, profile.retrieveEmails().size());

        // this next email should be rejected as the max number of emails has been reached.
        added = profile.addEmail(new Email("jimmy3@edu.com"));
        assertFalse(added);
        assertEquals(5, profile.retrieveEmails().size());
    }

    @Test
    void addDuplicateEmailsToProfile() {
        assertEquals(2, profile.retrieveEmails().size());
        boolean added = profile.addEmail(new Email("jimmy@edu.com"));
        assertTrue(added);
        assertEquals(3, profile.retrieveEmails().size());

        // this next email should be rejected as it has the same address as the last email added.
        added = profile.addEmail(new Email("jimmy@edu.com"));
        assertFalse(added);
        assertEquals(3, profile.retrieveEmails().size());
    }

    @Test
    void removeEmailFromProfile() {
    }

    @Test
    void changePrimaryEmail() {
    }

    @Test
    void addPassportCountry() {
    }

    @Test
    void removePassportCountry() {
    }
}