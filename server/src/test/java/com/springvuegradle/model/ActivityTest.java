package com.springvuegradle.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Calendar;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class ActivityTest {
    private Profile profile;
    private Activity activity;
    private final String[] passportCountries = {"USA", "UK"};
    private final String[] extraEmails = {"throwayway1@gmail.com", "throwaway2@gmail.com"};
    private final String[] activityTypes = {"Walking", "Running", "Jogging"};

    @BeforeEach
    void setUp() {
        Calendar calendar1 = new GregorianCalendar(2000, 11, 5);
        this.profile = new Profile(1L, "Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar1,
                "Male", 2, passportCountries, activityTypes);
        this.activity = new Activity("City to Surf", "15km walk/jog/run.", activityTypes, true,
                "2020-02-20T08:00:00+1300","2020-02-20T08:00:00+1300", "Christchurch", 100.0, 100.0);
    }

    /**
     * Testing the retrieveCreator method.
     */
    @Test
    void testRetrieveCreatorReturnsNullWhereNoCreatorExists(){
        assertNull(activity.retrieveCreator());
    }

    @Test
    void testRetrieveCreatorReturnsCreatorWhereCreatorExists(){
        activity.addMember(new ActivityMembership(activity, profile, ActivityMembership.Role.CREATOR));
        assertEquals(profile, activity.retrieveCreator());
    }

    /**
     * Testing the getCreator method.
     */
    @Test
    void testGetCreatorReturnsNullWhereNoCreatorExists(){
        assertNull(activity.getCreator());
    }

    @Test
    void testGetCreatorReturnsCreatorFullNameWhereCreatorExists(){
        activity.addMember(new ActivityMembership(activity, profile, ActivityMembership.Role.CREATOR));
        assertEquals(profile.getFullName(), activity.getCreator());
    }
}
