package com.springvuegradle.service;

import com.springvuegradle.Controller.enums.ActivityResponseMessage;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.service.ActivityService;
import org.hibernate.type.CalendarTimeType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivityServiceTest {

    @Autowired
    ActivityService service;
    @Autowired
    ProfileRepository profileRepository;
    @Autowired
    ActivityRepository activityRepository;
    @Autowired
    static ActivityTypeRepository typeRepository;
    private Map<Long, Profile> profileIds = null;
    private static final String MISSING_EXCEPTION = "Exception should have been thrown.";

    @BeforeAll
    static void setUpBeforeClass() {
        populateActivityTypes();
    }

    @BeforeEach
    void setUp() {
        profileIds = populateProfiles();
    }

    @AfterEach
    void tearDown() {
        profileRepository.deleteAll();
        activityRepository.deleteAll();
    }

    @Test
    void updateActivityWithNormalDataSavesActivityTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivityKaikoura(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        service.update(expectedActivity, activityId);
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithNoNameThrowsExceptionCorrectly() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.MISSING_NAME;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityNoName(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(e.getMessage(), expectedMessage.toString());
        }
    }

    @Test
    void updateActivityWithNoNameDoesNotSaveDataTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        service.update(createBadActivityNoName(), activityId);
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityNotInDatabaseThrowsException() {
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.INVALID_ACTIVITY;
        try {
            service.update(createNormalActivityKaikoura(), 0L);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(e.getMessage(), expectedMessage.toString());
        }
    }

    @Test
    void updateActivityWithBlankNameThrowsExceptionCorrectly() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.MISSING_NAME;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityBlankName(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(e.getMessage(), expectedMessage.toString());
        }
    }

    @Test
    void updateActivityWithBlankNameDoesNotSaveDataTest() {
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        actualActivity = updateAndGetResult(expectedActivity, createBadActivityBlankName());
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithInvalidDateThrowsExceptionTest() {
        fail("not implemented");
    }

    @Test
    void updateActivityWithInvalidDateDoesNotSaveDataTest() {
        fail("not implemented");
    }

    @Test
    void updateActivityWithInvalidActivityTypesThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.MISSING_NAME;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityInvalidActivityTypes(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(e.getMessage(), expectedMessage.toString());
        }
    }

    @Test
    void updateActivityWithInvalidActivityDoesNotSaveDataTest() {
        Activity expectedActivity = createNormalActivitySilly();
        Activity actualActivity = updateAndGetResult(expectedActivity, createBadActivityInvalidActivityTypes());
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithEmptyActivityTypesThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.MISSING_TYPES;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityEmptyActivityTypes(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(e.getMessage(), expectedMessage.toString());
        }
    }

    @Test
    void updateActivityWithEmptyActivityTypesDoesNotSaveDataTest() {
        Activity expectedActivity = createNormalActivitySilly();
        Activity actualActivity = updateAndGetResult(expectedActivity, createBadActivityEmptyActivityTypes());
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithNoActivityTypesThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.MISSING_TYPES;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityNoActivityTypes(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(e.getMessage(), expectedMessage.toString());
        }
    }

    @Test
    void updateActivityWithNoActivityTypesDoesNotSaveDataTest() {
        Activity expectedActivity = createNormalActivitySilly();
        Activity actualActivity = updateAndGetResult(expectedActivity, createBadActivityNoActivityTypes());
        assertEquals(expectedActivity, actualActivity);
    }



    private Activity createNormalActivityKaikoura() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"tramping", "hiking"}, false, start, end, "Kaikoura, NZ");
    }

    private Activity createNormalActivitySilly() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("Wibble", "A bald man",
                new String[]{"hockey"}, true, start, end, "K2");
    }

    private Activity createBadActivityNoName() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity(null, "A big and nice race on a lovely peninsula",
                new String[]{"tramping", "hiking"}, false, start, end, "Kaikoura, NZ");
    }

    private Activity createBadActivityBlankName() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("", "A big and nice race on a lovely peninsula",
                new String[]{"tramping", "hiking"}, false, start, end, "Kaikoura, NZ");
    }

    private Activity createBadActivityNoActivityTypes() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("", "A big and nice race on a lovely peninsula",
                null, false, start, end, "Kaikoura, NZ");
    }

    private Activity createBadActivityEmptyActivityTypes() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("", "A big and nice race on a lovely peninsula",
                new String[]{}, false, start, end, "Kaikoura, NZ");
    }

    private Activity createBadActivityInvalidActivityTypes() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("", "A big and nice race on a lovely peninsula",
                new String[]{"nugts"}, false, start, end, "Kaikoura, NZ");
    }

    private static void populateActivityTypes() {
        typeRepository.save(new ActivityType("football"));
        typeRepository.save(new ActivityType("tennis"));
        typeRepository.save(new ActivityType("hockey"));
        typeRepository.save(new ActivityType("basketball"));
        typeRepository.save(new ActivityType("hiking"));
        typeRepository.save(new ActivityType("tramping"));
        typeRepository.save(new ActivityType("rock climbing"));
    }

    /**
     * Saves original activity to repo, then applies update and returns the updated activity.
     * @param original the original activity to save to the repo
     * @param update the update to be applied to the original activity
     * @return The updated activity from repository
     */
    private Activity updateAndGetResult(Activity original, Activity update) {
        activityRepository.save(original);
        Long activityId = activityRepository.getLastInsertedId();
        Activity actualActivity = null;

        service.update(update, activityId);
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        return actualActivity;
    }

    static Profile createNormalProfileJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{});
    }

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{"New Zealand", "China"}, new String[]{});
    }

    private Map<Long, Profile> populateProfiles() {
        Profile jimmy = createNormalProfileJimmy(),
                maurice = createNormalProfileMaurice();
        Map<Long, Profile> map = new HashMap<>();
        profileRepository.save(jimmy);
        map.put(profileRepository.getLastInsertedId(), jimmy);
        profileRepository.save(maurice);
        map.put(profileRepository.getLastInsertedId(), maurice);
        return map;
    }
}