package com.springvuegradle.service;

import com.springvuegradle.Controller.enums.ActivityResponseMessage;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.Utilities.FormatHelper;
import org.junit.jupiter.api.AfterEach;
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
    ActivityTypeRepository typeRepository;
    private Map<Long, Profile> profileIds = null;
    private static final String MISSING_EXCEPTION = "Exception should have been thrown.";

    @BeforeEach
    void setUp() {
        profileIds = populateProfiles();
        populateActivityTypes();
    }

    @AfterEach
    void tearDown() {
        profileRepository.deleteAll();
        activityRepository.deleteAll();
        typeRepository.deleteAll();
    }
/*
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
            assertEquals(expectedMessage.toString(), e.getMessage());
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
            assertEquals(expectedMessage.toString(), e.getMessage());
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
            assertEquals(expectedMessage.toString(), e.getMessage());
        }
    }

    @Test
    void updateActivityWithBlankNameDoesNotSaveDataTest() {
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        actualActivity = updateAndGetResult(expectedActivity, createBadActivityBlankName());
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithDurationAndNoStartDateThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.MISSING_START_DATE;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityDurationAndNoStartDate(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(expectedMessage.toString(), e.getMessage());
        }
    }

    @Test
    void updateActivityWithDurationAndNoStartDateDoesNotChangeData() {
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        actualActivity = updateAndGetResult(expectedActivity, createBadActivityBlankName());
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithDurationAndNoEndDateThrowsException() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.MISSING_END_DATE;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityDurationAndNoEndDate(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(expectedMessage.toString(), e.getMessage());
        }
    }

    @Test
    void updateActivityWithDurationAndNoEndDateDoesNotChangeData() {
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        actualActivity = updateAndGetResult(expectedActivity, createBadActivityDurationAndNoEndDate());
        assertEquals(expectedActivity, actualActivity);
    }


    @Test
    void updateActivityWithMisorderedDateThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ActivityResponseMessage expectedMessage = ActivityResponseMessage.INVALID_DATES;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        try {
            service.update(createBadActivityMisorderedDates(), activityId);
            fail(MISSING_EXCEPTION);
        } catch (Exception e) {
            assertEquals(expectedMessage.toString(), e.getMessage());
        }
    }

    @Test
    void updateActivityWithMisorderedDateDoesNotSaveDataTest() {
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        actualActivity = updateAndGetResult(expectedActivity, createBadActivityMisorderedDates());
        assertEquals(expectedActivity, actualActivity);
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
            assertEquals(expectedMessage.toString(), e.getMessage());
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
            assertEquals(expectedMessage.toString(), e.getMessage());
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
            assertEquals(expectedMessage.toString(), e.getMessage());
        }
    }

    @Test
    void updateActivityWithNoActivityTypesDoesNotSaveDataTest() {
        Activity expectedActivity = createNormalActivitySilly();
        Activity actualActivity = updateAndGetResult(expectedActivity, createBadActivityNoActivityTypes());
        assertEquals(expectedActivity, actualActivity);
    }

 */



    private Activity createNormalActivityKaikoura() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"tramping", "hiking"}, false, "2020-02-20T08:00:00+1300",
                "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private Activity createNormalActivitySilly() {
        return new Activity("Wibble", "A bald man", new String[]{"hockey"}, true,
                "2020-02-20T08:00:00+1300","2020-02-20T08:00:00+1300", "K2");
    }

    private Activity createBadActivityNoName() {
        Activity activity = createNormalActivityKaikoura();
        activity.setActivityName(null);
        return activity;
    }

    private Activity createBadActivityBlankName() {
        Activity activity = createNormalActivityKaikoura();
        activity.setActivityName("");
        return activity;
    }

    private Activity createBadActivityDurationAndNoStartDate() {
        Activity activity = createNormalActivityKaikoura();
        activity.setStartTime(null);
        return activity;
    }

    private Activity createBadActivityDurationAndNoEndDate() {
        Activity activity = createNormalActivityKaikoura();
        activity.setEndTime(null);
        return activity;
    }

    private Activity createBadActivityMisorderedDates() {
        Activity activity = createNormalActivityKaikoura();
        activity.setEndTime(FormatHelper.parseOffsetDateTime("2020-01-20T08:00:00+1300"));
        return activity;
    }

    private Activity createBadActivityNoActivityTypes() {
        return new Activity("", "A big and nice race on a lovely peninsula",null, false,
                "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private Activity createBadActivityEmptyActivityTypes() {
        return new Activity("", "A big and nice race on a lovely peninsula", new String[]{},
                false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private Activity createBadActivityInvalidActivityTypes() {
        return new Activity("", "A big and nice race on a lovely peninsula", new String[]{"nugts"},
                false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private void populateActivityTypes() {
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

    static Profile createNormalProfileJohnny() {
        return new Profile(null, "Johnny", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileMim() {
        return new Profile(null, "Mim", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }

    private Map<Long, Profile> populateProfiles() {
        Profile johnny = createNormalProfileJohnny(),
                mim = createNormalProfileMim();
        Map<Long, Profile> map = new HashMap<>();
        profileRepository.save(johnny);
        map.put(profileRepository.getLastInsertedId(), johnny);
        profileRepository.save(mim);
        map.put(profileRepository.getLastInsertedId(), mim);
        return map;
    }
}