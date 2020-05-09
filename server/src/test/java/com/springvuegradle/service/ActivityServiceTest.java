package com.springvuegradle.service;

import com.springvuegradle.Controller.ActivityController;
import com.springvuegradle.Controller.enums.ActivityResponseMessage;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.Utilities.FormatHelper;
import com.springvuegradle.Utilities.InitialDataHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
    @Autowired
    ActivityMembershipRepository activityMembershipRepository;
    @Autowired
    EmailRepository emailRepository;

    private Map<Long, Profile> profileIds = null;
    private static final String MISSING_EXCEPTION = "Exception should have been thrown.";

    @BeforeEach
    void setUp() {
        profileIds = populateProfiles();
        InitialDataHelper.init(typeRepository, profileRepository, emailRepository);
    }

    @AfterEach
    void tearDown() {
        activityMembershipRepository.deleteAll();
        profileRepository.deleteAll();
        activityRepository.deleteAll();
        typeRepository.deleteAll();
    }

//    @Test
//    void testCreateNewActivity() {
//        Profile ben = createNormalProfileBen();
//        Profile profile = profileRepository.save(ben);
//
//        Activity trackRace = createNormalActivity();
//        service.create(trackRace, profile.getId());
//        List<Activity> result = activityRepository.findByActivityNames(trackRace.getActivityName());
//
//        assertEquals(result.get(0).getActivityName(), "Kaikoura Coast Track race");
//
//        List<ActivityType> activityTypeList = typeRepository.findByActivityTypeName("tramping");
//        ActivityType activityType = activityTypeList.get(0);
//        System.out.println(activityType.getActivityTypeName());
//        assertEquals(1, activityType.getActivities().size());
//    }


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
    void updateActivityWithNoNameTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityNoName(), activityId );});
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
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createNormalActivityKaikoura(), 0L);});
    }

    @Test
    void updateActivityWithBlankNameTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityBlankName(), activityId );});
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithDurationAndNoStartDateTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityDurationAndNoStartDate(), activityId );});
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithDurationAndNoEndDateTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityDurationAndNoEndDate(), activityId );});
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }


    @Test
    void updateActivityWithMisorderedDateThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityBlankName(), activityId );});
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithInvalidActivityTypesThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityInvalidActivityTypes(), activityId );});
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithEmptyActivityTypesTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityEmptyActivityTypes(), activityId );});
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithNoActivityTypesTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createBadActivityNoActivityTypes(), activityId );});
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void deleteActivitySuccess() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        assertEquals(1, activityRepository.count());
        assertTrue(service.delete(activity.getId()));
        assertEquals(0, activityRepository.count());
        assertEquals(7, typeRepository.count());
    }

    @Test
    void deleteActivityDoesNotExist() {
        assertFalse(activityRepository.existsById((long) 1));
        assertFalse(service.delete((long) 1));
    }



    static Activity createNormalActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Tramping", "Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private Activity createNormalActivityKaikoura() {
        Activity activity =  new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Hiking"}, false, "2020-02-20T08:00:00+1300",
                "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
        Set<ActivityType> updatedActivityType = new HashSet<ActivityType>();
        for(ActivityType activityType : activity.getActivityTypes()){
            List<ActivityType> resultActivityTypes = typeRepository.findByActivityTypeName(activityType.getActivityTypeName());{
                updatedActivityType.add(resultActivityTypes.get(0));
            }
        }
        activity.setActivityTypes(updatedActivityType);
        return activity;
    }

    private Activity createNormalActivitySilly() {
        return new Activity("Wibble", "A bald man", new String[]{"Hockey"}, true,
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


    static Profile createNormalProfileBen() {
        return new Profile(null, "Ben", "Sales", "James", "Ben10", "ben10@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "Wooooooow", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
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