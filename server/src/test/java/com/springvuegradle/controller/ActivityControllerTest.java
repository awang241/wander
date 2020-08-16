package com.springvuegradle.controller;
import com.springvuegradle.dto.SimplifiedActivitiesResponse;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityType;
import com.springvuegradle.model.Profile;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import com.springvuegradle.utilities.InitialDataHelper;
import com.springvuegradle.service.ActivityService;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static com.springvuegradle.controller.Profile_Controller.hashPassword;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the ActivityController class, these tests are run separately from the actual repository.
 */
@ExtendWith(SpringExtension.class)
@DataJpaTest
public class ActivityControllerTest {

    @Autowired
    private ActivityRepository arepo;

    @Autowired
    private ActivityMembershipRepository amRepo;

    @Autowired
    private ProfileRepository prepo;

    @Autowired
    private EmailRepository erepo;

    @Autowired
    private ActivityController activityController;

    @Autowired
    private LoginController loginController;

    private ActivityService mockService;

    @Autowired
    private ActivityTypeRepository activityTypeRepo;

    /**
     * Needs to be run before each test to ensure the repository starts empty.
     */
    @BeforeEach
    void setup() {
        amRepo.deleteAll();
        erepo.deleteAll();
        prepo.deleteAll();
        arepo.deleteAll();
        activityTypeRepo.deleteAll();

        InitialDataHelper.init(activityTypeRepo, prepo, erepo);
    }

    /**
     * This tests to ensure activities structured correctly returns the correct response
     */
    @Test
    void createActivityResponseTest() {
        Activity trackRace = createNormalActivity();
        Profile maurice = createNormalProfileMaurice();
        Profile profile = prepo.save(maurice);

        ResponseEntity<String> response_entity = activityController.createActivity(profile.getId(), trackRace, null, true);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());
    }

    /**
     * This tests to ensure activities structured correctly can be added to the database
     */
    @Test
    void createActivityTest() {
        Activity trackRace = createNormalActivity();
        Profile maurice = createNormalProfileMaurice();
        Profile profile = prepo.save(maurice);
        activityController.createActivity(profile.getId(), trackRace, null, true);

        assertEquals(1, arepo.findByActivityNames("Kaikoura Coast Track race").get(0).retrieveActivityTypes().size());
    }

    /**
     * This tests to ensure activities structured correctly can be added and checks the database sizes one activity
     */
    @Test
    void createActivitySizeCheckTest() {
        Activity trackRace = createNormalActivity();
        Profile maurice = createNormalProfileMaurice();
        Profile profile = prepo.save(maurice);
        activityController.createActivity(profile.getId(), trackRace, null, true);
        Activity activity = arepo.findAll().get(0);

        int expected_in_repo = 1;
        assertEquals(expected_in_repo, arepo.count());
    }

    /**
     * This tests to check incorrectly structured activities cannot be added to the database
     */
    @Test
    void createIncorrectActivityTest() {
        Activity trackRace = createIncorrectActivity();
        Profile profile = prepo.save(createNormalProfileMaurice());
        activityController.createActivity(profile.getId(), trackRace, null, true);

        assertEquals(0, arepo.count());
    }

    /**
     * This tests to check incorrectly structured activities cannot be added to the database and returns a
     * forbidden response
     */
    @Test
    void createIncorrectActivityResponseTest() {
        Activity trackRace = createIncorrectActivity();
        Profile profile = prepo.save(createNormalProfileMaurice());
        ResponseEntity<String> response_entity = activityController.createActivity(profile.getId(), trackRace, null, true);

        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());
    }

    /**
     * Check if a test array full of the same activities as saved in the database.
     */
    @Test
    void checkActivitiesMatchTest() {
        List<String> testActivities = Arrays.asList("Kaikoura Coast Track race", "Triathlon");
        int i = 0;
        arepo.save(createNormalActivity());
        arepo.save(createNormalActivity1());
        ResponseEntity<List<Activity>> responseEntity = activityController.getActivities(null, null);
        for (Activity activity: responseEntity.getBody()) {
            assertEquals(testActivities.get(i++), activity.getActivityName());
        }
    }

    /**
     * Tests the repsonse of deleting a activity, blue sky scenario where the activity exists.
     */
    @Test
    void deleteActivityTestResponseExists() {
        Activity trackRace = createNormalActivity();
        Profile profile = prepo.save(createNormalProfileMaurice());
        activityController.createActivity(profile.getId(), trackRace, null, true);
        ResponseEntity<String> response_entity2 = activityController.deleteActivity(null, profile.getId(), trackRace.getId(), true);

        assertEquals(HttpStatus.OK, response_entity2.getStatusCode());
    }

    /**
     * Tests deleting a activity, blue sky scenario where the activity exists.
     */
    @Test
    void deleteActivityTestExists() {
        Activity trackRace = createNormalActivity();
        Profile profile = prepo.save(createNormalProfileMaurice());
        activityController.createActivity(profile.getId(), trackRace, null, true);
        activityController.deleteActivity(null, profile.getId(), trackRace.getId(), true);

        assertEquals(0, prepo.findAll().get(0).getActivities().size());
    }

    /**
     * Tests response of deleting a activity where the activity does not exist.
     */
    @Test
    void deleteActivityDoesNotExistResponseTest() {
        Profile profile = prepo.save(createNormalProfileMaurice());
        ResponseEntity<String> response_entity2 = activityController.deleteActivity(null, profile.getId(), (long) 1, true);
        assertEquals(HttpStatus.NOT_FOUND, response_entity2.getStatusCode());
    }

    /**
     * Check if the right amount of activities are saved in the database.
     */
    @Test
    void checkActivitiesCount() {
        arepo.deleteAll();
        arepo.save(createNormalActivity());
        arepo.save(createNormalActivity1());
        int expected_activities_in_repo = 2;
        assertEquals(expected_activities_in_repo, arepo.count());
        arepo.deleteAll();
    }

    /**
     * Tests the response to get endpoint for activities list
     */
    @Test
    void getActivitiesResponseTest() {
        arepo.save(createNormalActivity());
        arepo.save(createNormalActivity1());
        ResponseEntity<List<Activity>> responseEntity = activityController.getActivities(null, null);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Tests the get endpoint for activities list
     */
    @Test
    void getActivitiesTest() {
        arepo.save(createNormalActivity());
        arepo.save(createNormalActivity1());
        ResponseEntity<List<Activity>> responseEntity = activityController.getActivities(null, null);
        assertEquals(2, responseEntity.getBody().size());
    }

    /* Below are a set of ready-made Activity objects which can be used for various tests. */

    /**
     * @return a valid activity object.
     */
    static Activity createNormalActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    static Activity createNormalActivity1() {
        return new Activity("Triathlon", "I hate triathlons",
                new String[]{"Hiking","Football"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    static Activity createIncorrectActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Tramping","Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-01-20T08:00:00+1300", "Kaikoura, NZ");
    }

    static ActivityType createActivityType() {
        return new ActivityType("Hiking");
    }

    static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }
    static Profile createNormalProfileMauriceWithHashedPassword() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, hashPassword("jacky'sSecuredPwd"),
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }
    static Profile createNormalProfileJohnny() {
        return new Profile(null, "Johnny", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }
}
