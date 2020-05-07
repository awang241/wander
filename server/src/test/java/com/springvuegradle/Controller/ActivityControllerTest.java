package com.springvuegradle.Controller;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.service.ActivityService;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

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
    private ActivityController activityController;

    private ActivityService mockService;

    @Autowired
    private ActivityTypeRepository activityTypeRepo;


    @AfterEach
    void resetRepo() {
        amRepo.deleteAll();
        prepo.deleteAll();
        arepo.deleteAll();

    }

    /**
     * Needs to be run before each test to ensure the repository starts empty.
     */
    @BeforeEach
    void setup() {
        amRepo.deleteAll();
        prepo.deleteAll();
        arepo.deleteAll();
        activityTypeRepo.deleteAll();
        activityTypeRepo.save(new ActivityType("Football"));
        activityTypeRepo.save(new ActivityType("Tennis"));
        activityTypeRepo.save(new ActivityType("Hockey"));
        activityTypeRepo.save(new ActivityType("Basketball"));
        activityTypeRepo.save(new ActivityType("Hiking"));
        activityTypeRepo.save(new ActivityType("Rock Climbing"));
    }


    /**
     * This tests to ensure activities structured correctly can be added to the database.
     */
    @Test
    void createActivityTest() {
        Activity trackRace = createNormalActivity();
        Profile maurice = createNormalProfileMaurice();
        Profile profile = prepo.save(maurice);

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, arepo.count());

        ResponseEntity<String> response_entity = activityController.createActivity(profile.getId(), trackRace, null, true);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, arepo.count());
    }

    /**
     * This tests to ensure activities structured correctly can be added to the database.
     */

    @Test
    void createIncorrectActivityTest() {
        Activity trackRace = createIncorrectActivity();
        Profile profile = prepo.save(createNormalProfileMaurice());
        assertEquals(0, arepo.count());
        ResponseEntity<String> response_entity = activityController.createActivity(profile.getId(), trackRace, null, true);
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());
        assertEquals(0, arepo.count());
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
        ResponseEntity<List<Activity>> response_entity = activityController.getActivitiesList();
        for (Activity activity: response_entity.getBody()) {
            assertEquals(testActivities.get(i++), activity.getActivityName());
        }
    }


    /**
     * Tests deleting a activity, blue sky scenario where the activity exists.
     */
    @Test
    void deleteActivityTestExists() {
        Activity trackRace = createNormalActivity();
        Profile profile = prepo.save(createNormalProfileMaurice());
        ResponseEntity<String> response_entity = activityController.createActivity(profile.getId(), trackRace, null, true);
        assertEquals(1, arepo.count());
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());
        ResponseEntity<String> response_entity2 = activityController.deleteActivity(null, profile.getId(), trackRace.getId(), true);
        assertEquals(HttpStatus.OK, response_entity2.getStatusCode());
        assertEquals(0, arepo.count());
        assertEquals(0, prepo.findAll().get(0).getActivities().size());
    }

    /**
     * Tests deleting a activity where the activity does not exist.
     */
    @Test
    void deleteActivityTestDoesNotExist() {
        Profile profile = prepo.save(createNormalProfileMaurice());
        assertEquals(0, arepo.count());
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

    @Disabled
    @Test
    void updateActivityNormalRequest() {
        fail("Not yet implemented");
    }

    /**
     * Tests the get endpoint for activities list
     */
    @Test
    void testGetActivities() {
        arepo.save(createNormalActivity());
        arepo.save(createNormalActivity1());
        ResponseEntity<List<Activity>> responseEntity = activityController.getActivitiesList();
        assertEquals(responseEntity.getStatusCode(), HttpStatus.OK);
        assertEquals(responseEntity.getBody().get(0).getActivityName(), "Kaikoura Coast Track race");
        assertEquals(responseEntity.getBody().get(1).getActivityName(), "Triathlon");
        assertEquals(responseEntity.getBody().size(), 2);
    }

    /**
     * Tests that the getUsersActivities endpoint retrieves the activities associated with a specific profile
     */
    @Test
    void testGetUsersActivities() {
        Activity trackRace = createNormalActivity();
        ActivityType hiking = createActivityType();
        Profile maurice = createNormalProfileMaurice();
        Profile profile = prepo.save(maurice);
        arepo.save(createNormalActivity1());
        activityTypeRepo.save(hiking);

        activityController.createActivity(profile.getId(), trackRace, null, true);
        ResponseEntity<List<Activity>> responseEntity = activityController.getAllUsersActivities(null,
                profile.getId(), true);
        assertEquals(responseEntity.getStatusCode(), HttpStatus.OK);
        assertEquals(responseEntity.getBody().get(0).getActivityName(), "Kaikoura Coast Track race");
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
                new String[]{"tramping","hiking"}, false, "2020-02-20T08:00:00+1300", "2020-01-20T08:00:00+1300", "Kaikoura, NZ");
    }

    static ActivityType createActivityType() {
        return new ActivityType("Hiking");
    }

    static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }
}
