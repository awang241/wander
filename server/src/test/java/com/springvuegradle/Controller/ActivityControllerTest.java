package com.springvuegradle.Controller;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.dto.ActivityTypesResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
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
    private ActivityController activityController;

    @Autowired
    static private ActivityTypeRepository activityTypeRepo;

    /**
     * This tests to ensure activities structured correctly can be added to the database.
     */
    @Disabled
    @Test
    void createActivityTest() {
        Activity trackRace = createNormalActivity();
        ActivityType hiking = createActivityType();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, arepo.count());

        ResponseEntity<String> response_entity = activityController.createActivity(null, trackRace, (long) 0, true);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, arepo.count());
    }

    /**
     * This tests to ensure activities structured correctly can be added to the database.
     */
    @Disabled
    @Test
    void createIncorrectActivityTest() {
        Activity trackRace = createIncorrectActivity();
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, arepo.count());

        ResponseEntity<String> response_entity = activityController.createActivity(null, trackRace, (long) 0);
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());

        expected_in_repo = 0;
        assertEquals(expected_in_repo, arepo.count());
    }

    /**
     * Check if a test array full of the same activities as saved in the database.
     */
    @Test
    void checkActivitiesMatchTest() {
        List<String> testActivities = Arrays.asList("Kaikoura Coast Track race", "Triathlon");
        int i = 0;
        arepo.deleteAll();
        arepo.save(createNormalActivity());
        arepo.save(createNormalActivity1());
        ResponseEntity<List<Activity>> response_entity = activityController.getActivitiesList();
        for (Activity activity: response_entity.getBody()) {
            assertEquals(testActivities.get(i++), activity.getActivityName());
        }
        arepo.deleteAll();
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
                new String[]{"running","swimming"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    static Activity createIncorrectActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"tramping","hiking"}, false, "2020-02-20T08:00:00+1300", "2020-01-20T08:00:00+1300", "Kaikoura, NZ");
    }

    static ActivityType createActivityType() {
        return new ActivityType("Hiking");
    }
}
