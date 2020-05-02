package com.springvuegradle.Controller;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Repositories.*;
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

    /**
     * This tests to ensure activities structured correctly can be added to the database.
     */
    @Test
    void createActivityTest() {
        Activity trackRace = createNormalActivity();
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, arepo.count());

        ResponseEntity<String> response_entity = activityController.createActivity(trackRace, (long) 0);
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
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, arepo.count());

        ResponseEntity<String> response_entity = activityController.createActivity(trackRace, (long) 0);
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());

        expected_in_repo = 0;
        assertEquals(expected_in_repo, arepo.count());
    }



    /* Below are a set of ready-made Activity objects which can be used for various tests. */

    /**
     * @return a valid activity object.
     */
    static Activity createNormalActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"tramping","hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    static Activity createIncorrectActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"tramping","hiking"}, false, "2020-02-20T08:00:00+1300", "2020-01-20T08:00:00+1300", "Kaikoura, NZ");
    }




}
