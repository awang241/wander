package com.springvuegradle.Controller;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.config.ActivityServiceTestConfiguration;
import com.springvuegradle.service.ActivityService;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the ActivityController class, these tests are run separately from the actual repository.
 */
@ActiveProfiles("test")
@ExtendWith(SpringExtension.class)
@Import(ActivityServiceTestConfiguration.class)
public class ActivityControllerTest {

    @Autowired
    private ActivityController activityController;

    @Test
    void updateActivityNormalRequest() {
        fail("Not yet implemented");
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

    static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }
}
