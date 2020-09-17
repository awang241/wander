package com.springvuegradle.repositories;

import com.springvuegradle.model.Activity;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivityRepositoryTest {

    static ActivityRepository repo = null;

    @Autowired
    ActivityRepositoryTest(ActivityRepository repo) {
        this.repo = repo;
    }

    @BeforeEach
    void setUp() {
        repo.deleteAll();
    }

    @Test
    void findAllInRangeReturnsSingleActivity() {
        Activity a1 = createNormalActivityTrackRace();
        repo.save(a1);
        List<Activity> activities = repo.findAllInRange(-2000.0, 2000.0, -1000.0, 1000.0);
        assertEquals(1, activities.size());
    }

    @Test
    void findAllInRangeReturnsTwoActivities() {
        Activity a1 = createNormalActivityTrackRace();
        Activity a2 = createNormalActivityTriathlon();
        repo.save(a1);
        repo.save(a2);
        List<Activity> activities = repo.findAllInRange(-200.0, 200.0, -100.0, 100.0);
        assertEquals(1, activities.size());
    }

    @Test
    void findAllInRangeWhenGivenExactCoordinatesReturnsSingleActivity() {
        Activity a1 = createNormalActivityTriathlon();
        repo.save(a1);
        List<Activity> activities = repo.findAllInRange(1000.0, 1000.0, 500.0, 500.0);
        assertEquals(1, activities.size());
    }

    @Test
    void findAllInRangeReturnsNoActivities() {
        Activity a1 = createNormalActivityTriathlon();
        repo.save(a1);
        List<Activity> activities = repo.findAllInRange(-200.0, 200.0, -100.0, 100.0);
        assertEquals(0, activities.size());
    }

    /**
     * All of the sample Activities.
     */

    static Activity createNormalActivityTrackRace() {
        Activity activity = new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
        activity.setLatitude(0.0);
        activity.setLongitude(0.0);
        return activity;
    }

    static Activity createNormalActivityTriathlon() {
        Activity activity =  new Activity("Triathlon", "I hate triathlons",
                new String[]{"Hiking","Football"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
        activity.setLatitude(500.0);
        activity.setLongitude(1000.0);
        return activity;
    }
}