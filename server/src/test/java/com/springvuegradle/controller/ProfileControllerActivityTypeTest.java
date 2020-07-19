package com.springvuegradle.controller;

import com.springvuegradle.model.ActivityType;
import com.springvuegradle.repositories.ActivityTypeRepository;
import com.springvuegradle.dto.ActivityTypesResponse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest

class ProfileControllerActivityTypeTest {

    @Autowired
    private ActivityTypeRepository arepo;

    @Autowired
    private Profile_Controller profileController;

    /**
     * Needs to be run before each test to ensure the activity repository starts empty.
     */
    @BeforeEach
    void dbSetup() {
        arepo.deleteAll();
        arepo.save(new ActivityType("Football"));
        arepo.save(new ActivityType("Tennis"));
        arepo.save(new ActivityType("Hockey"));
        arepo.save(new ActivityType("Basketball"));
        arepo.save(new ActivityType("Hiking"));
        arepo.save(new ActivityType("Rock Climbing"));
    }

    /**
     * Check if a test array full of the same activities as saved in the database.
     */
    @Test
    void checkActivitiesMatchTest() {
        List<String> testActivities = Arrays.asList("Football", "Tennis", "Hockey", "Basketball", "Hiking", "Rock Climbing");
        ResponseEntity<ActivityTypesResponse> response_entity = profileController.getActivityTypesList();
        ActivityTypesResponse activities_response = response_entity.getBody();
        assertLinesMatch(testActivities, activities_response.getAllActivityTypes());
    }

    /**
     * Check if the right amount of activities are saved in the database.
     */
    @Test
    void checkActivitiesCount() {
        int expected_activities_in_repo = 6;
        assertEquals(expected_activities_in_repo, arepo.count());
    }

}