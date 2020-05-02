//package com.springvuegradle.Controller;
//
//import com.springvuegradle.Model.Activity;
//import com.springvuegradle.Repositories.ActivityTypeRepository;
//import com.springvuegradle.dto.ActivityTypesResponse;
//
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
//import org.springframework.http.ResponseEntity;
//import org.springframework.test.context.junit.jupiter.SpringExtension;
//
//import java.util.*;
//
//import static org.junit.jupiter.api.Assertions.*;
//
//@ExtendWith(SpringExtension.class)
//@DataJpaTest
//
//class ProfileControllerActivitiesTest {
//
//    @Autowired
//    private ActivityTypeRepository arepo;
//
//    @Autowired
//    private Profile_Controller profileController;
//
//    /**
//     * Needs to be run before each test to ensure the activity repository starts empty.
//     */
//    @BeforeEach
//    void dbSetup() {
//        arepo.deleteAll();
//        arepo.save(new Activity("Running", true, "testing", null, null, null));
//        arepo.save(new ActivityType("Swimming"));
//        arepo.save(new ActivityType("Cycling"));
//        arepo.save(new ActivityType("Walking"));
//        arepo.save(new ActivityType("Jogging"));
//        arepo.save(new ActivityType("Meditating"));
//    }
//
//    /**
//     * Check if a test array full of the same activities as saved in the database.
//     */
//    @Test
//    void checkActivitiesMatchTest() {
//        List<String> testActivities = Arrays.asList("Football", "Tennis", "Hockey", "Basketball", "Hiking", "Rock Climbing");
//
//        ResponseEntity<ActivityTypesResponse> response_entity = profileController.getActivityTypesList();
//        ActivityTypesResponse activities_response = response_entity.getBody();
//        //assertArrayEquals(testActivities, activities_response.getAllActivities());
//        assertLinesMatch(testActivities, activities_response.getAllActivityTypes());
//    }
//
//    /**
//     * Check if the right amount of activities are saved in the database.
//     */
//    @Test
//    void checkActivitiesCount() {
//        int expected_activities_in_repo = 6;
//        assertEquals(expected_activities_in_repo, arepo.count());
//    }
//
//}