package com.springvuegradle.Controller;

import com.springvuegradle.Controller.enums.ActivityResponseMessage;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.hibernate.type.CalendarTimeType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivityControllerTest {

    @Autowired
    ActivityController controller;
    @Autowired
    ProfileRepository profileRepository;
    @Autowired
    ActivityRepository activityRepository;
    @Autowired
    static ActivityTypeRepository typeRepository;

    @BeforeAll
    static void setUpBeforeClass() {
        populateActivityTypes();
    }

    @BeforeEach
    void setUp() {

    }

    @AfterEach
    void tearDown() {
        profileRepository.deleteAll();
        activityRepository.deleteAll();
    }

    @Test
    void updateActivityWithNormalRequestGeneratesOKResponseTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ResponseEntity<String> expectedResponse = new ResponseEntity<String>(ActivityResponseMessage.EDIT_SUCCESS.toString(),
                                                                             HttpStatus.OK);

        ResponseEntity<String> actualResponse = controller.updateActivity(createNormalActivityKaikoura(), activityId);

        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void updateActivityWithNormalRequestSavesDataTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivityKaikoura(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        controller.updateActivity(expectedActivity, activityId);
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithNoNameGeneratesErrorResponseTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ResponseEntity<String> expectedResponse = new ResponseEntity<String>(ActivityResponseMessage.MISSING_NAME.toString(),
                                                                             HttpStatus.BAD_REQUEST);

        ResponseEntity<String> actualResponse = controller.updateActivity(createBadActivityNoName(), activityId);

        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void updateActivityWithNoNameDoesNotSaveDataTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        controller.updateActivity(createBadActivityNoName(), activityId);
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    @Test
    void updateActivityWithBlankNameGeneratesErrorResponseTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        ResponseEntity<String> expectedResponse = new ResponseEntity<String>(ActivityResponseMessage.MISSING_NAME.toString(),
                HttpStatus.BAD_REQUEST);

        ResponseEntity<String> actualResponse = controller.updateActivity(createBadActivityBlankName(), activityId);

        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void updateActivityWithBlankNameDoesNotSaveDataTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        assertTrue(activityRepository.existsById(activityId), "Sanity check: test setup correctly");

        controller.updateActivity(createBadActivityBlankName(), activityId);
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    private Activity createNormalActivityKaikoura() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"tramping", "hiking"}, false, start, end, "Kaikoura, NZ");
    }

    private Activity createNormalActivitySilly() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("Wibble", "A bald man",
                new String[]{"hockey"}, true, start, end, "K2");
    }

    private Activity createBadActivityNoName() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity(null, "A big and nice race on a lovely peninsula",
                new String[]{"tramping", "hiking"}, false, start, end, "Kaikoura, NZ");
    }

    private Activity createBadActivityBlankName() {
        CalendarTimeType start = new CalendarTimeType();
        CalendarTimeType end = new CalendarTimeType();
        return new Activity("", "A big and nice race on a lovely peninsula",
                new String[]{"tramping", "hiking"}, false, start, end, "Kaikoura, NZ");
    }

    private static void populateActivityTypes() {
        typeRepository.save(new ActivityType("football"));
        typeRepository.save(new ActivityType("tennis"));
        typeRepository.save(new ActivityType("hockey"));
        typeRepository.save(new ActivityType("basketball"));
        typeRepository.save(new ActivityType("hiking"));
        typeRepository.save(new ActivityType("tramping"));
        typeRepository.save(new ActivityType("rock climbing"));
    }
}