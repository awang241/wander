package com.springvuegradle.controller;

import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.config.MockServiceConfig;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.support.AnnotationConfigContextLoader;

import static org.junit.jupiter.api.Assertions.*;

@ActiveProfiles("mock-service")
@ExtendWith(SpringExtension.class)
@SpringBootTest
@ContextConfiguration(loader = AnnotationConfigContextLoader.class, classes = {MockServiceConfig.class})
class ActivityControllerMockedTest {

    @Autowired
    ActivityService mockService;
    @Autowired
    ActivityRepository activityRepository;
    @Autowired
    JwtUtil mockJwt;
    @Autowired
    ActivityController activityController;
    @Autowired
    ActivityRepository mockRepo;

    @AfterEach
    private void tearDown() {
        mockRepo.deleteAll();;
    }

    @Test
    void getActivityByValidActivityIdTest() {
        long mockActivityId = 10;
        String mockToken = "token";
        Activity mockActivity = ActivityTestUtils.createNormalActivity();
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);

        mockRepo.save(mockActivity);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getActivityByActivityId(mockActivityId)).thenReturn(mockActivity);
        ResponseEntity<Activity> actualResponse = activityController.getActivity(mockToken, mockActivityId);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getActivityByInvalidIdTest() {
        long mockActivityId = 10;
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);

        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getActivityByActivityId(mockActivityId)).thenReturn(null);
        ResponseEntity<Activity> actualResponse = activityController.getActivity(mockToken, mockActivityId);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getActivityByIdInvalidTokenTest(){
        long mockActivityId = 10;
        String mockToken = "invalid token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.FORBIDDEN);

        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(false);
        ResponseEntity<Activity> actualResponse = activityController.getActivity(mockToken, mockActivityId);

        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void getActivityByIdNoTokenTest() {
        long mockActivityId = 10;
        ResponseEntity<String> expectedResponse = new ResponseEntity<>((HttpStatus.UNAUTHORIZED));
        ResponseEntity<Activity> actualResponse = activityController.getActivity(null, mockActivityId);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }
}
