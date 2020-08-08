package com.springvuegradle.controller;

import com.springvuegradle.config.MockServiceConfig;
import com.springvuegradle.dto.SimplifiedActivitiesResponse;
import com.springvuegradle.enums.ActivityPrivacy;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityTestUtils;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.utilities.JwtUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
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

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
        mockRepo.deleteAll();
        ;
    }

    @Test
    void removeMembershipFromActivitySuccessTest() {
        long mockActivityId = 10;
        long mockProfileId = 11;
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.removeMembership(mockProfileId, mockActivityId)).thenReturn(true);
        ResponseEntity<String> actualResponse = activityController.deleteActivityMembership(mockToken, mockProfileId, mockActivityId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void removeMembershipFromActivityFailTest() {
        long mockActivityId = 10;
        long mockProfileId = 11;
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.removeMembership(mockProfileId, mockActivityId)).thenReturn(false);
        ResponseEntity<String> actualResponse = activityController.deleteActivityMembership(mockToken, mockProfileId, mockActivityId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getActivitiesWithPrivacyLevelSuccessTest() {
        String mockToken = "token";
        Activity mockActivity = ActivityTestUtils.createNormalActivity();
        List<Activity> activityList = new ArrayList<>();
        activityList.add(mockActivity);
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getActivitiesWithPrivacyLevel(ActivityPrivacy.PUBLIC)).thenReturn(activityList);
        ResponseEntity<List<Activity>> actualResponse = activityController.getActivities("public", mockToken);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getActivityWithPrivacyLevelFailTest() {
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<List<Activity>> actualResponse = activityController.getActivities(mockToken, "fail");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
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

    @Test
    void getUsersActivitiesSuccessTest() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        String mockToken = "bob";
        long mockProfileId = 420;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<SimplifiedActivitiesResponse> actualResponse = activityController.getAllUsersActivities(mockToken, mockProfileId, 5, 0, "creator");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getUsersActivitiesInvalidCountTest() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        String mockToken = "bob";
        long mockProfileId = 420;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<SimplifiedActivitiesResponse> actualResponse = activityController.getAllUsersActivities(mockToken, mockProfileId, 0, 0, "creator");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    //Need test for invalid roles
}
