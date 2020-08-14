package com.springvuegradle.controller;

import com.springvuegradle.dto.*;
import com.springvuegradle.enums.ActivityPrivacy;
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
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.support.AnnotationConfigContextLoader;

import java.util.ArrayList;
import java.util.List;

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

    @AfterEach
    private void tearDown() {
        Mockito.reset(mockService, mockJwt);
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
        long mockProfileId = 25;
        Activity mockActivity = ActivityTestUtils.createNormalActivity();
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);

        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(mockProfileId);
        Mockito.when(mockService.getActivityByActivityId(mockProfileId, mockActivityId)).thenReturn(mockActivity);
        ResponseEntity<Activity> actualResponse = activityController.getActivity(mockToken, mockActivityId);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getActivityByInvalidIdTest() {
        long mockActivityId = 10;
        String mockToken = "token";
        long mockProfileId = 25;
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);

        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getActivityByActivityId(mockProfileId, mockActivityId)).thenReturn(null);
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

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getActivityByIdNoTokenTest() {
        long mockActivityId = 10;
        long mockProfileId = 25;
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
        ResponseEntity<SimplifiedActivitiesResponse> actualResponse = activityController.getUsersActivitiesByRole(mockToken, mockProfileId, 5, 0, "creator");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getUsersActivitiesInvalidCountTest() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        String mockToken = "bob";
        long mockProfileId = 420;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<SimplifiedActivitiesResponse> actualResponse = activityController.getUsersActivitiesByRole(mockToken, mockProfileId, 0, 0, "creator");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    //Need test for invalid roles

    @Test
    void postActivityParticipationSuccessTest() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.CREATED);
        String mockToken = "bob";
        ActivityParticipationRequest mockParticipationRequest = ActivityTestUtils.createNormalParticipationRequest();
        long mockProfileId = 420;
        long mockActivityId = 505;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<String> actualResponse = activityController.addActivityParticipation(mockParticipationRequest, mockToken, mockProfileId, mockActivityId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void postActivityParticipationWhenServiceThrowsExceptionReturnsA403ErrorTest() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.FORBIDDEN);
        String mockToken = "bob";
        ActivityParticipationRequest mockParticipationRequest = ActivityTestUtils.createNormalParticipationRequest();
        ActivityParticipation mockParticipation = ActivityTestUtils.createNormalParticipation();
        long mockProfileId = 420;
        long mockActivityId = 505;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.doThrow(new IllegalArgumentException()).when(mockService).createParticipation(mockActivityId, mockProfileId, mockParticipation);
        ResponseEntity<String> actualResponse = activityController.addActivityParticipation(mockParticipationRequest, mockToken, mockProfileId, mockActivityId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void editActivityParticipationSuccessTest() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        String mockToken = ":)";
        ActivityParticipationRequest updatedParticipation = ActivityTestUtils.createADifferentParticipationRequest();
        long mockParticipationId = 555;
        long mockProfileId = 420;
        long mockActivityId = 505;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<String> actualResponse = activityController.updateParticipation(updatedParticipation, mockToken, mockProfileId, mockActivityId, mockParticipationId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void editActivityParticipationThrow403Test() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.FORBIDDEN);
        String mockToken = ":)";
        ActivityParticipationRequest mockUpdatedParticipationRequest = ActivityTestUtils.createADifferentParticipationRequest();
        ActivityParticipation mockUpdatedParticipation = ActivityTestUtils.createADifferentParticipation();
        long mockParticipationId = 555;
        long mockProfileId = 420;
        long mockActivityId = 505;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.doThrow(new IllegalArgumentException()).when(mockService).editParticipation(mockActivityId, mockProfileId, mockParticipationId, mockUpdatedParticipation);
        ResponseEntity<String> actualResponse = activityController.updateParticipation(mockUpdatedParticipationRequest, mockToken, mockProfileId, mockActivityId, mockParticipationId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void deleteActivityParticipationSuccessTest() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        String mockToken = ":)";
        long mockParticipationId = 555;
        long mockProfileId = 420;
        long mockActivityId = 505;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.removeParticipation(mockActivityId, mockProfileId, mockParticipationId)).thenReturn(true);
        ResponseEntity<String> actualResponse = activityController.deleteParticipation(mockToken, mockProfileId, mockActivityId, mockParticipationId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void deleteActivityParticipationFailThrow404Test() {
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);
        String mockToken = ":)";
        long mockParticipationId = 555;
        long mockProfileId = 420;
        long mockActivityId = 505;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.removeParticipation(mockActivityId, mockProfileId, mockParticipationId)).thenReturn(false);
        ResponseEntity<String> actualResponse = activityController.deleteParticipation(mockToken, mockProfileId, mockActivityId, mockParticipationId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }
}
