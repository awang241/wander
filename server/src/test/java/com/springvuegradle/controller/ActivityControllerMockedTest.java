package com.springvuegradle.controller;

import com.springvuegradle.dto.requests.ActivityRoleUpdateRequest;
import com.springvuegradle.dto.responses.ActivityMemberRoleResponse;
import com.springvuegradle.dto.responses.ProfileSummary;
import com.springvuegradle.enums.ActivityResponseMessage;
import com.springvuegradle.enums.AuthenticationErrorMessage;
import com.springvuegradle.enums.ProfileErrorMessage;
import com.springvuegradle.model.*;
import com.springvuegradle.config.MockServiceConfig;
import com.springvuegradle.dto.ActivityRequest;
import com.springvuegradle.dto.SimplifiedActivitiesResponse;
import com.springvuegradle.enums.ActivityPrivacy;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityTestUtils;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.service.SecurityService;
import com.springvuegradle.utilities.FormatHelper;
import com.springvuegradle.utilities.JwtUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.support.AnnotationConfigContextLoader;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

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
    @Autowired
    SecurityService mockSecurity;

    @AfterEach
    private void tearDown() {
        mockRepo.deleteAll();
        ;
    }


    @Test
    void removeMembershipFromActivitySuccessTest() {
        long mockActivityId = 10;
        long mockProfileId = 11;
        long editingUserId = 11;
        String mockToken = "token";
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(editingUserId);
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.doNothing().when(mockService).removeUserRoleFromActivity(editingUserId, mockProfileId, mockActivityId);
        ResponseEntity<String> actualResponse = activityController.deleteActivityMembership(mockToken, mockProfileId, mockActivityId);
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void removeMembershipFromActivityFailTest() {
        long mockActivityId = 10;
        long mockProfileId = 11;
        long editingUserId = 11;
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(editingUserId);
        Mockito.doThrow(NoSuchElementException.class).when(mockService).removeUserRoleFromActivity(mockProfileId, mockProfileId, mockActivityId);
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

        mockRepo.save(mockActivity);
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

        assertEquals(expectedResponse, actualResponse);
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
    void getActivityMembersNormalTest() {
        String roleName = "participant";
        int count = 5;
        int startIndex = 0;

        long mockId = 10;
        Pageable mockPageRequest = PageRequest.of(0, 5);
        String mockToken = "token";
        Page<Profile> mockPage = new PageImpl<>(ProfileTestUtils.createProfilesWithSameSurnameAsJimmy(), mockPageRequest, 12);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getActivityMembersByRole(mockId, ActivityMembership.Role.PARTICIPANT, mockPageRequest))
                .thenReturn(mockPage);
        List<ProfileSummary> summaries = FormatHelper.createProfileSummaries(mockPage.getContent());
        ActivityMemberRoleResponse expectedBody = new ActivityMemberRoleResponse(summaries);
        ResponseEntity<ActivityMemberRoleResponse> expectedResponse = new ResponseEntity<>(expectedBody, HttpStatus.OK);
        assertEquals(expectedResponse, activityController.getActivityMembers(mockToken, mockId, roleName, count, startIndex));
    }

    @Test
    void getActivityMembersWithInvalidTokenTest() {
        String roleName = "participant";
        int count = 5;
        int startIndex = 0;
        long mockId = 10;
        String mockToken = "token";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(false);
        ActivityMemberRoleResponse expectedBody = new ActivityMemberRoleResponse(AuthenticationErrorMessage.INVALID_CREDENTIALS);
        ResponseEntity<ActivityMemberRoleResponse> expectedResponse = new ResponseEntity<>(expectedBody, HttpStatus.UNAUTHORIZED);
        assertEquals(expectedResponse, activityController.getActivityMembers(mockToken, mockId, roleName, count, startIndex));
    }

    @Test
    void getActivityMembersWithSomePagingParametersMissingTest() {
        String roleName = "participant";
        int count = 5;

        long mockId = 10;
        String mockToken = "token";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ActivityMemberRoleResponse expectedBody = new ActivityMemberRoleResponse(ProfileErrorMessage.INVALID_SEARCH_COUNT);
        ResponseEntity<ActivityMemberRoleResponse> expectedResponse = new ResponseEntity<>(expectedBody, HttpStatus.BAD_REQUEST);
        assertEquals(expectedResponse, activityController.getActivityMembers(mockToken, mockId, roleName, count, null));
    }

    @Test
    void getActivityMembersWithServiceThrowingNotFoundErrorTest() {
        String roleName = "participant";
        int count = 5;
        int startIndex = 0;

        long mockId = 10;
        Pageable mockPageRequest = PageRequest.of(0, 5);
        String mockToken = "token";
        Exception serviceException = new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getActivityMembersByRole(mockId, ActivityMembership.Role.PARTICIPANT, mockPageRequest))
                .thenThrow(serviceException);
        ActivityMemberRoleResponse expectedBody = new ActivityMemberRoleResponse(ActivityResponseMessage.INVALID_ACTIVITY);
        ResponseEntity<ActivityMemberRoleResponse> expectedResponse = new ResponseEntity<>(expectedBody, HttpStatus.NOT_FOUND);
        assertEquals(expectedResponse, activityController.getActivityMembers(mockToken, mockId, roleName, count, startIndex));
    }

    @Test
    void getActivityMembersWithInvalidRoleNameTest() {
        String roleName = "iohfad";
        int count = 5;
        int startIndex = 0;

        long mockId = 10;
        String mockToken = "token";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<ActivityMemberRoleResponse> actualResponse = activityController.getActivityMembers(mockToken, mockId, roleName, count, startIndex);
        assertEquals(HttpStatus.BAD_REQUEST, actualResponse.getStatusCode());
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

    @Test
    void clearRoleOfActivitySuccessTest(){
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        String mockToken = "54321";
        long mockActivityID = 666;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(54321l);
        Mockito.when(mockService.isProfileActivityCreator(54321l, mockActivityID)).thenReturn(true);
        ResponseEntity actualResponse = activityController.clearRoleOfActivity(mockToken, mockActivityID, "participant");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void clearRoleOfActivityNotOwnerTest(){
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.FORBIDDEN);
        String mockToken = "54321";
        long mockActivityID = 666;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(54321l);
        Mockito.when(mockService.isProfileActivityCreator(54321l, mockActivityID)).thenReturn(false);
        ResponseEntity actualResponse = activityController.clearRoleOfActivity(mockToken, mockActivityID, "participant");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void clearRoleOfActivityNoTokenTest(){
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        String mockToken = null;
        long mockActivityID = 666;
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(54321l);
        Mockito.when(mockService.isProfileActivityCreator(54321l, mockActivityID)).thenReturn(true);
        ResponseEntity actualResponse = activityController.clearRoleOfActivity(mockToken, mockActivityID, "participant");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void clearRoleOfActivityBadTokenTest(){
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        String mockToken = "badtoken";
        long mockActivityID = 666;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(false);
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(54321l);
        Mockito.when(mockService.isProfileActivityCreator(54321l, mockActivityID)).thenReturn(true);
        ResponseEntity actualResponse = activityController.clearRoleOfActivity(mockToken, mockActivityID, "participant");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void clearRoleOfActivityInvalidRoleTest(){
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        String mockToken = "54321";
        long mockActivityID = 666;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(54321l);
        Mockito.when(mockService.isProfileActivityCreator(54321l, mockActivityID)).thenReturn(true);
        ResponseEntity actualResponse = activityController.clearRoleOfActivity(mockToken, mockActivityID, "partycrasher");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void clearRoleOfActivityNothingtoDeleteTest(){
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);
        String mockToken = "54321";
        long mockActivityID = 666;
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockJwt.extractId(mockToken)).thenReturn(54321l);
        Mockito.when(mockService.isProfileActivityCreator(54321l, mockActivityID)).thenReturn(true);
        Mockito.doThrow(new NoSuchElementException()).when(mockService).clearActivityRoleList(mockActivityID, "PARTICIPANT");
        ResponseEntity actualResponse = activityController.clearRoleOfActivity(mockToken, mockActivityID, "participant");
        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }


    //TODO Need test for invalid roles
}
