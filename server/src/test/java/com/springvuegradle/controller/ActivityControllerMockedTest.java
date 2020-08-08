package com.springvuegradle.controller;

import com.springvuegradle.dto.responses.ActivityMemberRoleResponse;
import com.springvuegradle.dto.responses.ProfileSummary;
import com.springvuegradle.enums.ActivityResponseMessage;
import com.springvuegradle.enums.AuthenticationErrorMessage;
import com.springvuegradle.enums.ProfileErrorMessage;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.utilities.FormatHelper;
import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.config.MockServiceConfig;
import org.junit.jupiter.api.*;
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
}
