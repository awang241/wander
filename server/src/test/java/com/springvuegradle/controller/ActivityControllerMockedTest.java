package com.springvuegradle.controller;

import com.springvuegradle.dto.EditAuthLevelRequest;
import com.springvuegradle.enums.AuthLevel;
import com.springvuegradle.enums.ActivityResponseMessage;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.config.MockServiceConfig;
import com.springvuegradle.dto.ProfileSearchResponse;
import com.springvuegradle.dto.ProfileSummary;
import com.springvuegradle.enums.AuthenticationErrorMessage;
import com.springvuegradle.service.ProfileService;
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
    @Autowired
    ActivityRepository mockRepo;

    Profile jimmy;
    Activity kaikouraCoastTrackRace;
    @BeforeEach
    private void setUp(){
        kaikouraCoastTrackRace = ActivityTestUtils.createNormalActivity();
    }

    @AfterEach
    private void tearDown() {
        mockRepo.deleteAll();;
    }

    @Test
    void getActivityByValidActivityIdTest() {
        long mockActivityId = 10;
        String mockToken = "token";
        Activity mockActivity = ActivityTestUtils.createNormalActivity();

        mockRepo.save(mockActivity);
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getActivityByActivityId(mockActivityId)).thenReturn(mockActivity);
        ResponseEntity<Activity> actualResponse = activityController.getActivity(mockToken, mockActivityId);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
    }

    @Test
    void getInvalidActivityIdTest() {
        long mockActivityId = 10;
        String mockToken = "token";
        Activity mockActivity = ActivityTestUtils.createNormalActivity();
//        Activity nullActivity = ActivityTestUtils.createNullActivity();
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);

        mockRepo.save(mockActivity);

        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
//        Mockito.when(mockService.getActivityByActivityId(mockActivityId)).thenReturn(nullActivity);
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



//    @Test
//    void getActivityByIdNoTokenTest() {
//        long mockId = 10;
//        EditAuthLevelRequest request = new EditAuthLevelRequest("admin");
//        ResponseEntity<String> expectedResponse = new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
//                HttpStatus.UNAUTHORIZED);
//        ResponseEntity<String> actualResponse = profileController.editAuthLevel(request, mockId, null);
//        assertEquals(expectedResponse, actualResponse);
//    }



//    @Test
//    void editAuthLevelToUserTest() {
//        long mockId = 10;
//        EditAuthLevelRequest request = new EditAuthLevelRequest("user");
//        String mockToken = "token";
//        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
//        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
//        ResponseEntity<String> actualResponse = profileController.editAuthLevel(request, mockId, mockToken);
//        assertEquals(expectedResponse, actualResponse);
//    }
}
