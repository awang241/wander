package com.springvuegradle.controller;

import com.springvuegradle.model.Profile;
import com.springvuegradle.model.ProfileSearchCriteria;
import com.springvuegradle.model.ProfileTestUtils;
import com.springvuegradle.repositories.*;
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
class ProfileControllerMockedTest {

    @Autowired
    ProfileService mockService;
    @Autowired
    ProfileRepository profileRepository;
    @Autowired
    JwtUtil mockJwt;
    @Autowired
    PassportCountryRepository passportCountryRepository;
    @Autowired
    EmailRepository emailRepository;
    @Autowired
    ActivityRepository activityRepository;
    @Autowired
    ActivityTypeRepository activityTypeRepository;
    @Autowired
    Profile_Controller profileController;

    Profile jimmy, jimmyAlternate, maurice;

    @BeforeEach
    private void setUp(){
        jimmy = ProfileTestUtils.createProfileJimmy();
        jimmyAlternate = ProfileTestUtils.createProfileJimmyAlternate();
        maurice = ProfileTestUtils.createNormalProfileMaurice();
    }

    @AfterEach
    private void tearDown() {
        jimmy = null;
        maurice = null;
    }

    @Test
    void getUserProfilesWithNormalParamsReturnsNormalResponseTest(){
        String mockToken = "mockToken";
        List<Profile> expectedProfiles = new ArrayList<>();
        expectedProfiles.add(jimmy);
        expectedProfiles.add(jimmyAlternate);

        List<ProfileSummary> profileSummaries = new ArrayList<>();
        for (Profile profile: expectedProfiles) {
            profileSummaries.add(new ProfileSummary(profile));
        }
        ProfileSearchResponse responseBody = new ProfileSearchResponse(profileSummaries);
        ResponseEntity<ProfileSearchResponse> expectedResponse = new ResponseEntity<>(responseBody, HttpStatus.OK);

        int count = 2;
        int startIndex = 1;

        Page<Profile> mockPage = new PageImpl<>(expectedProfiles);
        Pageable mockRequest = PageRequest.of(startIndex / count, count);
        ProfileSearchCriteria criteria = new ProfileSearchCriteria("Jimmy", "Jones", "Quick", null, null);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(mockService.getUsers(criteria, mockRequest)).thenReturn(mockPage);

        ResponseEntity<ProfileSearchResponse> actualResponse = profileController.getUserProfiles(null,
                "Jimmy Jones Quick", null, null, null, count, startIndex, mockToken);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void getUserProfilesWithNoTokenReturnsUnauthorizedResponseTest(){
        ResponseEntity<ProfileSearchResponse> response = profileController.getUserProfiles(null,
                null, null, null, null, 2, 0, null);
        assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
        assertEquals(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(), response.getBody().getMessage());
    }

    @Test
    void getUserProfilesWithInvalidTokenReturnsForbiddenResponseTest(){
        String mockToken = "badToken";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(false);
        ResponseEntity<ProfileSearchResponse> response = profileController.getUserProfiles(null,
                null, null, null, null, 2, 0, mockToken);
        assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
    }

    @Test
    void getUserProfilesWithInvalidCountParameterReturnsBadRequestResponseTest(){
        String mockToken = "mockToken";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<ProfileSearchResponse> response = profileController.getUserProfiles(null,
                null, null, null, null, 0, 0, mockToken);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
}