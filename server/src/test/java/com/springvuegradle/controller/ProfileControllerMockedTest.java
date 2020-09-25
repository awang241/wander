package com.springvuegradle.controller;

import com.springvuegradle.dto.requests.EditAuthLevelRequest;
import com.springvuegradle.dto.responses.NotificationsResponse;
import com.springvuegradle.enums.ProfileErrorMessage;
import com.springvuegradle.model.Profile;
import com.springvuegradle.model.ProfileSearchCriteria;
import com.springvuegradle.utilities.ProfileTestUtils;
import com.springvuegradle.repositories.*;
import com.springvuegradle.service.NotificationService;
import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.config.MockServiceConfig;
import com.springvuegradle.dto.responses.ProfileSearchResponse;
import com.springvuegradle.dto.responses.ProfileSummary;
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
    JwtUtil mockJwt;
    @Autowired
    Profile_Controller profileController;
    @Autowired
    ProfileRepository mockProfileRepo;
    @Autowired
    NotificationService notificationService;

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
        Mockito.reset(mockService, mockJwt);
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

    @Test
    void editAuthLevelToAdminTest() {
        long mockId = 10;
        EditAuthLevelRequest request = new EditAuthLevelRequest("admin");
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<String> actualResponse = profileController.editAuthLevel(request, mockId, mockToken);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void editAuthLevelToUserTest() {
        long mockId = 10;
        EditAuthLevelRequest request = new EditAuthLevelRequest("user");
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(HttpStatus.OK);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<String> actualResponse = profileController.editAuthLevel(request, mockId, mockToken);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void editAuthLevelInvalidRoleTest() {
        long mockId = 10;
        EditAuthLevelRequest request = new EditAuthLevelRequest("not a valid role");
        String mockToken = "token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(ProfileErrorMessage.INVALID_ROLE.getMessage(), HttpStatus.FORBIDDEN);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);

        ResponseEntity<String> actualResponse = profileController.editAuthLevel(request, mockId, mockToken);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void editAuthLevelNoTokenTest() {
        long mockId = 10;
        EditAuthLevelRequest request = new EditAuthLevelRequest("admin");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                HttpStatus.UNAUTHORIZED);
        ResponseEntity<String> actualResponse = profileController.editAuthLevel(request, mockId, null);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void editAuthLevelInvalidTokenTest() {
        long mockId = 10;
        EditAuthLevelRequest request = new EditAuthLevelRequest("admin");
        String mockToken = "not a valid token";
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(),
                HttpStatus.FORBIDDEN);
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(false);
        ResponseEntity<String> actualResponse = profileController.editAuthLevel(request, mockId, mockToken);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void checkValidEmailExistsTest() {
        String mockEmail = "validEmail@gmail.com";
        Mockito.when(mockService.checkEmailExistsInDB(mockEmail)).thenReturn(true);
        boolean actualResponse = profileController.verifyEmailExists(mockEmail);
        assertEquals(true, actualResponse);
    }

    @Test
    void checkInvalidEmailExistsTest() {
        String mockEmail = "invalidEmail@gmail.com";
        Mockito.when(mockService.checkEmailExistsInDB(mockEmail)).thenReturn(false);
        boolean actualResponse = profileController.verifyEmailExists(mockEmail);
        assertEquals(false, actualResponse);
    }

    @Test
    void getNotificationsWithPaginationSuccessTest() {
        long mockId = 10;
        int mockCount = 10;
        int mockStartIndex = 0;
        String mockToken = "babababa";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        Mockito.when(notificationService.getSortedNotifications(mockId, mockCount, mockStartIndex)).thenReturn(null);
        ResponseEntity<NotificationsResponse> actualResponse = profileController.getNotifications(mockToken, mockId, mockCount, mockStartIndex);
        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    void getNotificationsInvalidCountTest() {
        long mockId = 10;
        int mockCount = 0;
        int mockStartIndex = 0;
        String mockToken = "babababa";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<NotificationsResponse> actualResponse = profileController.getNotifications(mockToken, mockId, mockCount, mockStartIndex);
        assertEquals(HttpStatus.BAD_REQUEST, actualResponse.getStatusCode());
    }

    @Test
    void getNotificationsInvalidTokenTest() {
        long mockId = 10;
        int mockCount = 10;
        int mockStartIndex = 0;
        String mockToken = "invalud";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(false);
        ResponseEntity<NotificationsResponse> actualResponse = profileController.getNotifications(mockToken, mockId, mockCount, mockStartIndex);
        assertEquals(HttpStatus.FORBIDDEN, actualResponse.getStatusCode());
    }

    @Test
    void getNotificationsBlankTokenTest() {
        long mockId = 10;
        int mockCount = 10;
        int mockStartIndex = 0;
        String mockToken = "";
        Mockito.when(mockJwt.validateToken(mockToken)).thenReturn(true);
        ResponseEntity<NotificationsResponse> actualResponse = profileController.getNotifications(mockToken, mockId, mockCount, mockStartIndex);
        assertEquals(HttpStatus.UNAUTHORIZED, actualResponse.getStatusCode());
    }
}
