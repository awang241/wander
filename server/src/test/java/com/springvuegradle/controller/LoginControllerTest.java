package com.springvuegradle.controller;


import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.dto.LoginRequest;
import com.springvuegradle.dto.LoginResponse;
import com.springvuegradle.model.Profile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;


import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Calendar;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class LoginControllerTest {

    @Autowired
    private JwtUtil jwtUtil;

    @Autowired
    private LoginController loginController;

    @Autowired
    private Profile_Controller profileController;


    /**
     * This test ensures profiles login in properly with the correct credentials
     * using the loginUser method
     */
    @BeforeEach
    void beforeEach() {
        Profile maurice = createNormalProfileMaurice();
        profileController.createProfile(maurice);
    }

    /**
     * This login failure with invalid password
     */
    @Test
    void loginFailureTest() {
        Profile jimmy = ProfileControllerTest.createNormalProfileJimmy();
        LoginRequest request = new LoginRequest("jimmy@yahoo.com", "invalid-password");
        ResponseEntity<LoginResponse> response_entity = loginController.loginUser(request);
        assertEquals(HttpStatus.UNAUTHORIZED, response_entity.getStatusCode());
    }

    /**
     * Test to check user can successfully login when providing the correct email and correct password.
     */
    @Test
    void loginTest() {
        LoginRequest jackyRequest = new LoginRequest("jacky@google.com", "jacky'sSecuredPwd");
        ResponseEntity<LoginResponse> response = loginController.loginUser(jackyRequest);
        assertTrue(jwtUtil.validateToken(response.getBody().getToken()));
    }

    /**
     * Test to check login response when providing the correct email and correct password.
     */
    @Test
    void loginResponseTest() {
        LoginRequest jackyRequest = new LoginRequest("jacky@google.com", "jacky'sSecuredPwd");
        ResponseEntity<LoginResponse> response = loginController.loginUser(jackyRequest);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{"New Zealand", "India"}, new String[]{});
    }


}
