package com.springvuegradle.steps;

import com.springvuegradle.Controller.LoginController;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.dto.LoginRequest;
import com.springvuegradle.dto.LoginResponse;
import io.cucumber.java.Before;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.sql.SQLException;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class LoginControllerTestSteps {

    private LoginController loginController;
    private EmailRepository emailRepository;
    private Exception registerException;
    private Collection<Profile> returnedUsers;
    private ResponseEntity<LoginResponse> loggedInUser;
    private String returnedPassword;
    private String userEmail;
    private long loggedInUserId;
    private JwtUtil jwtUtil;

    @Before
    public void resetLocalFields() throws SQLException {
        loginController = new LoginController(jwtUtil, emailRepository);

        registerException = null;
        loggedInUser = null;
        loggedInUserId = 0;
        returnedUsers = null;
        returnedPassword = null;
        userEmail = null;
        jwtUtil = null;
    }


    @When("I log in using email {string} and password {string}")
    public void i_log_in_using_email_and_password(String email, String password) throws SQLException {
        LoginRequest jackyRequest = new LoginRequest(email, password);
        loggedInUser = loginController.loginUser(jackyRequest);
    }


    @Then("I am successfully logged in")
    public void i_am_successfully_logged_in(String email) {
        assertTrue(jwtUtil.validateToken(loggedInUser.getBody().getToken()));
        assertEquals(HttpStatus.OK, loggedInUser.getStatusCode());
    }


}
