package gradle.cucumber.steps;

import com.springvuegradle.controller.LoginController;
import com.springvuegradle.controller.Profile_Controller;
import com.springvuegradle.dto.*;
import com.springvuegradle.model.Email;
import com.springvuegradle.model.Profile;
import com.springvuegradle.model.ProfileLocation;
import com.springvuegradle.repositories.EmailRepository;
import com.springvuegradle.repositories.ProfileLocationRepository;
import com.springvuegradle.repositories.ProfileRepository;
import com.springvuegradle.service.ProfileService;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.junit.jupiter.api.AfterEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.ProfileController;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@Transactional
public class PasswordTestSteps {

    @Autowired
    ProfileRepository profileRepository;

    @Autowired
    ProfileService profileService;

    @Autowired
    EmailRepository emailRepository;

    @Autowired
    Profile_Controller profileController;

    @Autowired
    LoginController loginController;

    private Profile profile;

    private LoginResponse loginResponse;

    @AfterEach()
    private void tearDown() {
        profileRepository.deleteAll();
        emailRepository.deleteAll();
    }

    @Given("I have registered account with email {string} and password {string}")
    public void i_have_registered_account_with_email_and_password(String email, String password) {
        profile = createNormalProfile(email, password);
        assertEquals(201, profileController.createProfile(profile).getStatusCodeValue());
    }

    @When("I change the password from {string} to {string}")
    public void i_change_the_password_from_to(String old_password, String new_password) {
        ChangePasswordRequest passwordRequest = new ChangePasswordRequest(profile.getId(), old_password, new_password, new_password);
        assertEquals(200, profileController.changePassword(passwordRequest, profile.getId(), null, true).getStatusCodeValue());
    }

    @Then("I can login with the email {string} and the password {string}")
    public void i_can_login_with_the_email_and_the_password(String email, String password) {
        LoginRequest loginRequest = new LoginRequest(email, password);
        ResponseEntity<LoginResponse> loginResponseEntity = loginController.loginUser(loginRequest);
        loginResponse = loginResponseEntity.getBody();
        assertEquals(200, loginResponseEntity.getStatusCodeValue());
    }

    private Profile createNormalProfile(String email, String password) {
        return new Profile(1L, "Testfname", "Testlname", "Middlenametest", "Nicknametest", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }
}
