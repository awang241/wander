package gradle.cucumber.steps;

import com.springvuegradle.Application;
import com.springvuegradle.Controller.LoginController;
import com.springvuegradle.Controller.Profile_Controller;
import com.springvuegradle.Controller.enums.EmailResponseMessage;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.dto.LoginRequest;
import com.springvuegradle.dto.LoginResponse;
import io.cucumber.java.Before;
import io.cucumber.java.en.And;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.web.WebAppConfiguration;

import java.sql.SQLException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ContextConfiguration(classes = Application.class)
@SpringBootTest
@WebAppConfiguration
public class ProfileTestSteps{

    private Profile_Controller profileController;
    private LoginController loginController;
    private ResponseEntity<String> createProfileResponse;
    private ResponseEntity<LoginResponse> loginResponse;
    private String dbPassword;

    @Autowired
    private ProfileRepository repo;
    @Autowired
    private JwtUtil jwtUtil;
    @Autowired
    private PassportCountryRepository pcRepo;
    @Autowired
    private EmailRepository eRepo;
    @Autowired
    private ActivityTypeRepository aRepo;
    @Autowired
    private ActivityRepository activityRepo;

    @Before
    public void setUp() {
        loginController = new LoginController(jwtUtil, eRepo);
        profileController = new Profile_Controller(repo, pcRepo, eRepo, aRepo, activityRepo, jwtUtil);
        //profileController = new Profile_Controller();
        createProfileResponse = null;
        loginResponse = null;
        dbPassword = null;
        eRepo.deleteAll();
        activityRepo.deleteAll();
        aRepo.deleteAll();
        repo.deleteAll();
    }

    @Given("I have registered an account with email {string} and password {string}")
    public void iHaveRegisteredAnAccountWithEmailAndPassword(String email, String password) {
        List<Profile> existingProfiles = repo.findByEmail(email);
        if (existingProfiles.isEmpty()) {
            Profile jimmy = createNormalProfileJacky(email, password);
            profileController.createProfile(jimmy);
            assertFalse(repo.findByEmail(email).isEmpty());
        } else {
            Profile existingProfile = existingProfiles.iterator().next();
            String hashedPassword = Profile_Controller.hashPassword(password);
            if (!existingProfile.getPassword().equals(hashedPassword)) {
                existingProfile.setPassword(hashedPassword);
                repo.save(existingProfile);
            }
        }
    }

    @Given("No account with email {string} exists")
    public void noAccountWithEmailExists(String email) {
        List<Profile> emails = repo.findByEmail(email);
        assertTrue(emails.isEmpty());
    }


    @When("I log in using email {string} and password {string}")
    public void i_log_in_using_email_and_password(String email, String password) throws SQLException {
        LoginRequest request = new LoginRequest(email, password);
        loginResponse = loginController.loginUser(request);
    }

    @When("I register an account with email {string} and password {string}")
    public void i_register_an_account_with_email_and_password(String email, String password) throws SQLException {
        Profile jimmy = createNormalProfileJacky(email, password);
        createProfileResponse = profileController.createProfile(jimmy);
    }

    @Then("I receive an error saying that email address is already in use")
    public void i_receive_an_error_saying_that_email_address_is_already_in_use() {
        assertEquals("An email address you have entered is already in use by another Profile.", createProfileResponse.getBody());
        assertEquals(HttpStatus.FORBIDDEN, createProfileResponse.getStatusCode());
    }


    /* Below are a set of ready-made Profile objects which can be used for various tests. */

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileJacky(String email, String password) {
        return new Profile(null, "Jacky", "Jones", "J", "Jac", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    @Then("I receive a {int} response code")
    public void iReceiveAError(int arg0) {
        assertEquals(arg0, createProfileResponse.getStatusCodeValue());
    }

    @Then("An account with email {string} exists")
    public void anAccountWithEmailExists(String arg0) {
        assertFalse(repo.findByEmail(arg0).isEmpty());
    }

    @And("I receive an error saying {string}")
    public void iReceiveAnErrorSaying(String arg0) {
        assertEquals(arg0, createProfileResponse.getBody());
    }

    @Then("I receive a {string} email error message")
    public void iReceiveAnEmailErrorMessage(String arg0) {
        String expectedMessage = EmailResponseMessage.valueOf(arg0).getMessage().strip();
        assertEquals(expectedMessage, createProfileResponse.getBody().strip());
    }

    @Then("I am successfully logged in")
    public void i_am_successfully_logged_in() {
        assertTrue(jwtUtil.validateToken(loginResponse.getBody().getToken()));
        assertEquals(HttpStatus.OK, loginResponse.getStatusCode());
    }

    @Then("I receive a login error message")
    public void iReceiveALoginErrorMessage() {
        assertEquals(HttpStatus.UNAUTHORIZED, loginResponse.getStatusCode());
    }

    @When("I query the database for the password of the user account with email {string}")
    public void iQueryTheDatabaseForThePasswordOfTheUserAccountWithEmail(String arg0) {
        Profile existingProfile = repo.findByEmail(arg0).iterator().next();
        if (existingProfile != null) {
            dbPassword = existingProfile.getPassword();
        }
    }

    @Then("the password field is not equal to {string}")
    public void thePasswordFieldIsNotEqualTo(String arg0) {
        assertNotEquals(arg0, dbPassword);
    }

    @And("the password field is equal to the hash of {string}")
    public void thePasswordFieldIsEqualToTheHashOf(String arg0) {
        assertEquals(Profile_Controller.hashPassword(arg0), dbPassword);
    }
}
