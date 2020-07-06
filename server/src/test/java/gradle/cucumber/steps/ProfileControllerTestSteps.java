package gradle.cucumber.steps;

import com.springvuegradle.Controller.Profile_Controller;
import com.springvuegradle.Controller.enums.EmailResponseMessage;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.Utilities.JwtUtil;
import gradle.cucumber.CucumberConfiguration;
import io.cucumber.java.Before;
import io.cucumber.java.en.And;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;

import java.sql.SQLException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class ProfileControllerTestSteps extends SpringAcceptanceTest{

    private Profile_Controller profileController;
    private long returnedCountOfUsers;
    private ResponseEntity<String> profileResponse;

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
    public void resetLocalFields() throws SQLException {
        profileController = new Profile_Controller(repo, pcRepo, eRepo, aRepo, activityRepo, jwtUtil);
        returnedCountOfUsers = 0;
        profileResponse = null;
    }

    @Given("I register an account with email {string} and password {string}")
    public void i_register_an_account_with_email_and_password(String email, String password) throws SQLException {
        Profile jimmy = createNormalProfileJacky(email, password);
        profileResponse = profileController.createProfile(jimmy);
        assertFalse(repo.findByEmail(email).isEmpty());
    }

    @When("I get count of accounts that have been registered")
    public void i_get_count_of_accounts_that_have_been_registered() throws SQLException {
        returnedCountOfUsers = repo.count();
    }

    @Then("exactly {long} account(s) should be returned")
    public void exactly_account_should_be_returned(Long count) {
        assertEquals(count, returnedCountOfUsers);
    }

    @Then("I receive an error saying that email address is already in use")
    public void i_receive_an_error_saying_that_email_address_is_already_in_use() {
        assertEquals(profileResponse.getBody(), "An email address you have entered is already in use by another Profile.");
        assertEquals(profileResponse.getStatusCode(), HttpStatus.FORBIDDEN);
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

    @Given("No account with email {string} exists")
    public void noAccountWithEmailExists(String email) {
        List<Profile> emails = repo.findByEmail(email);
        assertTrue(emails.isEmpty());
    }

    @Then("I receive a {int} response code")
    public void iReceiveAError(int arg0) {
        assertEquals(arg0, profileResponse.getStatusCodeValue());
    }

    @Then("An account with email {string} exists")
    public void anAccountWithEmailExists(String arg0) {
        assertFalse(repo.findByEmail(arg0).isEmpty());
    }

    @And("I receive the error message {}")
    public void iReceiveTheErrorMessage(EmailResponseMessage arg0) {

    }
}
