package gradle.cucumber.steps;

import com.springvuegradle.controller.LoginController;
import com.springvuegradle.controller.Profile_Controller;
import com.springvuegradle.dto.EmailAddRequest;
import com.springvuegradle.dto.EmailUpdateRequest;
import com.springvuegradle.dto.LoginRequest;
import com.springvuegradle.dto.LoginResponse;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@Transactional
public class EmailTestSteps {

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

    @Given("I have registered an account with email {string} and the password {string}")
    public void i_have_registered_an_account_with_email_and_the_password(String email, String password) {
        profile = createNormalProfile(email, password);
        profileController.createProfile(profile);
        LoginRequest loginRequest = new LoginRequest(email, password);
        loginResponse = loginController.loginUser(loginRequest).getBody();
    }

    @Then("An account with the email {string} exists")
    public void an_account_with_the_email_exists(String email) {
        assertNotNull(profile = profileRepository.findByEmail(email).get(0));
    }

    @When("I set the new primary email to {string} and set the old primary {string} to an additional email")
    public void i_set_the_new_primary_email_to_and_set_the_old_primary_to_an_additional_email(String primary, String additional) {
        ArrayList<String> additional_emails = new ArrayList<String>();
        additional_emails.add(additional);
        EmailUpdateRequest emailUpdateRequest = new EmailUpdateRequest(additional_emails, primary, Math.toIntExact(profile.getId()));
        profileController.editEmails(emailUpdateRequest, profile.getId(), loginResponse.getToken());
    }

    @Then("An account with the email {string} exists with the additional email address {string}")
    public void an_account_with_the_email_exists_with_the_additional_email_address(String primary, String additional) {
        assertNotNull(profile = profileRepository.findByPrimaryEmail(primary).get(0));
        assertEquals(1, profile.getAdditional_email().size());
        for (String additional_email_found: profile.getAdditional_email()) {
            assertEquals(additional, additional_email_found);
        }
    }

    @When("The following additional emails are added")
    public void the_following_additional_emails_are_added(io.cucumber.datatable.DataTable dataTable) {
        ArrayList<String> additional_emails = new ArrayList<>();
        for (String additional_email_to_add : dataTable.asList()) {
            additional_emails.add(additional_email_to_add);
        }
        EmailAddRequest emailAddRequest = new EmailAddRequest(additional_emails);
        profileController.addEmails(emailAddRequest, profile.getId(), loginResponse.getToken());
    }

    @Then("An account with email {string} exists with the following additional emails")
    public void an_account_with_email_exists_with_the_following_additional_emails(String primary, io.cucumber.datatable.DataTable dataTable) {
        assertNotNull(profile = profileRepository.findByPrimaryEmail(primary).get(0));
        for (String actual_additional_email : dataTable.asList()) {
            assertTrue(profile.getAdditional_email().contains(actual_additional_email));
        }
    }

    @When("I change the primary email {string} to the new primary email {string}")
    public void i_change_the_primary_email_to_the_new_primary_email(String old_primary, String new_primary) {
        ArrayList<String> additional_emails = new ArrayList<String>();
        EmailUpdateRequest emailUpdateRequest = new EmailUpdateRequest(additional_emails, new_primary, Math.toIntExact(profile.getId()));
        profileController.editEmails(emailUpdateRequest, profile.getId(), loginResponse.getToken());
    }


    private Profile createNormalProfile(String email, String password) {
        return new Profile(1L, "Testfname", "Testlname", "Middlenametest", "Nicknametest", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }
}
