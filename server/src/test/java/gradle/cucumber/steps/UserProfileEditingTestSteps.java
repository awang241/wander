package gradle.cucumber.steps;

import com.springvuegradle.controller.LoginController;
import com.springvuegradle.controller.Profile_Controller;
import com.springvuegradle.dto.requests.LoginRequest;
import com.springvuegradle.dto.responses.LoginResponse;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.EmailRepository;
import com.springvuegradle.repositories.ProfileRepository;
import com.springvuegradle.service.ProfileService;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.junit.jupiter.api.AfterEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@Transactional
public class UserProfileEditingTestSteps {

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

    @Given("I have registered an account with email {string} and the password {string} and a bio stating {string}")
    public void i_have_registered_an_account_with_email_and_the_password_and_a_bio_stating(String email, String password, String bio) {
        profile = createNormalProfileBio(email, password, bio);
        profileController.createProfile(profile);
        LoginRequest loginRequest = new LoginRequest(email, password);
        loginResponse = loginController.loginUser(loginRequest).getBody();
    }

    @When("I change the bio to {string}")
    public void i_change_the_bio_to(String bio) {
        profile = profileRepository.findByPrimaryEmail(profile.getPrimary_email()).get(0);
        profile.setBio(bio);
        profileController.updateProfile(profile, loginResponse.getToken(), profile.getId());
    }

    @Then("An account with email {string} exists with the new bio {string}")
    public void an_account_with_email_exists_with_the_new_bio(String email, String bio) {
        assertNotNull(profile = profileRepository.findByPrimaryEmail(email).get(0));
        assertEquals(bio, profile.getBio());
    }

    @Given("I have registered an account with email {string} and the password {string} and the gender {string}")
    public void i_have_registered_an_account_with_email_and_the_password_and_the_gender(String email, String password, String gender) {
        profile = createNormalProfileGender(email, password, gender);
        profileController.createProfile(profile);
        LoginRequest loginRequest = new LoginRequest(email, password);
        loginResponse = loginController.loginUser(loginRequest).getBody();
    }

    @When("I change the gender to {string}")
    public void i_change_the_gender_to(String gender) {
        profile = profileRepository.findByPrimaryEmail(profile.getPrimary_email()).get(0);
        profile.setGender(gender);
        profileController.updateProfile(profile, loginResponse.getToken(), profile.getId());
    }

    @Then("An account with email {string} exists with the gender {string}")
    public void an_account_with_email_exists_with_the_gender(String email, String gender) {
        assertNotNull(profile = profileRepository.findByPrimaryEmail(email).get(0));
        assertEquals(gender, profile.getGender());
    }

    @Given("I have registered an account with email {string} and the password {string} and nickname {string}")
    public void i_have_registered_an_account_with_email_and_the_password_and_nickname(String email, String password, String nickname) {
        profile = createNormalProfileNickname(email, password, nickname);
        profileController.createProfile(profile);
        LoginRequest loginRequest = new LoginRequest(email, password);
        loginResponse = loginController.loginUser(loginRequest).getBody();
    }

    @When("I remove the nickname by setting it to an empty string {string} for the account with the primary email {string}")
    public void i_remove_the_nickname_by_setting_it_to_an_empty_string_for_the_account_with_the_primary_email(String nickname, String email) {
        profile = profileRepository.findByPrimaryEmail(email).get(0);
        profile.setNickname(nickname);
        profileController.updateProfile(profile, loginResponse.getToken(), profile.getId());
    }

    @Then("An account with the email {string} exists with an empty string {string} as the nickname")
    public void an_account_with_the_email_exists_with_an_empty_string_as_the_nickname(String email, String nickname) {
        assertNotNull(profile = profileRepository.findByPrimaryEmail(email).get(0));
        assertEquals(nickname, profile.getNickname());
    }


    private Profile createNormalProfile(String email, String password) {
        return new Profile(1L, "Testfname", "Testlname", "Middlenametest", "Nicknametest", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    private Profile createNormalProfileBio(String email, String password, String bio) {
        return new Profile(1L, "Testfname", "Testlname", "Middlenametest", "Nicknametest", email, new String[]{}, password,
                bio, new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    private Profile createNormalProfileGender(String email, String password, String gender) {
        return new Profile(1L, "Testfname", "Testlname", "Middlenametest", "Nicknametest", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), gender, 1, new String[]{}, new String[]{});
    }

    private Profile createNormalProfileNickname(String email, String password, String nickname) {
        return new Profile(1L, "Testfname", "Testlname", "Middlenametest", nickname, email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }
}
