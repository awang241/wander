package gradle.cucumber.steps;

import com.springvuegradle.Application;
import com.springvuegradle.Controller.LoginController;
import com.springvuegradle.Controller.Profile_Controller;
import com.springvuegradle.dto.ProfileSearchResponse;
import com.springvuegradle.dto.ProfileSummary;
import com.springvuegradle.enums.EmailResponseMessage;
import com.springvuegradle.Model.PassportCountry;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.dto.LoginRequest;
import com.springvuegradle.dto.LoginResponse;
import com.springvuegradle.service.ProfileService;
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
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@ContextConfiguration(classes = Application.class)
@SpringBootTest
@WebAppConfiguration
public class ProfileTestSteps{

    private Profile_Controller profileController;
    private LoginController loginController;
    private ResponseEntity<String> createProfileResponse;
    private ResponseEntity<LoginResponse> loginResponse;
    private ResponseEntity<ProfileSearchResponse> searchResponse;
    private String dbPassword;

    @Autowired
    private ProfileService profileService;
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
        profileController = new Profile_Controller(profileService, repo, pcRepo, eRepo, aRepo, activityRepo, jwtUtil);
        //profileController = new Profile_Controller();
        createProfileResponse = null;
        loginResponse = null;
        searchResponse = null;
        dbPassword = null;
        eRepo.deleteAll();
        activityRepo.deleteAll();
        aRepo.deleteAll();
        repo.deleteAll();
        Profile steve = createProfileSteve("steve@google.com", "12345678");
        profileController.createProfile(steve);
        Profile dave = createProfileDave("dave@google.com", "12345678");
        profileController.createProfile(dave);
        repo.save(steve);
        repo.save(dave);
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

    static Profile createNormalProfileJackyWithFitnessLevel(String email, String password, Integer fitness_level) {
        return new Profile(null, "Jacky", "Jones", "J", "Jac", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", fitness_level, new String[]{}, new String[]{});
    }

    static Profile createNormalProfileJackyWithFitnessLevelPassportCountries(String email, String password, Integer fitness_level, String[] passport_countries) {
        return new Profile(null, "Jacky", "Jones", "J", "Jac", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", fitness_level, passport_countries, new String[]{});
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

    /** U2 - Fitness level and passport countries **/

    @When("I register an account with email {string} and password {string} and fitness level {int}")
    public void i_register_an_account_with_email_and_password_and_fitness_level(String email, String password, Integer fitness_level) {
        Profile jacky = createNormalProfileJackyWithFitnessLevel(email, password, fitness_level);
        createProfileResponse = profileController.createProfile(jacky);
    }

    @Then("An account with email {string} exists with fitness level {int}")
    public void an_account_with_email_exists_with_fitness_level(String email, Integer fitness_level) {
        Profile profile = repo.findByEmail(email).get(0);
        assertTrue(profile != null);
        assertEquals(fitness_level, profile.getFitness());
    }


    @When("I register an account with email {string} and password {string} and fitness level {int} and the following passport countries are added")
    public void i_register_an_account_with_email_and_password_and_fitness_level_and_the_following_passport_countries_are_added(String email, String password, Integer fitness_level, io.cucumber.datatable.DataTable dataTable) {
        System.out.println(pcRepo.count());
        ArrayList<String> passport_countries = new ArrayList<>();
        for (String name : dataTable.asList()) {
            if (!name.equals("name")) {
                passport_countries.add(name);
                System.out.println(name);
                System.out.println(pcRepo.existsByCountryName(name));
            }
        }
        String[] myArray = new String[passport_countries.size()];
        passport_countries.toArray(myArray);
        Profile jacky = createNormalProfileJackyWithFitnessLevelPassportCountries(email, password, fitness_level, myArray);
        createProfileResponse = profileController.createProfile(jacky);
        System.out.println(createProfileResponse.getBody());
    }

    @Then("An account with email {string} exists with fitness level {int} and the following passport countries")
    public void an_account_with_email_exists_with_fitness_level_and_the_following_passport_countries(String email, Integer fitness_level, io.cucumber.datatable.DataTable dataTable) {
        System.out.println(repo.count());
        Profile profile = repo.findByEmail(email).get(0);
        assertTrue(profile != null);
        assertEquals(fitness_level, profile.getFitness());
        ArrayList<String> passport_countries = new ArrayList<>();
        for (String name : dataTable.asList()) {
            passport_countries.add(name);
        }
        for (PassportCountry passportCountry: profile.getPassportObjects()) {
            assertTrue(passport_countries.contains(passportCountry.getCountryName()));
        }
    }

    /** U11 - Search for a user by name or email **/

    @When("I search by the surname {string}")
    public void searchBySurname(String surname) throws SQLException {
        searchResponse = profileController.getUserProfiles(null, surname, null,
                null , null ,99999 , 0,  loginResponse.getBody().getToken());
    }

    @Then("the search result will return the following users")
    public void surnameSearchResult(io.cucumber.datatable.DataTable dataTable) {
        ArrayList<String> expectedNames = new ArrayList<>();
        for (String name : dataTable.asList()) {
            expectedNames.add(name);
        }
        for (ProfileSummary profileSummary: searchResponse.getBody().getResults()) {
            assertTrue(expectedNames.contains(profileSummary.getLastname()));
        }
    }

    @When("I search by the full name {string}")
    public void searchByFullName(String fullName) throws SQLException {
        searchResponse = profileController.getUserProfiles(null, fullName, null,
                null , null ,99999 , 0,  loginResponse.getBody().getToken());
    }

    @Then("the search result will return the following user")
    public void fullNameSearchResult(io.cucumber.datatable.DataTable dataTable) {
        ArrayList<String> expectedNames = new ArrayList<>();
        for (String name : dataTable.asList()) {
            expectedNames.add(name);
        }
        for (ProfileSummary profileSummary: searchResponse.getBody().getResults()) {
            assertTrue(expectedNames.contains(profileSummary.getFirstname()));
        }
    }

    @When("I search by the email address {string}")
    public void searchByEmailAddress(String email) {
        searchResponse = profileController.getUserProfiles(null, null, email,
                null , null ,99999 , 0,  loginResponse.getBody().getToken());
    }

    @Then("the search result will return the user with the email address {string}")
    public void emailSearchResult(String email) {
        ProfileSummary profileSummary = searchResponse.getBody().getResults().get(0);
        assertEquals(profileSummary.getEmail(), email);
    }

    @When("I search by surname {string} and the email address {string}")
    public void searchBySurnameAndEmail(String surname, String email) {
        searchResponse = profileController.getUserProfiles(null, surname, email,
                null , null ,99999 , 0,  loginResponse.getBody().getToken());
    }

    @Then("the search result will return the profile with surname {string}")
    public void surnameAndSearchResult(String surname) {
        ProfileSummary profileSummary = searchResponse.getBody().getResults().get(0);
        assertEquals(profileSummary.getLastname(), surname);
    }

    @And("the email address {string}")
    public void andEmailSearchResult(String email) {
        ProfileSummary profileSummary = searchResponse.getBody().getResults().get(0);
        assertEquals(profileSummary.getEmail(), email);
    }

    @Then("the search result will return the profile with first name {string}")
    public void fullNameAndSearchResult(String firstName) {
        ProfileSummary profileSummary = searchResponse.getBody().getResults().get(0);
        assertEquals(profileSummary.getFirstname(), firstName);
    }

    @And("the surname {string}")
    public void fullNameAndSurnameAndSearchResult(String surname) {
        ProfileSummary profileSummary = searchResponse.getBody().getResults().get(0);
        assertEquals(profileSummary.getLastname(), surname);
    }

    static Profile createProfileSteve(String email, String password) {
        return new Profile(null, "Steve", "Tester", "Peter", "Steveo", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    static Profile createProfileDave(String email, String password) {
        return new Profile(null, "Dave", "Tester", "John", "Davey", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }
}
