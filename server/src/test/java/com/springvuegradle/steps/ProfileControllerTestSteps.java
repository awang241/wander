package com.springvuegradle.steps;

import com.springvuegradle.Controller.Profile_Controller;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ProfileRepository;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.sql.SQLException;
import java.util.Calendar;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ProfileControllerTestSteps {

    private Profile_Controller profileController;
    private long returnedCountOfUsers;
    private ResponseEntity<String> profileResponse;


    @Autowired
    private ProfileRepository repo;


    @Before
    public void resetLocalFields() throws SQLException {
        profileController = new Profile_Controller();
        returnedCountOfUsers = 0;
        profileResponse = null;
    }

    @Given("I register an account with email {string} and password {string}")
    public void i_register_an_account_with_email_and_password(String email, String password) throws SQLException {
        Profile jimmy = createNormalProfileJacky(email, password);
        profileResponse = profileController.createProfile(jimmy);

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

}
