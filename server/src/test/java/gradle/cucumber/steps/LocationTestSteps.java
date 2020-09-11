package gradle.cucumber.steps;

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
import org.springframework.transaction.annotation.Transactional;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

@Transactional
public class LocationTestSteps {

    @Autowired
    ProfileRepository profileRepository;

    @Autowired
    ProfileLocationRepository profileLocationRepository;

    @Autowired
    ProfileService profileService;

    @Autowired
    EmailRepository emailRepository;

    private Profile profile;

    @AfterEach()
    private void tearDown() {
        profileRepository.deleteAll();
    }

    @Given("A profile exists with no location")
    public void aProfileExistsWithNoLocation() {
        profile = createNormalProfileJacky("jacky@gmail.com", "password");
        Email email = new Email("jacky@gmail.com");
        emailRepository.save(email);
        email.setProfile(profile);
        profile.setEmails(Set.copyOf(List.of(email)));
        profileRepository.save(profile);
    }


    @When("I select the country {string} and city {string}")
    public void iSelectTheCountryAndCity(String country, String city) {
        ProfileLocation location = new ProfileLocation(city, "", country);
        profile.setLocation(location);
        profileService.updateProfileLocation(location, profile.getId());
    }

    @Then("The profile has the country {string} and city {string}")
    public void theProfileHasTheCountryAndCity(String country, String city) {
        Profile profileFromDatabase = profileRepository.getOne(profile.getId());
        String address = String.format("%s, %s, %s.", city, "", country);
        assertEquals(profileFromDatabase.getProfileLocation().getAddress(), address);
    }


    @When("I select the country {string} and city {string} and the state {string}")
    public void iSelectTheCountryAndCityAndTheState(String country, String city, String state) {
        ProfileLocation location = new ProfileLocation(city, state, country);
        profileService.updateProfileLocation(location, profile.getId());
    }

    @Then("The profile has the country {string} city {string} and state {string}")
    public void theProfileHasTheCountryCityAndState(String country, String city, String state) {
        Profile profileFromDatabase = profileRepository.getOne(profile.getId());
        String address = String.format("%s, %s, %s.", city, state, country);
        assertEquals(profileFromDatabase.getProfileLocation().getAddress(), address);
    }

    @When("I select the country {string} and the State {string}")
    public void iSelectTheCountryAndTheState(String country, String state) {
        ProfileLocation location = new ProfileLocation("", state, country);

        profileService.updateProfileLocation(location, profile.getId());
    }

    @Then("The profile location is not added to the database")
    public void theProfileLocationIsNotAddedToTheDatabase() {
        assertNull(profile.getProfileLocation());
    }

    @Given("A profile exists with city {string} and country {string}")
    public void aProfileExistsWithCityAndCountry(String city, String country) {
        profile = createNormalProfileJacky("email@gmail.com", "password");
        ProfileLocation location = new ProfileLocation(city, "", country);
        profile.setLocation(location);
        location.setProfile(profile);
        profileRepository.save(profile);
    }

    @When("I select {string} as country {string} city")
    public void iSelectAsCountryCity(String country, String city) {
        ProfileLocation location = new ProfileLocation(city, "", country);
        profileService.updateProfileLocation(location, profile.getId());
    }

    @Then("The profile should now have {string} As country and {string} as city")
    public void theProfileShouldNowHaveAsCountryAndAsCity(String country, String city) {
        ProfileLocation location = profileRepository.getOne(profile.getId()).getProfileLocation();
        assertEquals(location.getAddress(), String.format("%s, %s, %s.", city, "", country));
    }

    @When("I choose to delete the location from this profile")
    public void iChooseToDeleteTheLocationFromThisProfile() {
        profileLocationRepository.delete(profile.getProfileLocation());
        profile.setLocation(null);
        profileRepository.save(profile);
    }

    @Then("the profile now has no location")
    public void theProfileNowHasNoLocation() {
        assertNull(profile.getProfileLocation());
    }

    @When("I select the address {string}, longitude {string} and latitude {string}")
    public void iSelectTheAddressLongitudeAndLatitude(String address, String str_longitude, String str_latitude) {
        ProfileLocation location = new ProfileLocation(Double.parseDouble(str_longitude), Double.parseDouble(str_latitude), address);
        profileService.updateProfileLocation(location, profile.getId());
    }

    @Then("the profile has the address {string}, longitude {string} and latitude {string}")
    public void the_profile_has_the_address_longitude_and_latitude(String address, String str_longitude, String str_latitude) {
        ProfileLocation location = profileRepository.getOne(profile.getId()).getProfileLocation();
        assertEquals(location.getAddress(), address);
        assertEquals(location.getLongitude(), Double.parseDouble(str_longitude));
        assertEquals(location.getLatitude(), Double.parseDouble(str_latitude));
    }

    private Profile createNormalProfileJacky(String email, String password) {
        return new Profile(1L, "Jacky", "Jones", "J", "Jac", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }
}
