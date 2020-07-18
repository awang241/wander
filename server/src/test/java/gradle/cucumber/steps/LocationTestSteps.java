package gradle.cucumber.steps;

import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileLocation;
import com.springvuegradle.Repositories.ProfileLocationRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.junit.jupiter.api.AfterEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import java.util.Calendar;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.assertEquals;

@Transactional
public class LocationTestSteps {

    @Autowired
    ProfileRepository profileRepository;

    @Autowired
    ProfileLocationRepository profileLocationRepository;
    
    private String country;
    private String city;
    private Profile profile;

    @AfterEach()
    private void tearDown(){
        profileRepository.deleteAll();
    }

    @Given("A profile exists with no location")
    public void aProfileExistsWithNoLocation() {
        profile = createNormalProfileJacky("jacky@gmail.com", "password");
        profileRepository.save(profile);
    }


    @When("I select the country {string} and city {string}")
    public void iSelectTheCountryAndCity(String country, String city) {
        this.country = country;
        this.city = city;
        ProfileLocation location = new ProfileLocation();
        location.setCity(city);
        location.setCountry(country);
        profile.setLocation(location);
        profileRepository.save(profile);
    }

    @Then("The profile has the country {string} and city {string}")
    public void theProfileHasTheCountryAndCity(String country, String city) {
        Profile profileFromDatabase = profileRepository.getOne(profile.getId());
        assertEquals(profileFromDatabase.getProfileLocation().getCity(), city);
        assertEquals(profileFromDatabase.getProfileLocation().getCountry(), country);
    }


    private Profile createNormalProfileJacky(String email, String password) {
        return new Profile(1L, "Jacky", "Jones", "J", "Jac", email, new String[]{}, password,
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    @When("I select the country {string} and city {string} and the state {string}")
    public void iSelectTheCountryAndCityAndTheState(String country, String city, String state) {
        ProfileLocation location = new ProfileLocation();
        location.setCity(city);
        location.setCountry(country);
        location.setState(state);
        profile.setLocation(location);
        profileRepository.save(profile);
    }

    @Then("The profile has the country {string} city {string} and state {string}")
    public void theProfileHasTheCountryCityAndState(String country, String city, String state) {
        Profile profileFromDatabase = profileRepository.getOne(profile.getId());
        assertEquals(profileFromDatabase.getProfileLocation().getCity(), city);
        assertEquals(profileFromDatabase.getProfileLocation().getCountry(), country);
        assertEquals(profileFromDatabase.getProfileLocation().getState(), state);
    }
}
