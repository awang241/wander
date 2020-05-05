package com.springvuegradle.steps;

import com.springvuegradle.CucumberRunnerTest;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.service.ActivityService;
import io.cucumber.java.en.And;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

@ExtendWith(SpringExtension.class)
@DataJpaTest
public class ActivityTestSteps {

    @Autowired
    ProfileRepository profileRepository;

    @Autowired
    ActivityRepository activityRepository;

    @Autowired
    ActivityTypeRepository typeRepository;

    @Autowired
    ActivityService activityService;


    String activityName;
    String activityDescription;
    boolean isContinuous;
    String[] activityTypes;
    String location;
    int count;

    @Given("I register a continuous activity with name {string}")
    public void iRegisterAContinuousActivityWithName(String name) {
        activityName = name;
        isContinuous = true;
        location = "Tonga";
        activityTypes = new String[]{"Hiking"};
        activityDescription = "Cool race";
        Activity activity = new Activity();
        activityService.create(activity, 3L);
    }

    @When("I get count of activities that have been registered")
    public void i_get_count_of_activities_that_have_been_registered() {
        this.count = 1;
    }

    @Then("exactly {int} activity should be returned")
    public void exactly_activity_should_be_returned(Integer expectedCount) {

    }


}
