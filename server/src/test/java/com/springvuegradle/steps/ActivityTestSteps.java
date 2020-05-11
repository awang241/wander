package com.springvuegradle.steps;

import com.springvuegradle.Controller.ActivityController;
import com.springvuegradle.CucumberRunnerTest;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.junit.Assert;
import org.junit.Before;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.mockito.Mockito.*;

@AutoConfigureMockMvc
public class ActivityTestSteps extends CucumberRunnerTest {

    public static String profileId;
    private String jsonString;
    private MockHttpSession token;

    @Autowired
    private MockMvc mvc;


    ActivityController activityController;

//    @Before
//    public void setup() throws Exception {
//        MockitoAnnotations.initMocks(this);
////        this.activityController = new ActivityController();
//        this.mvc = MockMvcBuilders.standaloneSetup(activityController).build();
//    }

    @Given("I register a user")
    public void i_register_a_user() throws Exception {
        token = new MockHttpSession();
        jsonString = "{\r\n  \"lastname\": \"Jones\",\r\n  \"firstname\": \"Bob\",\r\n  \"middlename\": \"B\",\r\n  \"nickname\": \"Bobby\",\r\n  \"primary_email\": \"bob@gmail.com\",\r\n  \"password\": \"bobby123\",\r\n  \"bio\": \"Bobs page\",\r\n  \"date_of_birth\": \"2000-05-04\",\r\n  \"gender\": \"female\"\r\n}";
        profileId = mvc.perform(MockMvcRequestBuilders
                .post("/profiles")
                .content(jsonString)
                .contentType(MediaType.APPLICATION_JSON)
                .session(token)
        ).andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn().getResponse().getContentAsString();
    }

//    @Given("I am logged in with email {string} and password {string}")
//    public void iAmLoggedInWithEmailAndPassword(String email, String password) throws Exception {
//        token = new MockHttpSession();
//        jsonString = "{\n" +
//                "\t\"email\": \"" + email + "\",\n" +
//                "\t\"password\": \"" + password + "\"\n" +
//                "}";
//        profileId = mvc.perform(MockMvcRequestBuilders
//                .post("/login")
//                .content(jsonString)
//                .contentType(MediaType.APPLICATION_JSON)
//                .session(token)
//        ).andExpect(MockMvcResultMatchers.status().isOk())
//        .andReturn().getResponse().getContentAsString();
//    }

    @When("I register an activity with name {string} and description {string}")
    public void iRegisterAnActivityWithName(String activityName, String activityDescription) throws Exception {
        token = new MockHttpSession();
        jsonString = "{\n" +
                "  \"activity_name\": \"" + activityName + "\",\n" +
                "  \"description\": \"" + activityDescription + "\",\n" +
                "  \"activity_type\": [\n" +
                "    \"" + "Hiking" + "\"\n" +
                "  ]\n" +
                "  \"continuous\": \"" + true + "\",\n" +
                "  \"start_time\": \"" + null + "\",\n" +
                "  \"end_time\": \"" + null + "\",\n" +
                "  \"location\": \"Kaikoura, NZ\"\n" +
                "}\n";
        System.out.println(jsonString);
        mvc.perform(MockMvcRequestBuilders
                .post("/profiles/"+profileId+"/activities")
                .content(jsonString)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .session(token))
                .andExpect(MockMvcResultMatchers.status().isCreated());
    }

    @Then("I check {string} exists in list of activities that have been registered")
    public void i_check_exists_in_list_of_activities_that_have_been_registered(String activityName) throws Exception {
        String responseData = mvc.perform(MockMvcRequestBuilders
                .get("/profiles/{id}/activities", profileId)
                .session(token)
        ).andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn().getResponse().getContentAsString();
        Assert.assertTrue(responseData.contains(activityName));

    }

}