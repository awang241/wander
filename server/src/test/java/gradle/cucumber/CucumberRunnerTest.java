package gradle.cucumber;

import com.springvuegradle.Application;
import com.springvuegradle.Controller.ActivityController;
import io.cucumber.junit.CucumberOptions;
import io.cucumber.junit.Cucumber;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.web.WebAppConfiguration;

@RunWith(Cucumber.class)
@CucumberOptions(
        plugin = {"pretty"}, // How to format test report, "pretty" is good for human eyes
        glue = {"java.gradle.cucumber.steps"}, // Where to look for your tests' steps
        features = {"features"}, // Where to look for your features
        strict = true // Causes cucumber to fail if any step definitions are still undefined
)

public class CucumberRunnerTest { } // Classname ends with "Test" so it will be picked up by JUnit and hence by 'gradle test'