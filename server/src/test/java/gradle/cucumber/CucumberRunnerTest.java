package gradle.cucumber;

import io.cucumber.junit.CucumberOptions;

//@RunWith(Cucumber.class)
@CucumberOptions(
        plugin = {"pretty"}, // How to format test report, "pretty" is good for human eyes
        glue = {"java.gradle.cucumber.steps"}, // Where to look for your tests' steps
        features = {"src/test/resources/library_app/features"}, // Where to look for your features
        strict = true // Causes cucumber to fail if any step definitions are still undefined
)

public class CucumberRunnerTest {

} // Classname ends with "Test" so it will be picked up by JUnit and hence by 'gradle test'