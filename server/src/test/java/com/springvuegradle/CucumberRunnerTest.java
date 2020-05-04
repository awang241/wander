package com.springvuegradle;

import io.cucumber.junit.CucumberOptions;
import io.cucumber.junit.Cucumber;
import org.junit.runner.RunWith;


@RunWith(Cucumber.class)
@CucumberOptions(
        plugin = {"pretty"}, // How to format test report, "pretty" is good for human eyes
        glue = {"springvuegradle.steps"}, // Where to look for your tests' steps
        features = {"classpath:springvuegradle/features/"}, // Where to look for your features
        strict = true // Causes cucumber to fail if any step definitions are still undefined
)

public class CucumberRunnerTest { } // Classname ends with "Test" so it will be picked up by JUnit and hence by 'gradle test'