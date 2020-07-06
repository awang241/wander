package gradle.cucumber.steps;

import com.springvuegradle.Application;
import gradle.cucumber.CucumberConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.web.WebAppConfiguration;

@ContextConfiguration(classes = Application.class)
@SpringBootTest
@WebAppConfiguration
public class SpringAcceptanceTest {
}
