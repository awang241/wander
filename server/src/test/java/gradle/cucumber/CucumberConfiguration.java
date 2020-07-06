package gradle.cucumber;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@ComponentScan(basePackages = {"com.springvuegradle.*"})
@PropertySource("classpath:application.properties")
public class CucumberConfiguration {

}
