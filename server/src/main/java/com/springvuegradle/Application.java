package com.springvuegradle;

import com.springvuegradle.Model.Activity;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.Utilities.ValidationHelper;
import com.springvuegradle.service.ActivityService;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.SpringApplication;

import java.time.format.DateTimeFormatter;
import java.util.*;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.Ordered;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;


import static com.springvuegradle.Controller.Profile_Controller.hashPassword;

@SpringBootApplication
@ComponentScan({"com.springvuegradle.Controller", "com.springvuegradle.Utilities", "com.springvuegradle.service"})
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

    @Bean
    CommandLineRunner init(EmailRepository emailRepository, ProfileRepository profileRepository,
                           PassportCountryRepository passportCountryRepository, ActivityTypeRepository activityTypeRepository,
                           ActivityRepository activityRepository, ActivityMembershipRepository activityMembershipRepository) {
        return args -> {
            System.out.println("-----Updating Passport Country Repository-----");
            ValidationHelper.updatePassportCountryRepository(passportCountryRepository, profileRepository);
            System.out.println("-----Update Complete-----");
            System.out.println("-----Program should be running now-----");
        };
    }

    // Fix the CORS errors
    @Bean
    public FilterRegistrationBean simpleCorsFilter() {
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        CorsConfiguration config = new CorsConfiguration();
        config.setAllowCredentials(true);
        // *** URL below needs to match the Vue client URL and port ***
        config.setAllowedOrigins(new ArrayList(Arrays.asList("http://localhost:9000", "http://localhost:9499", "http://localhost:9500", "https://csse-s302g0.canterbury.ac.nz/test", "https://csse-s302g0.canterbury.ac.nz/prod")));
        config.setAllowedMethods(Collections.singletonList("*"));
        config.setAllowedHeaders(Collections.singletonList("*"));
        source.registerCorsConfiguration("/**", config);
        FilterRegistrationBean bean = new FilterRegistrationBean<>(new CorsFilter(source));
        bean.setOrder(Ordered.HIGHEST_PRECEDENCE);
        return bean;
    }

}