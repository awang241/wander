package com.springvuegradle.config;

import com.springvuegradle.service.ActivityService;
import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;

@Profile("test")
@Configuration
public class ActivityServiceTestConfiguration {

    @Bean
    @Primary
    public ActivityService activityService() {
        return Mockito.mock(ActivityService.class);
    }
}
