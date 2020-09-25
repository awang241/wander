package com.springvuegradle.config;

import com.springvuegradle.service.*;
import com.springvuegradle.utilities.JwtUtil;
import org.mockito.Mockito;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;

@Profile("mock-service")
@TestConfiguration
public class MockServiceConfig {

    @Bean
    @Primary
    public ActivityService mockActivityService() {
        return Mockito.mock(ActivityService.class);
    }

    @Bean
    @Primary
    public ActivitySearchService mockActivitySearchService() {
        return Mockito.mock(ActivitySearchService.class);
    }

    @Bean
    @Primary
    public SecurityService mockSecurityService() {
        return Mockito.mock(SecurityService.class);
    }

    @Bean
    @Primary
    public ProfileService mockProfileService() {
        return Mockito.mock(ProfileService.class);
    }

    @Bean
    @Primary
    public NotificationService mockNotificationService() {
        return  Mockito.mock(NotificationService.class);
    }

    @Bean
    @Primary
    public JwtUtil mockJwtUtil() {
        return Mockito.mock(JwtUtil.class);
    }
}
