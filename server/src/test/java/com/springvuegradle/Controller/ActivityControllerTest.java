package com.springvuegradle.Controller;

import com.springvuegradle.Controller.enums.ActivityResponseMessage;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.hibernate.type.CalendarTimeType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivityControllerTest {

    @Autowired
    ActivityController controller;
    @Autowired
    ProfileRepository profileRepository;
    @Autowired
    ActivityRepository activityRepository;
    @Autowired
    static ActivityTypeRepository typeRepository;

    @BeforeAll
    static void setUpBeforeClass() {
        populateActivityTypes();
    }

    @BeforeEach
    void setUp() {

    }

    @AfterEach
    void tearDown() {
        profileRepository.deleteAll();
        activityRepository.deleteAll();
    }

    private static void populateActivityTypes() {
        typeRepository.save(new ActivityType("football"));
        typeRepository.save(new ActivityType("tennis"));
        typeRepository.save(new ActivityType("hockey"));
        typeRepository.save(new ActivityType("basketball"));
        typeRepository.save(new ActivityType("hiking"));
        typeRepository.save(new ActivityType("tramping"));
        typeRepository.save(new ActivityType("rock climbing"));
    }
}