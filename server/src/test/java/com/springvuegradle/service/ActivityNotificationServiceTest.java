package com.springvuegradle.service;

import com.springvuegradle.controller.ActivityController;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import com.springvuegradle.utilities.InitialDataHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import java.util.*;

import static com.springvuegradle.service.ActivityServiceTest.createNormalActivity;
import static com.springvuegradle.service.ActivityServiceTest.createNormalProfileBen;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivityNotificationServiceTest {

    @Autowired
    ActivityController aController;
    @Autowired
    ActivityService aService;
    @Autowired
    ProfileRepository pRepo;
    @Autowired
    ActivityRepository aRepo;
    @Autowired
    ActivityTypeRepository tRepo;
    @Autowired
    ActivityMembershipRepository amRepo;
    @Autowired
    EmailRepository eRepo;
    @Autowired
    ActivityParticipationRepository apRepo;
    @Autowired
    NotificationRepository nRepo;
    @Autowired
    NotificationService nService;

    /**
     * Needs to be run before each test to create new test profiles and repositories.
     */
    @BeforeEach
    void setUp() {
        InitialDataHelper.init(tRepo, pRepo, eRepo);
    }

    /**
     * Needs to be run after each test to ensure the repositories are emptied.
     */
    @AfterEach
    void tearDown() {
        amRepo.deleteAll();
        eRepo.deleteAll();
        nRepo.deleteAll();
        pRepo.deleteAll();
        aRepo.deleteAll();
        tRepo.deleteAll();

    }

    /**
     * Test to create a basic new activity
     **/
    @Test
    void createNewActivityPostsNotificationTest() {
        assertEquals(0, nRepo.count());
        Profile profile = pRepo.save(createNormalProfileBen());
        aService.create(createNormalActivity(), profile.getId());
        assertEquals(1, nRepo.count());
        String message = "Ben James Sales created a new activity called Kaikoura Coast Track race.";
        assertEquals(message, nRepo.findAll().get(0).getMessage());
    }
}