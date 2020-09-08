package com.springvuegradle.service;

import com.springvuegradle.controller.ActivityController;
import com.springvuegradle.enums.NotificationType;
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
        apRepo.deleteAll();
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
        Notification notification = nRepo.findAll().get(0);
        assertEquals(message, notification.getMessage());
        assertNotNull(notification.getActivity());
        assertEquals(1, profile.getNotifications().size());
        assertEquals(1, notification.getActivity().getNotifications().size());
        assertEquals(1, notification.getRecipients().size());
    }

    /**
     * Test to create a basic new activity
     **/
    @Test
    void deleteActivityPostsNotificationTest() {
        Profile profile = pRepo.save(createNormalProfileBen());
        aService.create(createNormalActivity(), profile.getId());
        assertEquals(1, nRepo.count());
        aService.delete(aRepo.getLastInsertedId(), profile.getId());
        assertEquals(2, nRepo.count());
        String message = "Ben James Sales deleted an activity called Kaikoura Coast Track race.";
        assertEquals(message, nRepo.findAll().get(1).getMessage());
    }


    /**
     *  Tests that when creating an activity's participation, a notification is successfully created
     */
    @Test
    void createParticipationPostsNotificationTest() {
        long profileId = pRepo.getLastInsertedId();
        aService.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = aRepo.getLastInsertedId();
        ActivityParticipation participation = ActivityTestUtils.createNormalParticipation();
        aService.createParticipation(activityId, profileId, participation);
        assertEquals(2, nRepo.count());
    }

    /**
     *  Tests that when creating an activity's participation, a notification is successfully created and has correct type.
     */
    @Test
    void createParticipationHasCorrectNotificationTypeTest() {
        long profileId = pRepo.getLastInsertedId();
        aService.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = aRepo.getLastInsertedId();
        ActivityParticipation participation = ActivityTestUtils.createNormalParticipation();
        aService.createParticipation(activityId, profileId, participation);
        assertEquals(NotificationType.ParticipantCreated, nRepo.findAll().get(1).getNotificationType());
    }

    /**
     *  Tests that when editing an activity's participation, a notification is successfully created
     */
    @Test
    void editParticipationPostsNotificationTest() {
        long profileId = pRepo.getLastInsertedId();
        aService.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = aRepo.getLastInsertedId();

        ActivityParticipation activityParticipation = ActivityTestUtils.createNormalParticipation();
        aService.createParticipation(activityId, profileId, activityParticipation);
        long participationId = activityParticipation.getId();
        ActivityParticipation editedParticipation = ActivityTestUtils.createEditedNormalParticipation();
        aService.editParticipation(activityId, profileId, participationId, editedParticipation);
        assertEquals(3, nRepo.count());
    }

    /**
     *  Tests that when editing an activity's participation, a notification is successfully created and and has correct type.
     */
    @Test
    void editParticipationHasCorrectNotificationTypeTest() {
        long profileId = pRepo.getLastInsertedId();
        aService.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = aRepo.getLastInsertedId();

        ActivityParticipation activityParticipation = ActivityTestUtils.createNormalParticipation();
        aService.createParticipation(activityId, profileId, activityParticipation);
        long participationId = activityParticipation.getId();
        ActivityParticipation editedParticipation = ActivityTestUtils.createEditedNormalParticipation();
        aService.editParticipation(activityId, profileId, participationId, editedParticipation);
        assertEquals(NotificationType.ParticipationEdited, nRepo.findAll().get(2).getNotificationType());
    }


    /**
     * Test to create a notification for editing or updating an activity
     */
    @Test
    void editActivityPostsNotificationTest() {
        Profile profile = pRepo.save(createNormalProfileBen());
        aService.create(createNormalActivity(), profile.getId());
        aService.update(aRepo.getOne(aRepo.getLastInsertedId()), aRepo.getLastInsertedId(), profile.getId());
        String message = "Ben James Sales edited an activity called Kaikoura Coast Track race.";
        assertEquals(message, nRepo.findAll().get(1).getMessage());
    }
}