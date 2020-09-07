package com.springvuegradle.service;


import com.springvuegradle.enums.NotificationType;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
public class NotificationServiceTest {

    @Autowired
    private ActivityMembershipRepository activityMembershipRepository;
    @Autowired
    private ActivityRepository activityRepository;
    @Autowired
    private ProfileRepository profileRepository;

    @Autowired
    private NotificationRepository notificationRepository;

    @Autowired
    private EmailRepository emailRepository;

    @Autowired
    private ActivityService activityService;

    @Autowired
    private NotificationService notificationService;

    @AfterEach
    void tearDown() {
        emailRepository.deleteAll();
        activityMembershipRepository.deleteAll();
        notificationRepository.deleteAll();
        profileRepository.deleteAll();
        activityRepository.deleteAll();
    }

    @Test
    /***
     * Tests to ensure creating a notification works successfully
     */
    void createsNotificationWhenCreatingActivity() {
        Profile profileOne = ProfileTestUtils.createProfileJimmyAlternate();
        Profile profileTwo = ProfileTestUtils.createProfileWithMinimalFields();
        Activity activityOne = ActivityTestUtils.createNormalActivity();
        activityRepository.save(activityOne);
        profileRepository.saveAll(List.of(profileOne, profileTwo));
        activityOne.addMember(new ActivityMembership(activityOne, profileOne, ActivityMembership.Role.ORGANISER));
        activityOne.addMember(new ActivityMembership(activityOne, profileTwo, ActivityMembership.Role.FOLLOWER));
        String message = "Activity edited";
        notificationService.createNotification(NotificationType.ActivityEdited, activityOne, profileOne, message);
        Notification expectedNotification = new Notification(message, activityOne, profileOne, NotificationType.ActivityEdited);
        expectedNotification.addRecipient(profileOne);
        expectedNotification.addRecipient(profileTwo);
        Notification actualNotification = notificationRepository.findAll().get(0);

        //Would rather call one assert equals statement for the entire object but can't because the ID's are different
        assertTrue(areNotificationsEqualExcludingId(expectedNotification, actualNotification));
    }

    @Test
    void createsNotificationWhenEditingActivityPrivacy(){
        Profile profileOne = ProfileTestUtils.createProfileJimmyAlternate();
        Profile profileTwo = ProfileTestUtils.createProfileWithMinimalFields();
        Activity activityOne = ActivityTestUtils.createNormalActivity();
        activityRepository.save(activityOne);
        profileRepository.saveAll(List.of(profileOne, profileTwo));
        ActivityMembership creatorMembership = new ActivityMembership(activityOne, profileOne, ActivityMembership.Role.CREATOR);
        ActivityMembership followerMembership = new ActivityMembership(activityOne, profileTwo, ActivityMembership.Role.FOLLOWER);
        activityMembershipRepository.saveAll(List.of(creatorMembership, followerMembership));
        activityOne.addMember(followerMembership);
        activityOne.addMember(creatorMembership);
        activityService.editActivityPrivacy("public", activityOne.getId(), profileOne.getId());
        Notification expectedNotification = new Notification("Activity Kaikoura Coast Track race's privacy level has been changed to public", activityOne, profileOne, NotificationType.ActivityPrivacyChanged);
        expectedNotification.addRecipient(profileOne);
        expectedNotification.addRecipient(profileTwo);
        Notification actualNotification = notificationRepository.findAll().get(0);
        assertTrue(areNotificationsEqualExcludingId(expectedNotification, actualNotification));
    }

    boolean areNotificationsEqualExcludingId(Notification notificationOne, Notification notificationTwo){
        return notificationOne.getRecipients().equals(notificationTwo.getRecipients()) &&
                notificationOne.getMessage().equals(notificationTwo.getMessage()) &&
                notificationOne.getNotificationType().equals(notificationTwo.getNotificationType()) &&
                notificationOne.getActivity().equals(notificationTwo.getActivity()) &&
                notificationOne.getEditorId() == (notificationTwo.getEditorId());

    }
}
