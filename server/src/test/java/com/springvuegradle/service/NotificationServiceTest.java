package com.springvuegradle.service;


import com.springvuegradle.enums.NotificationType;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.ActivityMembershipRepository;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.repositories.NotificationRepository;
import com.springvuegradle.repositories.ProfileRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
    private NotificationService notificationService;


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
        assertEquals(actualNotification.getMessage(), expectedNotification.getMessage());
        assertEquals(actualNotification.getRecipients(), expectedNotification.getRecipients());
        assertEquals(actualNotification.getNotificationType(), expectedNotification.getNotificationType());
        assertEquals(actualNotification.getActivityId(), expectedNotification.getActivityId());
        assertEquals(actualNotification.getEditorId(), expectedNotification.getEditorId());
    }
}
