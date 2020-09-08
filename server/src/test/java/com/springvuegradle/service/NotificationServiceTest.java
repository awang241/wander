package com.springvuegradle.service;


import com.springvuegradle.enums.NotificationType;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import org.junit.jupiter.api.AfterEach;
import com.springvuegradle.repositories.ActivityMembershipRepository;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.repositories.NotificationRepository;
import com.springvuegradle.repositories.ProfileRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

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

    void saveNotifications(Profile profile, Activity activity) {
        Notification notification1 = new Notification("activity created", activity, profile, NotificationType.ActivityCreated);
        notificationRepository.save(notification1);
        profile.addNotification(notification1);
        Notification notification2 = new Notification("activity has new follower", activity, profile, NotificationType.ActivityFollowerAdded);
        notificationRepository.save(notification2);
        profile.addNotification(notification2);
        Notification notification3 = new Notification("activity has removed a follower", activity, profile, NotificationType.ActivityFollowerRemoved);
        notificationRepository.save(notification3);
        profile.addNotification(notification3);
    }

    @Test
    void getNotificationsSuccessTest() {
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        Activity activity = ActivityTestUtils.createNormalActivity();
        activityRepository.save(activity);
        profileRepository.save(profile);
        saveNotifications(profile, activity);

        int count = 5;
        int startIndex = 0;
        long profileId = profile.getId();
        List<Notification> notificationsList = notificationService.getSortedNotifications(profileId, count, startIndex);
        assertEquals(3, notificationsList.size());
    }

    @Test
    void getNotificationsInvalidProfileErrorTest() {
        int count = 5;
        int startIndex = 0;
        long profileId = 420;
        assertThrows(IllegalArgumentException.class, ()->notificationService.getSortedNotifications(profileId, count, startIndex));
    }

    @Test
    void getNotificationsWithPaginationSuccessCountLimitingTest() {
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        Activity activity = ActivityTestUtils.createNormalActivity();
        activityRepository.save(activity);
        profileRepository.save(profile);
        saveNotifications(profile, activity);

        int count = 2;
        int startIndex = 0;
        long profileId = profile.getId();
        List<Notification> notificationsList = notificationService.getSortedNotifications(profileId, count, startIndex);
        assertEquals(2, notificationsList.size());
    }

    @Test
    void getNotificationsWithPaginationSuccessStartIndexLimitingTest() {
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        Activity activity = ActivityTestUtils.createNormalActivity();
        activityRepository.save(activity);
        profileRepository.save(profile);
        saveNotifications(profile, activity);

        int count = 2;
        int startIndex = 2;
        long profileId = profile.getId();
        List<Notification> notificationsList = notificationService.getSortedNotifications(profileId, count, startIndex);
        assertEquals(1, notificationsList.size());
    }

    /**
     * Helper function to compare two notifications based on their activity, message and notificationType
     * @param notificationOne
     * @param notificationTwo
     * @return true if they're the same, false otherwise
     */
    boolean compareNotification (Notification notificationOne, Notification notificationTwo) {
        return notificationOne.getActivity().equals(notificationTwo.getActivity())
                && notificationOne.getMessage().equals(notificationTwo.getMessage())
                && notificationOne.getNotificationType().equals(notificationTwo.getNotificationType());
    }

    /**
     * Helper function to compare two lists of notifications
     * @param notificationListOne
     * @param notificationListTwo
     * @return true if the two lists are the same, false otherwise
     */
    boolean compareNotificationArray(List<Notification> notificationListOne, List<Notification> notificationListTwo) {
        for (int i = 0; i < Math.min(notificationListOne.size(), notificationListTwo.size()); i++) {
            if (!compareNotification(notificationListOne.get(i), notificationListTwo.get(i))) {
                return false;
            }
        }
        return true;
    }

    @Test
    void getNotificationsSortedByDateTest() {
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        Activity activity = ActivityTestUtils.createNormalActivity();
        activityRepository.save(activity);
        profileRepository.save(profile);

        Notification notification1 = new Notification("activity created", activity, profile, NotificationType.ActivityCreated);
        notificationRepository.save(notification1);
        profile.addNotification(notification1);
        Notification notification2 = new Notification("activity has new follower", activity, profile, NotificationType.ActivityFollowerAdded);
        notificationRepository.save(notification2);
        profile.addNotification(notification2);
        Notification notification3 = new Notification("activity has removed a follower", activity, profile, NotificationType.ActivityFollowerRemoved);
        notificationRepository.save(notification3);
        profile.addNotification(notification3);


        List<Notification> expectedNotificationsList = new ArrayList<>();
        expectedNotificationsList.add(notification3);
        expectedNotificationsList.add(notification2);
        expectedNotificationsList.add(notification1);

        int count = 3;
        int startIndex = 0;
        long profileId = profile.getId();
        List<Notification> notificationsList = notificationService.getSortedNotifications(profileId, count, startIndex);

        assertTrue(compareNotificationArray(notificationsList, expectedNotificationsList));
    }




}
