package com.springvuegradle.service;

import com.springvuegradle.enums.ActivityMessage;
import com.springvuegradle.enums.NotificationType;
import com.springvuegradle.enums.ProfileErrorMessage;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.Notification;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Service-layer class containing business logic handling notifications.
 */
@Service(value = "notificationService")
public class NotificationService {

    private NotificationRepository notificationRepo;
    private ProfileRepository profileRepository;

    /**
     * Autowired constructor for Spring to create an ActivityService and inject the correct dependencies.
     * @param notificationRepo             the notification repository being injected.
     */
    @Autowired
    public NotificationService(NotificationRepository notificationRepo,
                               ProfileRepository profileRepo) {
        this.notificationRepo = notificationRepo;
        this.profileRepository = profileRepo;
    }

    /** Inserts the given Notification into the database
     * @param notificationType the type of notification
     * @param activity the activity the notification belongs to
     * @param notificationCreator the profile the notification is for
     * @param message string message the contains the content of the notification
     */
    public void createNotification(NotificationType notificationType, Activity activity, Profile notificationCreator, String message){
        Notification notification = new Notification(message, activity, notificationCreator, notificationType);
        for (ActivityMembership membership: activity.getMembers()) {
            notification.addRecipient(membership.getProfile());
        }
        activity.addNotification(notification);
        notificationCreator.addNotification(notification);
        notificationRepo.save(notification);
    }

    /**
     * Removes the activity from all the notifications connected to the given activity, required in order to delete the
     * activity.
     *
     * @param activity contains a reference to all the notifications it is connected to.
     */
    public void detachActivityFromNotifications(Activity activity) {
        for (Notification notification: activity.getNotifications()) {
            if (notification.getActivityId().equals(activity.getId())) {
                notification.setActivity(null);
                notificationRepo.save(notification);
            }
        }
    }

    /**
     * Gets all notifications related to the user
     * @param profileId the users ID which is used to get the notifications
     * @return a list of notifications sorted by time (latest first, earliest last)
     */
    public List<Notification> getSortedNotifications(Long profileId, int count, int startIndex) {
        Optional<Profile> result = profileRepository.findById(profileId);
        if (result.isPresent()) {
            Profile targetProfile = result.get();
            Set<Notification> notificationSet = targetProfile.getNotifications();
            List<Notification> notificationList = new ArrayList<>(notificationSet);
            sortNotifications(notificationList);
            return notificationList.subList(Math.min(notificationList.size(), startIndex), Math.min(notificationList.size(), count + startIndex));
        }
        throw new IllegalArgumentException(ProfileErrorMessage.PROFILE_NOT_FOUND.getMessage());
    }

    /**
     * Customer sorter which sorts a list of notifications by date (latest first, earliest last)
     * @param notificationsList the list of notifications to be sorted
     */
    private void sortNotifications(List<Notification> notificationsList) {
        Collections.sort(notificationsList, (o1, o2) -> o2.getTimeStamp().compareTo(o1.getTimeStamp()));
    }

    /**
     * Returns the notification type generated when an activity membership is created with the given role.
     * @param role The given role as an enum.
     * @return the notification type generated when an activity membership is created with the given role.
     */
    public static NotificationType getTypeForAddingRole(ActivityMembership.Role role) {
        NotificationType notificationType;
        switch (role) {
            case FOLLOWER:
                notificationType = NotificationType.ACTIVITY_FOLLOWER_ADDED;
                break;
            case ORGANISER:
                notificationType = NotificationType.ACTIVITY_ORGANISER_ADDED;
                break;
            case PARTICIPANT:
                notificationType = NotificationType.ACTIVITY_PARTICIPANT_ADDED;
                break;
            case CREATOR:
                notificationType = NotificationType.ACTIVITY_CREATOR_ADDED;
                break;
            default:
                throw new IllegalArgumentException(ActivityMessage.INVALID_ROLE.getMessage());
        }
        return notificationType;
    }

    /**
     * Returns the notification type generated when an activity membership is created with the given role.
     * @param roleName The name of the given role.
     * @return the notification type generated when an activity membership is created with the given role.
     */
    public static NotificationType getTypeForAddingRole(String roleName) {
        ActivityMembership.Role role = ActivityMembership.Role.valueOf(roleName.toUpperCase());
        return getTypeForAddingRole(role);
    }
}
