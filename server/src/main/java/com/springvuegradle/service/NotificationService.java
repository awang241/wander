package com.springvuegradle.service;

import com.springvuegradle.enums.NotificationType;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.Notification;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Service-layer class containing business logic handling notifications.
 */
@Service(value = "notificationService")
public class NotificationService {

    private ProfileRepository profileRepo;
    private ActivityRepository activityRepo;
    private NotificationRepository notificationRepo;

    /**
     * Autowired constructor for Spring to create an ActivityService and inject the correct dependencies.
     *
     * @param profileRepo                  the profile repository being injected.
     * @param activityRepo                 the activity repository being injected.
     * @param notificationRepo             the notification repository being injected.
     */
    @Autowired
    public NotificationService(ProfileRepository profileRepo, ActivityRepository activityRepo, NotificationRepository notificationRepo) {
        this.profileRepo = profileRepo;
        this.activityRepo = activityRepo;
        this.notificationRepo = notificationRepo;
    }

    /**
     * Create a notification with the given message, share it with the activity's current profiles.
     *
     * @param spawner
     * @param activity
     * @param message
     */
    public void create(Profile spawner, Activity activity, String message) {
        Notification notification = new Notification(message, activity, spawner, NotificationType.ActivityCreated);
        for (ActivityMembership member: activity.getMembers()) {
            notification.addRecipient(member.getProfile());
        }
        notificationRepo.save(notification);
        spawner.addNotification(notification);
        activity.addNotification(notification);
        profileRepo.save(spawner);

    }
}
