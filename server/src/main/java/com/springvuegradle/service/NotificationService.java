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

    private NotificationRepository notificationRepo;

    /**
     * Autowired constructor for Spring to create an ActivityService and inject the correct dependencies.
     * @param notificationRepo             the notification repository being injected.
     */
    @Autowired
    public NotificationService(NotificationRepository notificationRepo) {
        this.notificationRepo = notificationRepo;
    }

    /** Inserts the given Notification into the database
     * @param notificationType the type of notification
     * @param activity the activity the notification belongs to
     * @param notificationCreator the profile the notification is for
     * @param message string message the contains the content of the notification
     */
    public void createNotification(NotificationType notificationType, Activity activity, Profile notificationCreator, String message){
        Notification notification = new Notification(message, activity, notificationCreator, notificationType);
        for(ActivityMembership membership: activity.getMembers()){
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
}
