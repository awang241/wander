package com.springvuegradle.service;

import com.springvuegradle.enums.NotificationType;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.Notification;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.*;
import org.aspectj.weaver.ast.Not;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
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
        for(ActivityMembership membership: activity.getMembers()){
            notification.addRecipient(membership.getProfile());
        }
        notificationRepo.save(notification);
    }

    /**
     * Gets all notifications related to the user
     * @param profileId the users ID which is used to get the notifications
     * @return a list of notifications sorted by time (latest first, earliest last)
     */
    public List<Notification> getSortedNotifications(Long profileId, PageRequest request) {
        Optional<Profile> result = profileRepository.findById(profileId);
        if (result.isPresent()) {
            Profile targetProfile = result.get();
            Set<Notification> notificationSet = targetProfile.getNotifications();
            List<Notification> notificationList = new ArrayList<>(notificationSet);
            sortNotifications(notificationList);
            return notificationList;
        }
        return null;
//        Profile targetProfile = result.get();
//        Set<Notification> notificationSet = targetProfile.getNotifications();
//        List<Notification> notificationList = new ArrayList<>(notificationSet);
//        sortNotifications(notificationList);
//        return notificationList;
    }

    /**
     * Customer sorter that sorts a list of notifications by date (latest first, earliest last)
     * @param notificationsList the list of notifications to be sorted
     */
    private void sortNotifications(List<Notification> notificationsList) {
        Collections.sort(notificationsList, new Comparator<Notification>() {
            @Override
            public int compare(Notification o1, Notification o2) {
                return 0;
            }
        });
    }
}
