package com.springvuegradle.enums;

public enum NotificationType {
    ACTIVITY_CREATED(0), ACTIVITY_REMOVED(1), ACTIVITY_EDITED(2), ACTIVITY_FOLLOWER_ADDED(3),
    NOTIFICATION_TYPE(4), ACTIVITY_ORGANISER_ADDED(5), ACTIVITY_ORGANISER_REMOVED(6),
    ACTIVITY_PARTICIPANT_ADDED(7), ACTIVITY_PARTICIPANT_REMOVED(8), PARTICIPANT_CREATED(9),
    PARTICIPATION_EDITED(10), ACTIVITY_PRIVACY_CHANGED(11), ACTIVITY_CREATOR_ADDED(12);
    private int num;
    NotificationType(int num) {
        this.num = num;
    }
    public int getNotificationNum() {
        return num;
    }
}
