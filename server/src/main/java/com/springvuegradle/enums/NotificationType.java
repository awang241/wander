package com.springvuegradle.enums;

public enum NotificationType {
    ActivityCreated(0), ActivityRemoved(1), ActivityEdited(2), ActivityFollowerAdded(3),
    ActivityFollowerRemoved(4), ActivityOrganiserAdded(5), ActivityOrganiserRemoved(6),
    ActivityParticipantAdded(7), ActivityParticipantRemoved(8), ParticipantCreated(9),
    ParticipationEdited(10), ActivityPrivacyChanged(11);
    private int num;
    NotificationType(int num) {
        this.num = num;
    }
    public int getNotificationNum() {
        return num;
    }
}
