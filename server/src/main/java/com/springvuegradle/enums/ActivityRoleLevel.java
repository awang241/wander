package com.springvuegradle.enums;

public enum ActivityRoleLevel {
    CREATOR(0), ORGANISER(1), PARTICIPANT(2), FOLLOWER(3);
    private int level;
    ActivityRoleLevel (int level) {
        this.level = level;
    }
    public int getActivityRoleLevel() {
        return level;
    }
}
