package com.springvuegradle.enums;

public enum AuthLevel {
    DEFAULT_ADMIN(0), ADMIN(1), USER(5);
    private int level;
    AuthLevel(int level) {
        this.level = level;
    }

    public int getLevel() {
        return level;
    }
}

