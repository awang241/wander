package com.springvuegradle.dto;

public class ChangePasswordRequest {

    private Long userId;
    private String currentPassword;
    private String newPassword;
    private String confPassword;

    /**
     * Constructor for a changepasswordrequest with parameters. for JSON parsing with spring requestmapping methods.
     * @param userId ID of the user changing password
     * @param currentPassword Existing password for verification
     * @param newPassword New password they want to change to
     * @param confPassword another field of their new password used for match checking
     */
    public ChangePasswordRequest(Long userId, String currentPassword, String newPassword, String confPassword) {
        this.userId = userId;
        this.currentPassword = currentPassword;
        this.newPassword = newPassword;
        this.confPassword = confPassword;
    }

    // getters and setters

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getCurrentPassword() {
        return currentPassword;
    }

    public void setCurrentPassword(String currentPassword) {
        this.currentPassword = currentPassword;
    }

    public String getNewPassword() {
        return newPassword;
    }

    public void setNewPassword(String newPassword) {
        this.newPassword = newPassword;
    }

    public String getConfPassword() {
        return confPassword;
    }

    public void setConfPassword(String confPassword) {
        this.confPassword = confPassword;
    }
}
