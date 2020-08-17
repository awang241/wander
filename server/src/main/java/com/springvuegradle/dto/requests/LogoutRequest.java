package com.springvuegradle.dto.requests;

public class LogoutRequest {
    private Long userId;


    /**
     * Constructor for a logoutrequest with parameters. for JSON parsing with spring requestmapping methods.
     * @param userId the ID of the user who's trying to log out
     */
    public LogoutRequest(Long userId){
        this.userId = userId;
    }

    public Long getUserId() {
        return userId;
    }
    public void setUserId(Long userId) {this.userId = userId;}

}