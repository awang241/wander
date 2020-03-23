package com.springvuegradle.dto;

public class LogoutRequest {
    private Long userId;


    public LogoutRequest(Long userId){
        this.userId = userId;
    }

    public Long getUserId() {
        return userId;
    }
    public void setUserId(Long userId) {this.userId = userId;}

}