package com.springvuegradle.Model;

public class LogoutRequest {
    private Long userId;

    public LogoutRequest(){

    }

    public LogoutRequest(Long userId){
        this.userId = userId;
    }

    public Long getUserId() {
        return userId;
    }
    public void setUserId(Long userId) {this.userId = userId;}

}