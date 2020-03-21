package com.springvuegradle.Model;

public class LoginResponse {
    private Long userId;
    private Long sessionId;

    public LoginResponse(Long sessionId, Long userId){
        this.userId = userId;
        this.sessionId = sessionId;
    }

    public Long getUserId() {
        return userId;
    }

    public Long getSessionId() {
        return sessionId;
    }

}
