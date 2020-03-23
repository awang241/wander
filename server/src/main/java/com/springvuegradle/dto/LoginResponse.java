package com.springvuegradle.dto;

public class LoginResponse {
    private Long userId;
    private Long sessionId;

    /**
     * Constructor for a loginresponse with parameters. for JSON parsing with spring requestmapping methods.
     * @param sessionId the session ID of their created login session
     * @param userId the ID of the user that the login details corresponded to
     */
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
