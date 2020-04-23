package com.springvuegradle.dto;

public class LoginResponse {
    private Long userId;
    private String token;

    /**
     * Constructor for a loginresponse with parameters. for JSON parsing with spring requestmapping methods.
     * @param token the JWT that the user receives
     * @param userId the ID of the user that the login details corresponded to
     */
    public LoginResponse(String token, Long userId){
        this.userId = userId;
        this.token = token;
    }

    public Long getUserId() {
        return userId;
    }

    public String getToken() {
        return token;
    }

}
