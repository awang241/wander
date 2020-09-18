package com.springvuegradle.dto;

public class MembersRequest {
    String email;
    String role;

    public MembersRequest (String email, String role){
        this.email = email;
        this.role = role;
    }

    public String getRole() {
        return this.role;
    }

    public String getEmail() {
        return this.email;
    }

    public void setRole(String newRole){
        this.role = newRole;
    }

    public void setEmail(String newEmail){
        this.email = newEmail;
    }

}
