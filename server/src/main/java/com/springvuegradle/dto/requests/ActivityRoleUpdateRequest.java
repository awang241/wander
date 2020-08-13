package com.springvuegradle.dto.requests;

import lombok.Data;

@Data
public class ActivityRoleUpdateRequest {
    private String role;

    public String getRole() {
        return this.role;
    }

    public void setRole(String newRole){
        this.role = newRole;
    }
}
