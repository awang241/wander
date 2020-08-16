package com.springvuegradle.dto.requests;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@Data
@NoArgsConstructor
public class ActivityRoleUpdateRequest {
    private String role;

    public String getRole() {
        return this.role;
    }

    public void setRole(String newRole){
        this.role = newRole;
    }
}
