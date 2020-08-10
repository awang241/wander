package com.springvuegradle.dto.responses;

import com.springvuegradle.model.ActivityMembership;
import lombok.AllArgsConstructor;
import lombok.Data;

@AllArgsConstructor
@Data
public class ActivityMemberProfileResponse {
    private long id;
    private String firstName;
    private String lastName;
    private String email;
    private ActivityMembership.Role role;
}
