package com.springvuegradle.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ActivityRoleCountResponse {
    private long organisers;
    private long participants;
    private long followers;

}
