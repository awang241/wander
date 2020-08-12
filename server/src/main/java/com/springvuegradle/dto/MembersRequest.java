package com.springvuegradle.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class MembersRequest {
    String email;
    String role;
}
