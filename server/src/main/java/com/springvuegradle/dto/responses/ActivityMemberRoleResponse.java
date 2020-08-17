package com.springvuegradle.dto.responses;

import lombok.Data;

import java.util.List;

@Data
public class ActivityMemberRoleResponse {
    private List<ProfileSummary> summaries;
    private String message;


    public ActivityMemberRoleResponse(List<ProfileSummary> summaries) {
        this.summaries = summaries;
        this.message = null;
    }

    public ActivityMemberRoleResponse(String message) {
        this.message = message;
        this.summaries = null;
    }

    public <E extends Enum<E>> ActivityMemberRoleResponse(E messageEnum) {
        this.message = messageEnum.toString();
        this.summaries = null;
    }
}
