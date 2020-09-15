package com.springvuegradle.dto.requests;

import lombok.Data;

@Data
public class ActivityRangeRequest {
    Double latitude;

    Double longitude;

    //Used to check if activities are within this distance (m) of the location
    int distance;
}
