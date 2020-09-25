package com.springvuegradle.dto.responses;

import com.springvuegradle.model.Activity;
import lombok.Data;

import java.time.OffsetDateTime;
import java.util.List;

@Data
public class ActivityLocationResponse {

    //The ID of the activity
    private Long id;

    //The name of the activity
    private String activityName;

    //true if the the activity is continuous. false if the activity is duration based
    private boolean continuous;

    //The start time of the activity or null if no start time
    private OffsetDateTime startTime;

    //The end time of the activity or null if there is no end time
    private OffsetDateTime endTime;

    //The location of the activity as a textual description
    private String location;

    //The latitude of the activity
    private Double latitude;

    //The longitude of the activity
    private Double longitude;


    //contains all the activity types of the activity as strings
    private List<String> activityTypes;

    public ActivityLocationResponse(Activity activity) {
        this.id = activity.getId();
        this.activityName = activity.getActivityName();
        this.continuous = activity.getContinuous();
        this.location = activity.getLocation();
        this.latitude = activity.getLatitude();
        this.longitude = activity.getLongitude();
        this.activityTypes = activity.getActivityTypes();
        this.startTime = activity.getStartTime();
        this.endTime = activity.getEndTime();
    }
}

