package com.springvuegradle.dto;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.ActivityParticipation;
import com.springvuegradle.model.ActivityType;
import com.springvuegradle.model.Notification;
import com.springvuegradle.utilities.FormatHelper;

import java.time.OffsetDateTime;
import java.util.HashSet;
import java.util.Set;

/**
 * Attempted to create this as an interim layer for spring endpoints that automatically create activity objects since having
 * them automatically create database-tracked @Entity classes is a security vulnerability, but ran into too many issues with
 * tests null-pointering etc since it seemed to interfere with it properly recognising things as the same activity with the same
 * ID. leaving it here for now though in case someone else wants to try get it working later.
 */
public class ActivityPOJO {

    public String activityName;
    public String description;
    public Boolean continuous;
    public OffsetDateTime startTime;
    public OffsetDateTime endTime;
    public String location;
    public Double latitude;
    public Double longitude;
    public Integer privacyLevel = 0;
    public Set<ActivityType> activityTypes;
    public Set<ActivityMembership> members;
    public Set<ActivityParticipation> activityParticipations = new HashSet<>();
    public Set<Notification> notifications = new HashSet<>();

    @JsonCreator
    public ActivityPOJO(
            @JsonProperty("activity_name") String activityName,
            @JsonProperty("description") String description,
            @JsonProperty("activity_type") String[] activityTypes,
            @JsonProperty("continuous") Boolean continuous,
            @JsonProperty("start_time") String startTime,
            @JsonProperty("end_time") String endTime,
            @JsonProperty("location") String location,
            @JsonProperty("latitude") double latitude,
            @JsonProperty("longitude") double longitude){
        this.activityName = activityName;
        this.description = description;
        this.activityTypes = new HashSet<>();
        if (activityTypes == null) {
            this.activityTypes = null;
        } else {
            this.activityTypes = new HashSet<>();
            for (String activityType: activityTypes) {
                addActivityType(new ActivityType(activityType));
            }
        }
        this.continuous = continuous;
        if (Boolean.TRUE.equals(this.continuous)) {
            this.startTime = null;
            this.endTime = null;
        }
        else {
            this.startTime = FormatHelper.parseOffsetDateTime(startTime);
            this.endTime = FormatHelper.parseOffsetDateTime(endTime);
        }
        this.location = location;
        this.latitude = latitude;
        this.longitude = longitude;
        this.members = new HashSet<>();
    }

    public boolean addActivityType(ActivityType activityType) {
        return this.activityTypes.add(activityType);
    }
}
