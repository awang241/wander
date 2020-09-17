package com.springvuegradle.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.springvuegradle.utilities.FormatHelper;

import javax.persistence.*;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.time.OffsetDateTime;
import java.util.*;

@Entity
public class Activity {
    /**
     * Holds the automatically generated activity id assigned when the object is saved to the database.
     */
    @Id
    @GeneratedValue
    private long id;

    /**
     * Holds the activity name its referring to.
     */
    @Column
    @NotNull
    private String activityName;

    @NotNull
    private String description;

    @NotNull
    private Boolean continuous;

    private OffsetDateTime startTime;

    private OffsetDateTime endTime;

    @NotNull
    private String location;

    private Double latitude;

    private Double longitude;


    /**
     * Holds the privacy level of the activity.
     * level 0 - private - only the creator can view the activity.
     * level 1 - share with members - only the members it is shared with can view the activity.
     * level 2 - public - anyone can view the activity.
     */
    @NotNull @Column(name = "privacyLevel") @Min(value = 0) @Max(value = 2)
    private Integer privacyLevel = 0;



    /**
     * Each activity object can have multiple activities.
     */
    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
    @JoinTable(name = "activity_activity_type",
            inverseJoinColumns = @JoinColumn(name = "activity_type_id", referencedColumnName = "id"),
            joinColumns = @JoinColumn(name = "activity_id", referencedColumnName = "id"))
    private Set<ActivityType> activityTypes;


    @OneToMany(fetch = FetchType.LAZY, mappedBy = "activity")
    private Set<ActivityMembership> members;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "activity")
    private Set<ActivityParticipation> activityParticipations = new HashSet<>();

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "activity")
    private Set<Notification> notifications = new HashSet<>();

    public Activity(){}

    @JsonCreator
    public Activity(
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

    public void update(Activity activity) {
        this.activityName = activity.activityName;
        this.description = activity.description;
        this.continuous = activity.continuous;
        this.startTime = activity.startTime;
        this.endTime = activity.endTime;
        this.location = activity.location;
        this.latitude = activity.latitude;
        this.longitude = activity.longitude;
        Set<ActivityType> removals = new HashSet<>(this.activityTypes);
        removals.removeAll(activity.activityTypes);
        for (ActivityType removal: removals) {
            removal.removeActivity(activity);
        }

        activity.activityTypes.removeAll(this.activityTypes);
        for (ActivityType addition: activity.activityTypes) {
            addition.addActivity(activity);
        }

        this.activityTypes.removeAll(removals);
        this.activityTypes.addAll(activity.activityTypes);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Activity activity = (Activity) o;
        return activityName.equals(activity.activityName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(activityName);
    }

    public long getId() {
        return id;
    }

    public String getActivityName() {
        return activityName;
    }

    public void setActivityName(String activityName) {
        this.activityName = activityName;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Boolean getContinuous() {
        return continuous;
    }

    public void setContinuous(Boolean continuous) {
        this.continuous = continuous;
    }

    public OffsetDateTime getStartTime() {
        return startTime;
    }

    public void setStartTime(OffsetDateTime startTime) {
        this.startTime = startTime;
    }

    public OffsetDateTime getEndTime() {
        return endTime;
    }

    public void setEndTime(OffsetDateTime endTime) {
        this.endTime = endTime;
    }

    public String getLocation() {
        return location;
    }


    public Double getLongitude(){
        return longitude;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public void setLatitude(double latitude) { this.latitude = latitude; }


    public void setLongitude(double longitude) { this.longitude = longitude; }

    @JsonIgnore
    public Set<ActivityMembership> getMembers() {
        return this.members;
    }

    public List<String> getActivityTypes() {
        List<String> result = new ArrayList<>();
        for (ActivityType activityType: activityTypes) {
            result.add(activityType.getActivityTypeName());
        }
        return result;
    }

    @JsonIgnore
    public Set<ActivityType> retrieveActivityTypes() {
        return Collections.unmodifiableSet(activityTypes);
    }

    public boolean addActivityType(ActivityType activityType) {
        return this.activityTypes.add(activityType);
    }

    public boolean removeActivityType(ActivityType type) {
        return this.activityTypes.remove(type);
    }

    public boolean addMember(ActivityMembership membership) {
        return this.members.add(membership);
    }

    public boolean removeMember(ActivityMembership membership) {
        return this.members.remove(membership);
    }

    public void setActivityTypes(Set<ActivityType> updatedActivityType) {
        this.activityTypes = updatedActivityType;
    }

    public Integer getPrivacyLevel() { return privacyLevel; }

    public void setPrivacyLevel(Integer privacyLevel) { this.privacyLevel = privacyLevel; }

    public Set<ActivityParticipation> getParticipations() {
        return Collections.unmodifiableSet(activityParticipations);
    }

    public boolean addParticipation(ActivityParticipation participation) {
        return activityParticipations.add(participation);
    }

    public boolean removeParticipation(ActivityParticipation participation) {
        return activityParticipations.remove(participation);
    }

    /**
     * Finds and returns the creator of the profile from the members of the activity. If there is no creator,
     * for whatever reason, null is returned.
     * @return the profile of the creator of the activity.
     */
    @JsonIgnore
    public Profile retrieveCreator() {
        for (ActivityMembership am: members) {
            if (am.getRole().equals(ActivityMembership.Role.CREATOR)) {
                return am.getProfile();
            }
        }
        return null;
    }

    /**
     * Calls retrieveCreator to get the profile of the creator and returns the full name of the creator. Need the string
     * to be returned in the json data that is send with each Activity object.
     * @return creator name if creator exists, null otherwise.
     */
    public String getCreator() {
        Profile creator = retrieveCreator();
        if (creator != null) {
            return creator.getFullName();
        }
        return null;
    }

    /**
     * Calls retrieveCreator to get the profile of the creator and returns the id of the creator. Need the long to be
     * returned in the json data that is sent with each activity object.
     * @return creator id if the creator exists, null otherwise.
     */
    public Long getCreatorId() {
        Profile creator = retrieveCreator();
        if (creator != null) {
            return creator.getId();
        }
        return null;
    }

    @JsonIgnore
    public Set<Notification> getNotifications() {
        return Collections.unmodifiableSet(notifications);
    }

    public void setNotifications(Set<Notification> notifications) {
        this.notifications = notifications;
    }

    public void addNotification(Notification notification) {
        this.notifications.add(notification);
    }

    public void removeNotification(Notification notification) {
        this.notifications.remove(notification);
    }

    public double getLatitude() {
        return latitude;
    }
}
