package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.springvuegradle.Utilities.FormatHelper;
import org.hibernate.type.CalendarTimeType;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
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

    @NotNull
    private OffsetDateTime startTime;

    @NotNull
    private OffsetDateTime endTime;

    private String location;


    /**
     * Each activity object can have multiple activities.
     */
    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = "activity_activity_type",
            inverseJoinColumns = @JoinColumn(name = "activity_type_id", referencedColumnName = "id"),
            joinColumns = @JoinColumn(name = "activity_id", referencedColumnName = "id"))
    private Set<ActivityType> activityTypes;

    @OneToMany(fetch = FetchType.EAGER, mappedBy = "activity")
    private Set<ActivityMembership> members;

    public Activity(){}

    @JsonCreator
    public Activity(
            @JsonProperty("activity_name") String activityName,
            @JsonProperty("description") String description,
            @JsonProperty("activity_type") String[] activityTypes,
            @JsonProperty("continuous") Boolean continuous,
            @JsonProperty("start_time") String startTime,
            @JsonProperty("end_time") String endTime,
            @JsonProperty("location") String location){
        this.activityName = activityName;
        this.description = description;
        this.activityTypes = new HashSet<>();
        for (String activityType: activityTypes) {
            addActivityType(new ActivityType(activityType));
        }
        this.continuous = continuous;
        this.startTime = FormatHelper.parseOffsetDateTime(startTime);
        this.endTime = FormatHelper.parseOffsetDateTime(endTime);
        this.location = location;
        this.members = new HashSet<>();
    }

    public void update(Activity activity) {
        this.activityName = activity.activityName;
        this.description = activity.description;
        this.continuous = activity.continuous;
        this.startTime = activity.startTime;
        this.endTime = activity.endTime;
        this.location = activity.location;
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

    public void setLocation(String location) {
        this.location = location;
    }

    public Set<ActivityType> getActivityTypes() {
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
}
