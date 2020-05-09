package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Entity
public class ActivityType {

    /**
     * Holds the automatically generated activityType id assigned when the object is saved to the database.
     */
    @Id
    @GeneratedValue
    private long id;

    /**
     * Holds the activityType name its referring to.
     */
    @Column
    @NotNull
    private String activityTypeName;

    /**
     * Each activityType object can have multiple profiles with the activityTypes being referred.
     */
    @ManyToMany(mappedBy = "activityTypes")
    @JsonBackReference
    private Set<Profile> profiles = new HashSet<Profile>();

    @ManyToMany(mappedBy = "activityTypes", cascade=CascadeType.ALL)
    @JsonIgnore
    private Set<Activity> activities = new HashSet<Activity>();

    public ActivityType() {
    }

    @JsonCreator
    public ActivityType(
            @JsonProperty("name") String activityTypeName) {
        this.activityTypeName = activityTypeName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ActivityType activityType = (ActivityType) o;
        return activityTypeName.equals(activityType.activityTypeName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(activityTypeName);
    }


    public String getActivityTypeName() {
        return activityTypeName;
    }

    public Set<Activity> getActivities() { return activities; }

    public boolean addActivity(Activity activity) { return activities.add(activity);}

    public boolean removeActivity(Activity activity) { return activities.remove(activity);}
}