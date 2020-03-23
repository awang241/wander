package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

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

    /**
     * Each activity object can have multiple profiles with the activities being referred.
     */
    @ManyToMany(mappedBy = "activities")
    @JsonBackReference
    private Set<Profile> profiles = new HashSet<Profile>();

    public Activity(){};

    @JsonCreator
    public Activity(
            @JsonProperty("name") String countryName) {
        this.activityName = countryName;
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


    public String getActivityName() {
        return activityName;
    }
}
