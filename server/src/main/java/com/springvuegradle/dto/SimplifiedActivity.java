package com.springvuegradle.dto;

import com.springvuegradle.model.Activity;

import java.util.List;
import java.util.Objects;

public class SimplifiedActivity {

    /**
     * contains the activity id
     */
    private Long id;

    /**
     * contains the name of the activity
     */
    private String activityName;

    /**
     * contains the creator of the activity
     */
    private String creatorName;

    /**
     * contains a boolean determining whether the activity is continuous or duration based
     */
    private boolean continuous;

    /**
     * contains the location of the activity
     */
    private String location;

    /**
     * contains all the activity types of the activity as strings
     */
    private List<String> activityTypes;

    /**
     * Creates a SimplifiedActivity based on the given Activity.
     * @param activity The activity being simplified.
     */
    public SimplifiedActivity(Activity activity, String creatorName) {
        this.id = activity.getId();
        this.activityName = activity.getActivityName();
        this.creatorName = creatorName;
        this.continuous = activity.getContinuous();
        this.location = activity.getLocation();
        this.activityTypes = activity.getActivityTypes();
    }

    /* ----------------------------------------- Getters and Setters ------------------------------------------------ */

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getActivityName() {
        return activityName;
    }

    public void setActivityName(String activityName) {
        this.activityName = activityName;
    }

    public String getCreatorName() {
        return creatorName;
    }

    public void setCreatorName(String creator) {
        this.creatorName = creator;
    }

    public boolean getContinuous() {
        return continuous;
    }

    public void setContinuous(Boolean continuous) {
        this.continuous = continuous;
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public List<String> getActivityTypes() {
        return activityTypes;
    }

    public void setActivityTypes(List<String> activityTypes) {
        this.activityTypes = activityTypes;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SimplifiedActivity that = (SimplifiedActivity) o;
        boolean returnVal =  Objects.equals(id, that.id) &&
                Objects.equals(activityName, that.activityName) &&
                Objects.equals(creatorName, that.creatorName) &&
                Objects.equals(continuous, that.continuous) &&
                Objects.equals(location, that.location);
        if (Objects.equals(activityTypes.size(), that.activityTypes.size())) {
            for (int i=0; i < activityTypes.size(); i++) {
                returnVal &= Objects.equals(activityTypes.get(i), that.activityTypes.get(i));
            }
        } else {
            returnVal = false;
        }
        return returnVal;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, activityName, creatorName, continuous, location);
    }
}
