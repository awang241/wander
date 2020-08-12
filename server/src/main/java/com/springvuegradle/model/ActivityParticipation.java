package com.springvuegradle.model;

import javax.persistence.*;
import com.fasterxml.jackson.annotation.JsonBackReference;
import com.springvuegradle.utilities.FormatHelper;

import javax.validation.constraints.NotNull;
import java.time.OffsetDateTime;
@Entity
public class ActivityParticipation {

    /**
     * Holds the automatically generated activity participation id assigned when the object is saved to the database.
     */
    @Id
    @GeneratedValue
    private long id;

    /**
     * Each activity participation object is associated with one and only one profile object.
     */
    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "profile_id")
    @JsonBackReference
    private Profile profile;

    /**
     * Each activity participation object is associated with one and only one activity object.
     */
    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "activity_id")
    @JsonBackReference
    private Activity activity;

    private String details;

    @NotNull
    private String outcome;

    private OffsetDateTime startTime;

    private OffsetDateTime endTime;

    public ActivityParticipation() {};

    public ActivityParticipation(String details,
                                 String outcome,
                                 OffsetDateTime startTime,
                                 OffsetDateTime endTime,
                                 Profile profile,
                                 Activity activity){

        this.details = details;
        this.outcome = outcome;
        this.startTime = startTime;
        this.endTime = endTime;
        this.profile = profile;
        this.activity = activity;
    }

    public ActivityParticipation(String details,
                                 String outcome,
                                 String startTime,
                                 String endTime){

        this.details = details;
        this.outcome = outcome;
        this.startTime = FormatHelper.parseOffsetDateTime(startTime);
        this.endTime = FormatHelper.parseOffsetDateTime(endTime);;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getDetails() {
        return details;
    }

    public void setDetails(String details) {
        this.details = details;
    }

    public String getOutcome() {
        return outcome;
    }

    public void setOutcome(String outcome) {
        this.outcome = outcome;
    }
}
