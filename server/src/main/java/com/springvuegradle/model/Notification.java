package com.springvuegradle.model;

import com.fasterxml.jackson.annotation.JsonBackReference;
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
public class Notification {
    /**
     * Holds automatically generated notification id that is assigned when the
     * object is saved to the database.
     */
    @Id
    @GeneratedValue
    private int id;

    /**
     * Holds the notification message as a string.
     */
    @Column
    @NotNull
    private String message;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "activity_id")
    @JsonBackReference(value = "profile")
    private Activity activity;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "profile_id")
    @JsonBackReference(value = "activity")
    private Profile editor;

    @Column
    private OffsetDateTime timeStamp;

    @Column
    @NotNull
    private String activityType;



}
