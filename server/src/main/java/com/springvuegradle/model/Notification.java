package com.springvuegradle.model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.springvuegradle.enums.NotificationType;

import javax.persistence.*;
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
    private Long id;

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
    private Profile profile;

    @Column
    private OffsetDateTime timeStamp = OffsetDateTime.now();

    @Column
    @NotNull
    private NotificationType notificationType;

    public Notification(
            String message, Activity activity, Profile profile, NotificationType notificationType)
    {
        this.message = message;
        this.activity = activity;
        this.profile = profile;
        this.notificationType = notificationType;
    };

    @Override
    public int hashCode() {
        return Objects.hash(message, notificationType);
    }

    public String getMessage() { return message; }

    public void setMessage(String message) { this.message = message; }

    public long getActivityId() { return activity.getId(); }

    public long getEditorId() { return profile.getId(); }

    public NotificationType getNotificationType() {
        return notificationType;
    }
}
