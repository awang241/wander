package com.springvuegradle.Model;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

@Entity
public class ActivityMembership {
    public enum Role {
        CREATOR, ORGANISER, PARTICIPANT, FOLLOWER;
    }

    @Embeddable
    public static class ActivityMembershipId implements Serializable {

        @Column(name = "activity_fk")
        protected Long activityId;

        @Column(name = "profile_fk")
        protected Long profileId;

        public ActivityMembershipId() {}

        public ActivityMembershipId(Long activityId, Long profileId) {
            this.activityId = activityId;
            this.profileId = profileId;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ActivityMembershipId that = (ActivityMembershipId) o;
            return Objects.equals(activityId, that.activityId) &&
                    Objects.equals(profileId, that.profileId);
        }

        @Override
        public int hashCode() {
            return Objects.hash(activityId, profileId);
        }
    }

    @EmbeddedId
    private ActivityMembershipId id;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "profile_fk", insertable = false, updatable = false)
    private Profile profile;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "activity_fk", insertable = false, updatable = false)
    private Activity activity;

    @Column(name = "role")
    private Role role;

    public ActivityMembership() {

    }

    public ActivityMembership(Activity activity, Profile profile, Role role) {
        this.id = new ActivityMembershipId(activity.getId(), profile.getId());
        this.activity = activity;
        this.profile = profile;
        this.role = role;
    }

    public Profile getProfile() {
        return profile;
    }

    public void setProfile(Profile profile) {
        this.profile = profile;
    }

    public Activity getActivity() {
        return activity;
    }

    public void setActivity(Activity activity) {
        this.activity = activity;
    }

    public Role getRole() {
        return role;
    }

    public void setRole(Role role) {
        this.role = role;
    }
}
