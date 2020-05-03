package com.springvuegradle.service;

import com.springvuegradle.Controller.Profile_Controller;
import com.springvuegradle.Controller.enums.ActivityResponseMessage;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityMembership;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service(value = "activityService")
public class ActivityService {

    private ProfileRepository profileRepo;
    private ActivityRepository activityRepo;
    private ActivityTypeRepository typeRepo;

    @Autowired
    public ActivityService(ProfileRepository profileRepo, ActivityRepository activityRepo, ActivityTypeRepository activityTypeRepo) {
        this.profileRepo = profileRepo;
        this.activityRepo = activityRepo;
        this.typeRepo = activityTypeRepo;
    }

    public void create(Activity activity, Long creatorId) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    public void read(Long activityId) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    public void update(Activity activity, Long activityId) {
        if (!activityRepo.existsById(activityId)) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }

        for (ActivityType type: activity.getActivityTypes()) {
            if (!typeRepo.existsByActivityTypeName(type.getActivityTypeName())) {
                throw new IllegalArgumentException(ActivityResponseMessage.INVALID_TYPE.toString());
            }
        }
    }

    public void delete(Long activityId) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    public List<Activity> getActivitiesByProfileId(Long profileId) {
        Profile profile = profileRepo.findAllById(profileId).get(0);
        List<Activity> userActivities = new ArrayList<>();
        for (ActivityMembership activityMembership: profile.getActivities()) {
            userActivities.add(activityMembership.getActivity());
        }
        return userActivities;
    }
}
