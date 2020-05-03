package com.springvuegradle.service;

import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityMembership;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service(value = "activityService")
public class ActivityService {

    @Autowired
    private ProfileRepository profileRepo;
    @Autowired
    private ActivityRepository activityRepo;
    @Autowired
    private ActivityTypeRepository typeRepo;

    public void create(Activity activity, Long creatorId) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    public void read(Long activityId) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    public void update(Activity activity, Long activityId) {
        throw new UnsupportedOperationException("Not yet implemented");
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
