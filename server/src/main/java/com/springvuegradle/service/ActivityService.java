package com.springvuegradle.service;

import com.springvuegradle.Model.Activity;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
}
