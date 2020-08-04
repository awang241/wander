package com.springvuegradle.service;

import com.springvuegradle.enums.ActivityResponseMessage;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import com.springvuegradle.repositories.spec.ProfileSpecifications;
import com.springvuegradle.utilities.FieldValidationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import javax.persistence.EntityNotFoundException;
import java.util.*;

/**
 * Service-layer class containing business logic handling activities.
 */
@Service(value = "activityService")
public class ActivityService {

    private ProfileRepository profileRepo;
    private ActivityRepository activityRepo;
    private ActivityTypeRepository typeRepo;
    private ActivityMembershipRepository membershipRepo;

    /**
     * Autowired constructor for Spring to create an ActivityService and inject the correct dependencies.
     * @param profileRepo
     * @param activityRepo
     * @param activityTypeRepo
     * @param activityMembershipRepository
     */
    @Autowired
    public ActivityService(ProfileRepository profileRepo, ActivityRepository activityRepo, ActivityTypeRepository activityTypeRepo,
                           ActivityMembershipRepository activityMembershipRepository) {
        this.profileRepo = profileRepo;
        this.activityRepo = activityRepo;
        this.typeRepo = activityTypeRepo;
        this.membershipRepo = activityMembershipRepository;
    }

    /**
     * Inserts the given activity into the database and registers the profile with the given ID as the creator.
     * @param activity The activity to be added to the database.
     * @param creatorId The ID of the creator's profile.
     */
    public void create(Activity activity, Long creatorId) {
        validateActivity(activity);
        Optional<Profile> profileResult = profileRepo.findById(creatorId);
        if (profileResult.isEmpty()) {
            throw new EntityNotFoundException(ActivityResponseMessage.INVALID_PROFILE.toString());
        }
        Profile profile = profileResult.get();

        Set<ActivityType> updatedActivityType = new HashSet<>();
        for (ActivityType activityType : activity.retrieveActivityTypes()) {
            List<ActivityType> resultActivityTypes = typeRepo.findByActivityTypeName(activityType.getActivityTypeName());
            updatedActivityType.add(resultActivityTypes.get(0));
        }
        activity.setActivityTypes(updatedActivityType);

        Activity result = activityRepo.save(activity);
        ActivityMembership activityMembership = new ActivityMembership(result, profile, ActivityMembership.Role.CREATOR);
        membershipRepo.save(activityMembership);
        profile.addActivity(activityMembership);
        activity.addMember(activityMembership);
        profileRepo.save(profile);
    }

    /**
     * Check if there is an activity with an associated activityId
     *
     * @param activityId the id of the activity we want to retrieve
     * @return The activity if it exists, null otherwise
     */
    public Activity read(Long activityId) {
        Optional<Activity> activity = activityRepo.findById(activityId);
        if (activity.isPresent()) {
            return activity.get();
        }
        return null;
    }

    /**
     * Updates the activity given the new activity object and the id of the activity you want to update.
     * @param activity the new activity object
     * @param activityId the id of the activity to update
     */
    public void update(Activity activity, Long activityId) {
        validateActivity(activity);
        Optional<Activity> result = activityRepo.findById(activityId);
        if (result.isPresent()) {
            Activity db_activity = result.get();
            Set<ActivityType> updatedActivityTypes = new HashSet<>();
            for (ActivityType activityType : activity.retrieveActivityTypes()) {
                List<ActivityType> resultActivityTypes = typeRepo.findByActivityTypeName(activityType.getActivityTypeName());
                updatedActivityTypes.add(resultActivityTypes.get(0));
            }
            db_activity.setActivityTypes(updatedActivityTypes);
            db_activity.update(activity);
            activityRepo.save(db_activity);
        } else {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }
    }

    /**
     * Checks if the activity exists in the repository, deletes the activity.
     *
     * @param activityId the activity to delete.
     * @return if activity exists then it deletes it and returns true. False otherwise.
     */
    public boolean delete(Long activityId) {
        if (activityRepo.existsById(activityId)) {
            for (ActivityMembership membership : membershipRepo.findAll()) {
                if (membership.getActivity().getId() == activityId) {
                    Profile profile = membership.getProfile();
                    membershipRepo.delete(membership);
                    profile.removeActivity(membership);
                }
            }
            Optional<Activity> activity = activityRepo.findById(activityId);
            for (ActivityType activityType : typeRepo.findAll()) {
                if (activity.isPresent() && activityType.getActivities().contains(activity.get())) {
                    activityType.removeActivity(activity.get());
                }
            }
            activityRepo.deleteById(activityId);

            return true;
        }
        return false;
    }

    /**\
     * Checks if the userId corresponds to the creator of the activity associated with the activityId.
     * @param userId id of the user making the request
     * @param activityId id of the activity
     * @return true if userId matches the creatorId, false otherwise
     */
    public boolean checkActivityCreator(Long userId, Long activityId) {
        Optional<Activity> activity = activityRepo.findById(activityId);
        if (activity.isPresent()) {
            Optional<ActivityMembership> creator = membershipRepo.findActivityMembershipsByActivity_IdAndRole(activityId, ActivityMembership.Role.CREATOR);
            if (creator.isPresent()) {
                Long creatorId = creator.get().getProfile().getId();
                return creatorId.equals(userId);
            }
        }
        return false;
    }

    /**
     * Returns all activities associated with the given profile.
     * @param profileId The ID of the profile whose activities are being retrieved.
     * @return A list of the given profile's activities.
     */
    public List<Activity> getActivitiesByProfileId(Long profileId) {
        Profile profile = profileRepo.findAllById(profileId).get(0);
        List<Activity> userActivities = new ArrayList<>();
        for (ActivityMembership activityMembership : profile.getActivities()) {
            userActivities.add(activityMembership.getActivity());
        }
        return userActivities;
    }

    private void validateActivity(Activity activity) {
        if (activity.getActivityName() == null || activity.getActivityName().isBlank()) {
            throw new IllegalArgumentException(ActivityResponseMessage.MISSING_NAME.toString());
        }

        if (Boolean.FALSE.equals(activity.getContinuous())) {
            if (activity.getStartTime() == null) {
                throw new IllegalArgumentException(ActivityResponseMessage.MISSING_START_DATE.toString());
            } else if (activity.getEndTime() == null) {
                throw new IllegalArgumentException(ActivityResponseMessage.MISSING_END_DATE.toString());
            } else if (activity.getEndTime().isBefore(activity.getStartTime())) {
                throw new IllegalArgumentException(ActivityResponseMessage.INVALID_DATES.toString());
            }
        }

        if (activity.retrieveActivityTypes() == null || activity.retrieveActivityTypes().isEmpty()) {
            throw new IllegalArgumentException(ActivityResponseMessage.MISSING_TYPES.toString());
        } else {
            for (ActivityType type : activity.retrieveActivityTypes()) {
                if (!typeRepo.existsByActivityTypeName(type.getActivityTypeName())) {
                    throw new IllegalArgumentException(ActivityResponseMessage.INVALID_TYPE.toString());
                }
            }
        }
    }

    /**
     * Returns the specified page from the list of all activities.
     *
     * @param request A page request containing the index and size of the page to be returned.
     * @return The specified page from the list of all activities.
     */
    public Page<Activity> getAllActivities(Pageable request) {

        return activityRepo.findAll(request);
    }

    /**
     * Returns the specified page from the list of all activities.
     *
     * @param request A page request containing the index and size of the page to be returned.
     * @param profileId The user's profile id
     * @return The specified page from the list of all activities.
     */
    public Page<Activity> getUsersActivities(Pageable request, Long profileId) {
        return activityRepo.findAllByProfileId(profileId, request);
    }
}
