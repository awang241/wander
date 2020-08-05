package com.springvuegradle.service;

import com.springvuegradle.dto.ActivityRoleCountResponse;
import com.springvuegradle.enums.ActivityMessage;
import com.springvuegradle.enums.ActivityResponseMessage;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.ActivityType;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.ActivityMembershipRepository;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.repositories.ActivityTypeRepository;
import com.springvuegradle.repositories.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
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

    /**
     * Gets the number of people who have a role in an activity
     * @param activityId the ID of the activity we are counting the amount of roles for
     * @return the number of people who have different roles in an activity
     */
    public ActivityRoleCountResponse getRoleCounts(long activityId){
        long organizers, followers, participants;
        organizers = followers = participants = 0;
        List<ActivityMembership> memberships = membershipRepo.findActivityMembershipsByActivity_Id(activityId);
        if(memberships.isEmpty()){
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }
        for(ActivityMembership membership: memberships){
            if(membership.getRole().equals(ActivityMembership.Role.PARTICIPANT)){
                participants++;
            } else if(membership.getRole().equals(ActivityMembership.Role.FOLLOWER)){
                followers++;
            }  else if(membership.getRole().equals(ActivityMembership.Role.ORGANISER)){
                organizers++;
            }
        }
        return new ActivityRoleCountResponse(organizers, participants, followers);
    }

    /**\
     * Checks if the userId corresponds to the creator of the activity associated with the activityId.
     * @param userId id of the user making the request
     * @param activityId id of the activity
     * @return true if userId matches the creatorId, false otherwise
     */
    public boolean isProfileActivityCreator(Long userId, Long activityId) {
        Optional<Activity> activity = activityRepo.findById(activityId);
        if (activity.isPresent()) {
            List<ActivityMembership> creator = membershipRepo.findActivityMembershipsByActivity_IdAndRole(activityId, ActivityMembership.Role.CREATOR);
            if (creator.size() > 0) {
                Long creatorId = creator.get(0).getProfile().getId();
                return creatorId.equals(userId);
            }
        }
        return false;
    }

    /**
     * Checks if the activity exists in the repository, checks if profile has membership,
     * deletes the membership if profile has membership
     *
     * @param profileId the profile which membership needs to be removed
     * @param activityId the specified activity
     * @return true if membership was found and deleted, false otherwise
     */
    public boolean removeMembership(Long profileId, Long activityId) {
        if (activityRepo.existsById(activityId)) {
            for (ActivityMembership membership : membershipRepo.findAll()) {
                if (membership.getActivity().getId() == activityId && membership.getProfile().getId().equals( profileId)) {
                    membershipRepo.delete(membership);
                    membership.getProfile().removeActivity(membership);
                    return true;
                }
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

    /**
     * Returns all activities associated with the given profile by role.
     * @param profileId The ID of the profile whose activities are being retrieved.
     * @param role The role of the user in the activity.
     * @return A list of the given profile's activities by role.
     */
    public List<Activity> getActivitiesByProfileIdByRole(Long profileId, String role) {

        Profile profile = profileRepo.findAllById(profileId).get(0);
        List<Activity> userActivities = new ArrayList<>();
        for (ActivityMembership activityMembership : profile.getActivities()) {
            if (activityMembership.getRole().toString().equals(role.toUpperCase())) {
                userActivities.add(activityMembership.getActivity());
            }
        }
        return userActivities;
    }

    /**
     * Returns all the activities with a given privacy level
     * @param privacy A string from the front end that specifies a privacy level
     * @return A list of the activities with a given privacy level
     */
    public List<Activity> getActivitiesWithPrivacyLevel(String privacy) {
        Integer privacyLevel;
        switch (privacy) {
            case "private":
                privacyLevel = 0;
                break;
            case "friends":
                privacyLevel = 1;
                break;
            case "public":
                privacyLevel = 2;
                break;
            default:
                throw new IllegalArgumentException(ActivityMessage.INVALID_PRIVACY.getMessage());
        }
        List<Activity> publicActivities = activityRepo.findAllPublic(privacyLevel);
        return publicActivities;
    }

    /**
     * Checks if an activity is valid by checking all fields and throws an exception otherwise.
     * @param activity the activity to check the fields of.
     */
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
     * Assigns the given role to the user for an activity.
     * @param activityId the id of the activity we want to assign the role for.
     * @param profileId the id of the user we want to assign the role to.
     * @param activityRole the role we want to assign to the user for the activity.
     */
    public void addActivityRole(Long activityId, Long profileId, String activityRole) throws IllegalArgumentException {
        Optional<Profile> optionalProfile = profileRepo.findById(profileId);
        Optional<Activity> optionalActivity = activityRepo.findById(activityId);
        if (optionalProfile.isEmpty()) {
            throw new IllegalArgumentException(ActivityMessage.PROFILE_NOT_FOUND.getMessage());
        } else if (optionalActivity.isEmpty()) {
            throw new IllegalArgumentException(ActivityMessage.ACTIVITY_NOT_FOUND.getMessage());
        }
        Profile profile = optionalProfile.get();
        Activity activity = optionalActivity.get();
        ActivityMembership.Role role = ActivityMembership.Role.valueOf(activityRole.toUpperCase());
        ActivityMembership activityMembership = new ActivityMembership(activity, profile, role);
        membershipRepo.save(activityMembership);
        profile.addActivity(activityMembership);
        activity.addMember(activityMembership);
        profileRepo.save(profile);
    }

    /**
     * Edits the privacy of a given activity to the given privacy.
     * @param privacy a string defining the privacy level.
     * @param activityId the id of the activity to edit.
     */
    public void editActivityPrivacy(String privacy, Long activityId) {
        Optional<Activity> optionalActivity = activityRepo.findById(activityId);
        if (optionalActivity.isEmpty()) {
            throw new IllegalArgumentException(ActivityMessage.ACTIVITY_NOT_FOUND.getMessage());
        } else {
            Integer privacyLevel;
            switch (privacy) {
                case "private":
                    privacyLevel = 0;
                    break;
                case "friends":
                    privacyLevel = 1;
                    break;
                case "public":
                    privacyLevel = 2;
                    break;
                default:
                    throw new IllegalArgumentException(ActivityMessage.INVALID_PRIVACY.getMessage());
            }
            Activity activity = optionalActivity.get();
            activity.setPrivacyLevel(privacyLevel);
            activityRepo.save(activity);
        }
    }
}
