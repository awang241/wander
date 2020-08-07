package com.springvuegradle.service;

import com.springvuegradle.dto.ActivityRoleCountResponse;
import com.springvuegradle.dto.SimplifiedActivity;
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
     * @param profileRepo the profile repository being injected.
     * @param activityRepo the activity repository being injected.
     * @param activityTypeRepo the activity type repository being injected.
     * @param activityMembershipRepository the activity membership repository being injected.
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
     * Updates the activity given the new activity object and the id of the activity you want to update.
     * @param activity the new activity object
     * @param activityId the id of the activity to update
     */
    public void update(Activity activity, Long activityId) {
        validateActivity(activity);
        Optional<Activity> result = activityRepo.findById(activityId);
        if (result.isPresent()) {
            Activity dbActivity = result.get();
            Set<ActivityType> updatedActivityTypes = new HashSet<>();
            for (ActivityType activityType : activity.retrieveActivityTypes()) {
                List<ActivityType> resultActivityTypes = typeRepo.findByActivityTypeName(activityType.getActivityTypeName());
                updatedActivityTypes.add(resultActivityTypes.get(0));
            }
            dbActivity.setActivityTypes(updatedActivityTypes);
            dbActivity.update(activity);
            activityRepo.save(dbActivity);
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
            Optional<ActivityMembership> membership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, userId);
            if (membership.isPresent()) {
                return membership.get().getRole().equals(ActivityMembership.Role.CREATOR);
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
            if (role.toUpperCase().equals("CREATOR")) {
                userActivities.add(activityMembership.getActivity());
            } else if (activityMembership.getRole().toString().equals(role.toUpperCase()) && activityMembership.getActivity().getPrivacyLevel() > 0) {
                userActivities.add(activityMembership.getActivity());
            }
        }
        return userActivities;
    }

    /**
     * Returns all the activities with a given privacy level
     *
     * @param privacy A string from the front end that specifies a privacy level
     * @return A list of the activities with a given privacy level
     */
    public List<Activity> getActivitiesWithPrivacyLevel(String privacy) {
        int privacyLevel = determinePrivacyLevel(privacy);
        List<Activity> publicActivities = activityRepo.findAllPublic(privacyLevel);
        return publicActivities;
    }

    /**
     * Switch that returns a privacy level based on an input string
     *
     * @param privacy A string from the front end
     * @return an integer for the backend, an exception otherwise
     */
    private int determinePrivacyLevel(String privacy) {
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
        return privacyLevel;
    }

    /**
     * Return an activity by activity id.
     * @param activityId The ID of the activity that is being retrieved
     * @return An activity object. If it does not exist returns null.
     */
    public Activity getActivityByActivityId(Long activityId) {
        Optional<Activity> activity = activityRepo.findById(activityId);
        if (activity.isPresent()) {
            return activity.get();
        }
        return null;
    }

    /**
     * Checks if an activity is valid by checking all fields and throws an exception otherwise.
     *
     * @param activity the activity to check the fields of.
     */
    /**
     * Return an activity by activity id.
     * @param activityId The ID of the activity that is being retrieved
     * @return An activity object. If it does not exist returns null.
     */
    public Activity getActivityByActivityId(Long activityId) {
        Optional<Activity> activity = activityRepo.findById(activityId);
        if (activity.isPresent()) {
            return activity.get();
        }
        return null;
    }

    /**
     * Checks all the fields in an Activity for errors. Throws an error if there are any
     * @param activity The activity object
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
            int privacyLevel = determinePrivacyLevel(privacy);
            Activity activity = optionalActivity.get();
            activity.setPrivacyLevel(privacyLevel);
            activityRepo.save(activity);
        }
    }

    /**
     * Sets the role of an activity member to the given role. Profiles cannot be set as creators, and creators
     * cannot have their role changed for that activity.
     * @param profileBeingEditedId The ID of the profile whose role is being changed
     * @param profileDoingEditingId The ID of the profile who is doing the editing
     * @param activityId the ID of the activity
     * @param newRole The role the member is being changed to
     * @throws IllegalArgumentException if the parameters do not match the database constraints (e.g. no profile with
     * that id exists, or the given profile isn't a member of that activity), or if the profile is being set to or from
     * the Creator role.
     */
    public void setProfileRole(long profileBeingEditedId, long profileDoingEditingId, long activityId, ActivityMembership.Role newRole) {
        if (newRole.equals(ActivityMembership.Role.CREATOR)) {
            throw new IllegalArgumentException(ActivityResponseMessage.EDITING_CREATOR.toString());
        }

        if(!canChangeRole(profileDoingEditingId, profileBeingEditedId, activityId, newRole)){
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_PERMISSION.toString());
        }

        Optional<ActivityMembership> optionalMembership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileBeingEditedId);
        if (optionalMembership.isEmpty()) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_MEMBERSHIP.toString());
        }

        ActivityMembership membership = optionalMembership.get();
        if (membership.getRole() == ActivityMembership.Role.CREATOR) {
            throw new IllegalArgumentException(ActivityResponseMessage.EDITING_CREATOR.toString());
        }
        membership.setRole(newRole);
        membershipRepo.save(membership);
    }

    /**
     * Checks whether someone has rights to change the role of a user in an activity
     * Users should only be able to change someone to an organizer if they are a creator/organizer or admin
     * Users should be able to change someone else to a participant or follower if they are creator/organizer, admin, or it is themself
     * @param profileDoingEditingId the ID of the profile who is editing the activities membership
     * @param activityId The ID of the activity we are editing
     * @param newRole The role we are trying to change the user to
     * @param profileBeingEditedId The ID of the user who is attempting to change the role
     * @return whether the user doing the editing can change the profiles membership to organizer
     */
    private boolean canChangeRole(long profileDoingEditingId, long profileBeingEditedId, long activityId, ActivityMembership.Role newRole){
        boolean isCreatorOrOrganizer = false;
        boolean isAdmin = profileRepo.getOne(profileDoingEditingId).getAuthLevel() < 2;
        Optional<ActivityMembership> membership =membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileDoingEditingId);
        if(membership.isPresent()){
            ActivityMembership.Role editorRole = membership.get().getRole();
            if(editorRole.equals(ActivityMembership.Role.CREATOR) || editorRole.equals(ActivityMembership.Role.ORGANISER)){
                isCreatorOrOrganizer = true;
            }
        }
        if(newRole.equals(ActivityMembership.Role.ORGANISER)) {
            return isAdmin || isCreatorOrOrganizer;
        }
        return isAdmin || isCreatorOrOrganizer || profileBeingEditedId == profileDoingEditingId;
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
     * Returns a map of activities alongside the user's role in that activity.
     *
     * @param request A page request containing the index and size of the page to be returned.
     * @param profileId The user's profile id
     * @return A map of the activities and the role that the user has with that activity.
     */
    public Map<Activity,ActivityMembership.Role> getUsersActivities(Pageable request, Long profileId) {
        Page<ActivityMembership> memberships = membershipRepo.findAllByProfileId(profileId, request);
        Map<Activity, ActivityMembership.Role> activityRoleMap = new HashMap<>();
        for (ActivityMembership membership: memberships.getContent()) {
            activityRoleMap.put(membership.getActivity(), membership.getRole());
        }
        return activityRoleMap;
    }

    /**
     * Converts a list of normal activities into a list of simplified activities
     * @param activities a list of normal activity objects to be simplified
     * @return a list of simplified activities
     */
    public List<SimplifiedActivity> createSimplifiedActivities(Map<Activity, ActivityMembership.Role> activities) {
        List<SimplifiedActivity> simplifiedActivities = new ArrayList<>();
        activities.forEach((k,v) -> simplifiedActivities.add(new SimplifiedActivity(k, v.toString())));
        return simplifiedActivities;
    }

    /**
     * Sets the role of an activity member to the given role. Profiles cannot be set as creators, and creators
     * cannot have their role changed for that activity.
     * @param profileBeingEditedId The ID of the profile whose role is being changed
     * @param profileDoingEditingId The ID of the profile who is doing the editing
     * @param activityId the ID of the activity
     * @param newRole The role the member is being changed to
     * @throws IllegalArgumentException if the parameters do not match the database constraints (e.g. no profile with
     * that id exists, or the given profile isn't a member of that activity), or if the profile is being set to or from
     * the Creator role.
     */
    public void setProfileRole(long profileBeingEditedId, long profileDoingEditingId, long activityId, ActivityMembership.Role newRole) {
        if (newRole.equals(ActivityMembership.Role.CREATOR)) {
            throw new IllegalArgumentException(ActivityResponseMessage.EDITING_CREATOR.toString());
        }

        if(!canChangeRole(profileDoingEditingId, profileBeingEditedId, activityId, newRole)){
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_PERMISSION.toString());
        }

        Optional<ActivityMembership> optionalMembership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileBeingEditedId);
        if (optionalMembership.isEmpty()) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_MEMBERSHIP.toString());
        }

        ActivityMembership membership = optionalMembership.get();
        if (membership.getRole() == ActivityMembership.Role.CREATOR) {
            throw new IllegalArgumentException(ActivityResponseMessage.EDITING_CREATOR.toString());
        }
        membership.setRole(newRole);
        membershipRepo.save(membership);
    }

    /**
     * Checks whether someone has rights to change the role of a user in an activity
     * Users should only be able to change someone to an organizer if they are a creator/organizer or admin
     * Users should be able to change someone else to a participant or follower if they are creator/organizer, admin, or it is themself
     * @param profileDoingEditingId the ID of the profile who is editing the activities membership
     * @param activityId The ID of the activity we are editing
     * @param newRole The role we are trying to change the user to
     * @param profileBeingEditedId The ID of the user who is attempting to change the role
     * @return whether the user doing the editing can change the profiles membership to organizer
     */
    private boolean canChangeRole(long profileDoingEditingId, long profileBeingEditedId, long activityId, ActivityMembership.Role newRole){
        boolean isCreatorOrOrganizer = false;
        boolean isAdmin = profileRepo.getOne(profileDoingEditingId).getAuthLevel() < 2;
        Optional<ActivityMembership> membership =membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileDoingEditingId);
        if(membership.isPresent()){
            ActivityMembership.Role editorRole = membership.get().getRole();
            if(editorRole.equals(ActivityMembership.Role.CREATOR) || editorRole.equals(ActivityMembership.Role.ORGANISER)){
                isCreatorOrOrganizer = true;
            }
        }
        if(newRole.equals(ActivityMembership.Role.ORGANISER)) {
            return isAdmin || isCreatorOrOrganizer;
        }
        return isAdmin || isCreatorOrOrganizer || profileBeingEditedId == profileDoingEditingId;
    }
}
