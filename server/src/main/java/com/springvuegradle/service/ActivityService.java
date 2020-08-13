package com.springvuegradle.service;

import com.springvuegradle.dto.ActivityRoleCountResponse;
import com.springvuegradle.dto.MembersRequest;
import com.springvuegradle.dto.SimplifiedActivity;
import com.springvuegradle.enums.*;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
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
    private ActivityParticipationRepository participationRepo;

    /**
     * Autowired constructor for Spring to create an ActivityService and inject the correct dependencies.
     * @param profileRepo the profile repository being injected.
     * @param activityRepo the activity repository being injected.
     * @param activityTypeRepo the activity type repository being injected.
     * @param activityMembershipRepository the activity membership repository being injected.
     */
    @Autowired
    public ActivityService(ProfileRepository profileRepo, ActivityRepository activityRepo, ActivityTypeRepository activityTypeRepo,
                           ActivityMembershipRepository activityMembershipRepository, ActivityParticipationRepository participationRepo) {
        this.profileRepo = profileRepo;
        this.activityRepo = activityRepo;
        this.typeRepo = activityTypeRepo;
        this.membershipRepo = activityMembershipRepository;
        this.participationRepo = participationRepo;
    }

    /**
     * Inserts the given activity into the database and registers the profile with the given ID as the creator.
     * @param activity The activity to be added to the database.
     * @param creatorId The ID of the creator's profile.
     */
    public void create(Activity activity, Long creatorId) {
        validateActivity(activity);
        Optional<Profile> profileResult =  profileRepo.findById(creatorId);
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
    public List<Activity> getActivitiesByProfileIdByRole(PageRequest request, Long profileId, ActivityMembership.Role role) {
        List<Activity> userActivities = new ArrayList<>();
        Page<ActivityMembership> memberships = membershipRepo.findAllByProfileId(profileId, request);
        for (ActivityMembership membership : memberships) {
            if (role.equals(ActivityMembership.Role.CREATOR)) {
                userActivities.add(membership.getActivity());
            } else if (membership.getRole().equals(role) && membership.getActivity().getPrivacyLevel() > 0) {
                userActivities.add(membership.getActivity());
            }
        }
        return userActivities;
    }

    /**
     * Returns all the activities with privacy level of 2. Aka returns all public activities.
     * @param request contains the count and start index for pagination
     * @return list of activities
     */
    public List<Activity> getPublicActivities(PageRequest request) {
        Page<Activity> activities = activityRepo.findAllByPrivacyLevelWithPagination(2, request);
        return activities.getContent();
    }

    /**
     * Returns all the activities that the user is a creator or organiser of.
     * @param request contains the count and start index for pagination
     * @param profileId the id of the user we want to check is the creator or organiser of an activity
     * @return list of activities
     */
    public List<Activity> getActivitiesUserCanModify(PageRequest request, Long profileId) {
        List<Activity> userActivities = new ArrayList<>();
        Page<ActivityMembership> memberships = membershipRepo.findAllByProfileId(profileId, request);
        for (ActivityMembership membership : memberships) {
            if (membership.getRole().equals(ActivityMembership.Role.CREATOR) ||
                    (membership.getRole().equals(ActivityMembership.Role.ORGANISER) && membership.getActivity().getPrivacyLevel() > 0)) {
                userActivities.add(membership.getActivity());
            }
        }
        return userActivities;
    }


    /**
     * Returns all the new activities for the user to discover.
     * @param request contains the count and start index for pagination
     * @param profileId refers to id of the user we want to check
     * @return list of activities the user has no current association with.
     */
    public List<Activity> getNewActivities(PageRequest request, Long profileId) {
        List<Activity> activities = new ArrayList<>();
        List<Activity> results = activityRepo.findAllByPrivacyLevel(2);
        if (results!= null) {
            for (Activity activity: results) {
                boolean profileAssociated = false;
                for (ActivityMembership am: activity.getMembers()) {
                    if (am.getProfile().getId().equals(profileId)) {
                        profileAssociated=true;
                    }
                }
                if (!profileAssociated) {
                    activities.add(activity);
                }
            }
        }
        return activities.subList(Math.min(activities.size(), request.getPageNumber()), Math.min(activities.size(), request.getPageSize()));
    }


    /**
     * Returns all activities with the given privacy level.
     *
     * @param privacy The given privacy level
     * @return A list of the activities with the given privacy level.
     */
    public List<Activity> getActivitiesWithPrivacyLevel(ActivityPrivacy privacy) {
        int privacyLevel = privacy.ordinal();
        return activityRepo.findAllByPrivacyLevel(privacyLevel);
    }

    /**
     * Switch that returns a privacy level based on an input string
     *
     * @param privacy A string from the front end
     * @return an integer for the backend, an exception otherwise
     */
    private int determinePrivacyLevel(String privacy) {
        int privacyLevel;
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
     * Returns a list of all activities in the repository
     * @return a list of all activities in the repository
     */
    public List<Activity> getAllActivities() {
        return activityRepo.findAll();
    }

    /**
     * Return an activity by activity id and profile id based on the privacy activity and role of the user.
     * @param profileId The ID of the profile that is requesting the Activity
     * @param activityId The ID of the activity that is being retrieved
     * @return An activity object. If it does not exist or the user is not authorized returns null.
     */
    public Activity getActivityByActivityId(Long profileId, Long activityId) {
        Optional<Activity> activity = activityRepo.findById(activityId);
        if (activity.isPresent()) {
            Optional<ActivityMembership> activityMembership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileId);
            if (activity.get().getPrivacyLevel().equals(2)){
                return activity.get();
            }
            else if (activityMembership.isPresent()) {
                if (activity.get().getPrivacyLevel().equals(1)) {
                    return activity.get();
                }
                else if (activityMembership.get().getRole().equals(ActivityMembership.Role.CREATOR)) {
                    return activity.get();
                }
            }
            return null;
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
    public void addActivityRole(Long activityId, Long profileId, String activityRole) {
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
    public List<SimplifiedActivity> createSimplifiedActivities(List<Activity> activities) {
        List<SimplifiedActivity> simplifiedActivities = new ArrayList<>();
        activities.forEach(k -> simplifiedActivities.add(new SimplifiedActivity(k)));
        return simplifiedActivities;
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
     * Adds the members to the activity under the given roles. If the member is already a role, their role is changed.
     * @param members list of MembersRequest objects which contain member emails and their associated roles.
     * @param activityId id referring to the activity.
     * @throws IllegalArgumentException when activity or email is not found.
     */
    public void addMembers(List<MembersRequest> members, long activityId) {
        Optional<Activity> optionalActivity = activityRepo.findById(activityId);
        if (optionalActivity.isEmpty()) {
            throw new IllegalArgumentException(ActivityMessage.ACTIVITY_NOT_FOUND.getMessage());
        }
        Map<Profile, String> profiles = new HashMap<>();
        for (MembersRequest member: members) {
            List<Profile> profilesWithEmail = profileRepo.findByPrimaryEmail(member.getEmail());
            if (!profilesWithEmail.isEmpty()) {
                profiles.put(profilesWithEmail.get(0), member.getRole());
            } else {
                throw new IllegalArgumentException(ActivityResponseMessage.INVALID_EMAILS.toString());
            }
        }
        Activity activity = optionalActivity.get();
        for (Profile profile: profiles.keySet()) {
            Optional<ActivityMembership> optionalMembership = membershipRepo.findByActivity_IdAndProfile_Id(profile.getId(), activityId);
            ActivityMembership.Role role = ActivityMembership.Role.valueOf(profiles.get(profile).toUpperCase());
            ActivityMembership membership;
            if (optionalMembership.isPresent()) {
                membership = optionalMembership.get();
                membership.setRole(role);
            } else {
                membership = new ActivityMembership(activity, profile, role);
            }
            membershipRepo.save(membership);
            profile.addActivity(membership);
            activity.addMember(membership);
            profileRepo.save(profile);
        }
    }

    /**
     * Saves the given participation details of a user who participated in a specific activity to the repository.
     * @param activityId    The ID of the activity
     * @param profileId     The ID of the profile
     * @param participation The participation being saved. The participation's activity and profile fields will be
     *                      replaced by ones with the given ID's.
     */
    public void createParticipation(long activityId, long profileId, ActivityParticipation participation) {
        checkParticipationHelper(activityId, profileId);
        Profile profile = profileRepo.getOne(profileId);
        Activity activity = activityRepo.getOne(activityId);
        participation.setProfile(profile);
        participation.setActivity(activity);
        participation = participationRepo.save(participation);
        profile.addParticipation(participation);
        profileRepo.save(profile);
        activity.addParticipation(participation);
        activityRepo.save(activity);
    }

    /**
     * Updates the fields of an existing participation
     * @param activityId      The ID of the activity
     * @param profileId       The ID of the profile
     * @param participationId The ID of the participation being changed
     * @param participation   An ActivityParticipation object which contains the new fields to be changed
     * @throws IllegalArgumentException if a participation does not exist
     */
    public void updateParticipation(long activityId, long profileId, long participationId, ActivityParticipation participation) {
        checkParticipationHelper(activityId, profileId);
        Optional<ActivityParticipation> participationResult = participationRepo.findById(participationId);
        if (participationResult.isEmpty()) {
            throw new IllegalArgumentException(ParticipationErrorMessage.PARTICIPATION_NOT_FOUND.toString());
        }
        ActivityParticipation dbParticipation = participationResult.get();
        dbParticipation.updateActivityParticipation(participation);
        participationRepo.save(dbParticipation);
    }

    /**
     * Checks if an activity and profile exists
     * @param activityId
     * @param profileId
     * @throws IllegalArgumentException If no profile or activity with the given ID exists in the repository
     */
    public void checkParticipationHelper(long activityId, long profileId) {
        Optional<Activity> activityResult = activityRepo.findById(activityId);
        if (activityResult.isEmpty()) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }
        Optional<Profile> profileResult = profileRepo.findById(profileId);
        if (profileResult.isEmpty()) {
            throw new IllegalArgumentException(ProfileErrorMessage.PROFILE_NOT_FOUND.toString());
        }
    }

}
