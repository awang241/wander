package com.springvuegradle.service;

import com.springvuegradle.dto.ActivityRoleCountResponse;
import com.springvuegradle.dto.MembersRequest;
import com.springvuegradle.dto.SimplifiedActivity;
import com.springvuegradle.dto.responses.ActivityLocationResponse;
import com.springvuegradle.dto.responses.ActivityMemberProfileResponse;
import com.springvuegradle.enums.*;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import javax.persistence.EntityNotFoundException;
import java.security.AccessControlException;
import java.util.*;
import java.util.stream.Collectors;

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
    private NotificationService notificationService;


    /**
     * Autowired constructor for Spring to create an ActivityService and inject the correct dependencies.
     *
     * @param profileRepo                  the profile repository being injected.
     * @param activityRepo                 the activity repository being injected.
     * @param activityTypeRepo             the activity type repository being injected.
     * @param activityMembershipRepository the activity membership repository being injected.
     */
    @Autowired
    public ActivityService(ProfileRepository profileRepo, ActivityRepository activityRepo, ActivityTypeRepository activityTypeRepo,
                           ActivityMembershipRepository activityMembershipRepository, ActivityParticipationRepository participationRepo,
                           NotificationService notificationService) {
        this.profileRepo = profileRepo;
        this.activityRepo = activityRepo;
        this.typeRepo = activityTypeRepo;
        this.membershipRepo = activityMembershipRepository;
        this.participationRepo = participationRepo;
        this.notificationService = notificationService;
    }

    /**
     * Inserts the given activity into the database and registers the profile with the given ID as the creator.
     *
     * @param activity  The activity to be added to the database.
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

        notificationService.createNotification(NotificationType.ACTIVITY_CREATED,
                activity,
                profile,
                profile.getFullName() + " created a new activity called " + activity.getActivityName() + ".");

        profileRepo.save(profile);
    }

    /**
     * Updates the activity given the new activity object and the id of the activity you want to update.
     *
     * @param activity   the new activity object
     * @param activityId the id of the activity to update
     * @param profileId  the id of the profile updating the activity
     */
    public void update(Activity activity, Long activityId, Long profileId) {
        Optional<Profile> editor = profileRepo.findById(profileId);
        if (editor.isEmpty()) {
            throw new IllegalArgumentException(ProfileErrorMessage.PROFILE_NOT_FOUND.toString());
        }
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
            notificationService.createNotification(NotificationType.ACTIVITY_EDITED, dbActivity, editor.get(),
                    editor.get().getFullName() + " edited an activity called " + dbActivity.getActivityName() + ".");
        } else {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }
    }

    /**
     * Filters a list of activities to ensure only ones containing all of the required activity types are shown
     * @param activities a list of activites
     * @param requiredActivityTypes
     * @return a list of activities that have all activity types
     */
    public List<Activity> filterActivitiesByActivityTypes(List<Activity> activities, List<ActivityType> requiredActivityTypes, String activityTypeSearchMethod){
        if(requiredActivityTypes.isEmpty() || activityTypeSearchMethod == null){return activities;}
        List<Activity> filteredActivities = new ArrayList<>();
        for(Activity activity: activities) {
            Set<ActivityType> actualActivityTypes = activity.getActivityTypeObjects();
            Set<ActivityType> result = requiredActivityTypes.stream()
                    .distinct()
                    .filter(actualActivityTypes::contains)
                    .collect(Collectors.toSet());
            if(result.size() == requiredActivityTypes.size() && activityTypeSearchMethod.equals("all")){
                filteredActivities.add(activity);
            } else if(result.size() > 0 && activityTypeSearchMethod.equals("any")){
                filteredActivities.add(activity);
            }
        }
        return filteredActivities;
    }

    /**
     * Takes a string array and returns an array of activity types corresponding to those strings
     * @param activityTypes
     * @return an array of activity types if types are specified, else none
     * @throws IllegalArgumentException if an activity type in the list does not exist
     */
    public List<ActivityType> getActivityTypesFromStringArray(String[] activityTypes) {
        if(activityTypes == null){return new ArrayList<>();}
        List<ActivityType> types = new ArrayList<>();
        for (String activityTypeString : activityTypes) {
            ActivityType type = typeRepo.getByActivityTypeName(activityTypeString);
            if (type == null) {
                throw new IllegalArgumentException();
            }
            types.add(type);
        }
        return types;
    }


    /**
     * Checks if the activity exists in the repository, deletes the activity.
     *
     * @param activityId the activity to delete.
     * @return if activity exists then it deletes it and returns true. False otherwise.
     */
    public boolean delete(Long activityId, Long profileId) {
        if (activityRepo.existsById(activityId)) {
            Profile profile = getModelObjectById(profileRepo, profileId);
            Activity activity = getModelObjectById(activityRepo, activityId);
            notificationService.createNotification(NotificationType.ACTIVITY_REMOVED, activity, profile,
                    profile.getFullName() + " deleted an activity called " + activity.getActivityName() + ".");
            notificationService.detachActivityFromNotifications(activity);

            for (ActivityMembership membership : membershipRepo.findAll()) {
                if (membership.getActivity().getId() == activityId) {
                    Profile profile1 = membership.getProfile();
                    membershipRepo.delete(membership);
                    profile1.removeActivity(membership);
                    membershipRepo.deleteActivityMembershipByProfileIdAndActivityId(profile1.getId(), activityId);
                }
            }
            for (ActivityParticipation participation : participationRepo.findAll()) {
                if (participation.getActivity().getId() == activityId) {
                    Profile profile2 = participation.getProfile();
                    participationRepo.delete(participation);
                    profile2.removeParticipation(participation);
                }
            }

            for (ActivityType activityType : typeRepo.findAll()) {
                if (activityType.getActivities().contains(activity)) {
                    activityType.removeActivity(activity);
                }
            }
            activityRepo.deleteById(activityId);
            return true;
        }
        return false;
    }

    /**
     * Removes an activity membership from an activity and generates a notification for that activity's remaining members.
     *
     * @param editorId   the ID of the profile doing the editing
     * @param editedId   the ID of the profile having their membership deleted
     * @param activityId the ID of the activity we are removing them from
     */
    public void removeUserRoleFromActivity(Long editorId, Long editedId, Long activityId) {
        Optional<ActivityMembership> membershipResult = membershipRepo.findByActivity_IdAndProfile_Id(activityId, editedId);
        if (membershipResult.isEmpty()) {
            throw new NoSuchElementException(ActivityMessage.MEMBERSHIP_NOT_FOUND.toString());
        }
        ActivityMembership membership = membershipResult.get();

        if (!canChangeRole(editorId, editedId, activityId, null)) {
            throw new AccessControlException("No permission");
        }

        if (membership.getRole() == ActivityMembership.Role.CREATOR) {
            throw new IllegalArgumentException(ActivityMessage.EDITING_CREATOR.toString());
        }

        NotificationType type;
        switch (membership.getRole()) {
            case FOLLOWER:
                type = NotificationType.ACTIVITY_FOLLOWER_REMOVED;
                break;
            case ORGANISER:
                type = NotificationType.ACTIVITY_ORGANISER_REMOVED;
                break;
            case PARTICIPANT:
                type = NotificationType.ACTIVITY_PARTICIPANT_REMOVED;
                break;
            default:
                throw new IllegalArgumentException(ActivityMessage.EDITING_CREATOR.toString());
        }

        Profile editor = getModelObjectById(profileRepo, editorId);
        Profile edited = getModelObjectById(profileRepo, editedId);
        String message;
        String activityName = membership.getActivity().getActivityName();
        if (editorId.equals(editedId)) {
            message = String.format("%s left the activity %s", edited.getFirstAndLastName(), activityName);
        } else {
            message = String.format("%s removed %s from the activity %s", editor.getFirstAndLastName(),
                    edited.getFirstAndLastName(), activityName);
        }
        notificationService.createNotification(type, membership.getActivity(), editor, message);

        membershipRepo.deleteActivityMembershipByProfileIdAndActivityId(editedId, activityId);
    }

    /**
     * Gets the number of people who have a role in an activity
     *
     * @param activityId the ID of the activity we are counting the amount of roles for
     * @return the number of people who have different roles in an activity
     */
    public ActivityRoleCountResponse getRoleCounts(long activityId) {
        long organisers = 0;
        long followers = 0;
        long participants = 0;
        List<ActivityMembership> memberships = membershipRepo.findActivityMembershipsByActivity_Id(activityId);
        if (memberships.isEmpty()) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }
        for (ActivityMembership membership : memberships) {
            if (membership.getRole().equals(ActivityMembership.Role.PARTICIPANT)) {
                participants++;
            } else if (membership.getRole().equals(ActivityMembership.Role.FOLLOWER)) {
                followers++;
            } else if (membership.getRole().equals(ActivityMembership.Role.ORGANISER)) {
                organisers++;
            }
        }
        return new ActivityRoleCountResponse(organisers, participants, followers);
    }

    /**
     * \
     * Checks if the userId corresponds to the creator of the activity associated with the activityId.
     *
     * @param userId     id of the user making the request
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
     * @param profileId  the profile which membership needs to be removed
     * @param activityId the specified activity
     * @return true if membership was found and deleted, false otherwise
     */
    public boolean removeMembership(Long profileId, Long activityId) {
        //Check against other service method in merge - This seems to be a double-up with removeUserRoleFromActivity
        if (activityRepo.existsById(activityId)) {
            for (ActivityMembership membership : membershipRepo.findAll()) {
                if (membership.getActivity().getId() == activityId && membership.getProfile().getId().equals(profileId)) {
                    membershipRepo.delete(membership);
                    membership.getProfile().removeActivity(membership);
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns all activities associated with the given profile by role.
     *
     * @param profileId The ID of the profile whose activities are being retrieved.
     * @param role      The role of the user in the activity.
     * @return A list of the given profile's activities by role.
     */
    public List<Activity> getActivitiesByProfileIdByRole(Long profileId, ActivityMembership.Role role, Integer startIndex, Integer count, Integer authLevel) {
        List<Activity> userActivities = new ArrayList<>();
        List<ActivityMembership> memberships = membershipRepo.findAllByProfileId(profileId);
        for (ActivityMembership membership : memberships) {
            if ((membership.getRole().equals(role) && (membership.getRole().equals(ActivityMembership.Role.CREATOR) || membership.getActivity().getPrivacyLevel() > 0 || authLevel < 2))) {
                userActivities.add(membership.getActivity());
            }
        }
        return userActivities.subList(Math.min(userActivities.size(), startIndex), Math.min(userActivities.size(), count + startIndex));
    }

    /**
     * Returns all the activities with privacy level of 2. Aka returns all public activities.
     *
     * @param request contains the count and start index for pagination
     * @return list of activities
     */
    public List<Activity> getPublicActivities(PageRequest request) {
        Page<Activity> activities = activityRepo.findAllByPrivacyLevelWithPagination(2, request);
        return activities.getContent();
    }

    /**
     * Returns all the activities that the user is a creator or organiser of.
     * @param count the number of activities to return. The function may return less if the last page is returned.
     * @param authLevel the user's authorisation level.
     * @param startIndex the index of an items to be returned; the page containing this item is returned.
     * @param profileId the id of the user we want to check is the creator or organiser of an activity
     * @return list of activities
     */
    public List<Activity> getActivitiesUserCanModify(Long profileId, Integer startIndex, Integer count, Integer authLevel) {
        List<Activity> userActivities = new ArrayList<>();
        List<ActivityMembership> memberships = membershipRepo.findAllByProfileId(profileId);
        for (ActivityMembership membership : memberships) {
            if (membership.getRole().equals(ActivityMembership.Role.CREATOR) ||
                    (membership.getRole().equals(ActivityMembership.Role.ORGANISER) &&
                            (membership.getActivity().getPrivacyLevel() > 0 || authLevel < 2))) {
                userActivities.add(membership.getActivity());
            }
        }
        return userActivities.subList(Math.min(userActivities.size(), startIndex), Math.min(userActivities.size(), count + startIndex));
    }

    /**
     * Returns all the new activities for the user to discover.
     *
     * @param profileId  refers to id of the user we want to check
     * @param startIndex used to sublist the activities returned such that the startIndex is the lower limit.
     * @param count      used to sublist the activities returned such that the startIndex+count is the upper limit.
     * @return list of activities the user has no current association with.
     */
    public List<Activity> getNewActivities(Long profileId, Integer startIndex, Integer count, Integer authLevel) {
        List<Activity> activities = new ArrayList<>();
        List<Activity> results = authLevel < 2 ? activityRepo.findAll() : activityRepo.findAllByPrivacyLevel(2);
        if (results != null) {
            for (Activity activity : results) {
                boolean profileAssociated = false;
                for (ActivityMembership am : activity.getMembers()) {
                    if (am.getProfile().getId().equals(profileId)) {
                        profileAssociated = true;
                    }
                }
                if (!profileAssociated) {
                    activities.add(activity);
                }
            }
        }
        return activities.subList(Math.min(activities.size(), startIndex), Math.min(activities.size(), startIndex + count));
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
            case "restricted":
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
     *
     * @return a list of all activities in the repository
     */
    public List<Activity> getAllActivities() {
        return activityRepo.findAll();
    }

    /**
     * Return an activity by activity id.
     *
     * Return an activity by activity id and profile id based on the privacy activity and role of the user.
     *
     * @param profileId  The ID of the profile that is requesting the Activity
     * @param activityId The ID of the activity that is being retrieved
     * @return An activity object. If it does not exist or the user is not authorized returns null.
     */
    public Activity getActivityByActivityId(Long profileId, Long activityId, Integer authLevel) {
        Optional<Activity> activity = activityRepo.findById(activityId);
        if (activity.isPresent()) {
            if (authLevel < 2) {
                return activity.get();
            }
            Optional<ActivityMembership> activityMembership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileId);
            if (activity.get().getPrivacyLevel().equals(2)) {
                return activity.get();
            } else if (activityMembership.isPresent()) {
                boolean roleIsCreator = activityMembership.get().getRole().equals(ActivityMembership.Role.CREATOR);
                if (activity.get().getPrivacyLevel().equals(1) || roleIsCreator) {
                    return activity.get();
                }
            }
            return null;
        }
        return null;
    }

    /**
     * Checks all the fields in an Activity for errors. Throws an error if there are any
     *
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
     * Assigns the given role to the user for an activity and creates notification for role change.
     *
     * @param activityId   the id of the activity we want to assign the role for.
     * @param profileId    the id of the user we want to assign the role to.
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

        NotificationType notificationType = NotificationService.getTypeForAddingRole(role);
        notificationService.createNotification(notificationType, activity, profile,
                profile.getFullName() + " changed role to " + activityRole + " for activity " + activity.getActivityName() + ".");

        profileRepo.save(profile);
    }


    /**
     * Returns a simplified list of users with roles in an activity
     *
     * @param activityId the ID of the activity we are getting members of
     * @return a list of simplified profiles with names and roles
     */
    public List<ActivityMemberProfileResponse> getActivityMembers(long activityId) {
        if (!activityRepo.existsById(activityId)) {
            throw new IllegalArgumentException();
        }
        return membershipRepo.findActivityMembershipsByActivityId(activityId);
    }

    /**
     * Returns a page of members of an activity that have the given role. Pagination information (count, index, etc.) is
     * provided in parameters.
     *
     * @param activityId the ID of the activity the members belong to.
     * @param role       the role members are matched against.
     * @param pageable   the pageable object providing pagination information.
     * @return a list of all activity members with the given role.
     */
    public Page<Profile> getActivityMembersByRole(long activityId, ActivityMembership.Role role, Pageable pageable) {
        if (!activityRepo.existsById(activityId)) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }
        return profileRepo.findByActivityAndRole(activityId, role, pageable);
    }

    /**
     * Edits the privacy of a given activity to the given privacy.
     *
     * @param privacy    a string defining the privacy level.
     * @param activityId the id of the activity to edit.
     */
    public void editActivityPrivacy(String privacy, Long activityId, Long profileId) {
        Optional<Activity> optionalActivity = activityRepo.findById(activityId);
        if (optionalActivity.isEmpty()) {
            throw new IllegalArgumentException(ActivityMessage.ACTIVITY_NOT_FOUND.getMessage());
        }
        int privacyLevel = determinePrivacyLevel(privacy);
        Activity activity = optionalActivity.get();
        activity.setPrivacyLevel(privacyLevel);
        activityRepo.save(activity);
        Profile editor = null;
        Optional<Profile> optionalEditor = profileRepo.findById(profileId);
        if (optionalEditor.isPresent()) {
            editor = optionalEditor.get();
        }
        notificationService.createNotification(NotificationType.ACTIVITY_PRIVACY_CHANGED,
                activity,
                editor,
                "Activity " + activity.getActivityName() +"'s privacy level has been changed to " + privacy);
    }

    /**
     * Converts a list of normal activities into a list of simplified activities
     *
     * @param activities a list of normal activity objects to be simplified
     * @return a list of simplified activities
     */
    public static List<ActivityLocationResponse> createActivityLocationResponse(List<Activity> activities) {
        List<ActivityLocationResponse> activityLocationResponse = new ArrayList<>();
        activities.forEach(activity -> activityLocationResponse.add(new ActivityLocationResponse(activity)));
        return activityLocationResponse;
    }

    /**
     * Sets the role of an activity member to the given role. Profiles cannot be set as creators, and creators
     * cannot have their role changed for that activity.
     *
     * @param editedId  The ID of the profile whose role is being changed
     * @param editorId The ID of the profile who is doing the editing
     * @param activityId            the ID of the activity
     * @param newRole               The role the member is being changed to
     * @throws IllegalArgumentException if the parameters do not match the database constraints (e.g. no profile with
     *                                  that id exists, or the given profile isn't a member of that activity), or if the profile is being set to or from
     *                                  the Creator role.
     */
    public void setProfileRole(long editedId, long editorId, long activityId, ActivityMembership.Role newRole) {
        if (newRole.equals(ActivityMembership.Role.CREATOR)) {
            throw new IllegalArgumentException(ActivityResponseMessage.EDITING_CREATOR.toString());
        }

        if (!canChangeRole(editorId, editedId, activityId, newRole)) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_PERMISSION.toString());
        }

        Optional<ActivityMembership> optionalMembership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, editedId);
        if (optionalMembership.isEmpty()) {
            throw new NoSuchElementException(ActivityResponseMessage.INVALID_MEMBERSHIP.toString());
        }

        ActivityMembership membership = optionalMembership.get();
        if (membership.getRole() == ActivityMembership.Role.CREATOR) {
            throw new IllegalArgumentException(ActivityResponseMessage.EDITING_CREATOR.toString());
        }
        NotificationType type = NotificationService.getTypeForAddingRole(newRole);
        String message;
        String template;
        Activity activity = getModelObjectById(activityRepo, activityId);
        Profile editor = getModelObjectById(profileRepo, editorId);
        Profile edited = getModelObjectById(profileRepo, editedId);
        String roleName = StringUtils.capitalize(newRole.toString().toLowerCase());
        if (editedId == editorId) {
            template = "%s changed their role in %s to %s.";
        } else {
            template = "%s had their role in %s changed to %s.";
        }
        message = String.format(template, edited.getFirstAndLastName(), activity.getActivityName(), roleName);

        membership.setRole(newRole);
        membershipRepo.save(membership);
        notificationService.createNotification(type, activity, editor, message);
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
     * Converts a list of normal activities into a list of simplified activities
     *
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
     * Users should only be able to change someone to an organiser if they are a creator/organiser or admin
     * Users should be able to change someone else to a participant or follower if they are creator/organiser, admin, or it is themself
     *
     * @param profileDoingEditingId the ID of the profile who is editing the activities membership
     * @param activityId            The ID of the activity we are editing
     * @param newRole               The role we are trying to change the user to
     * @param profileBeingEditedId  The ID of the user who is attempting to change the role
     * @return whether the user doing the editing can change the profiles membership to organiser
     */
    private boolean canChangeRole(long profileDoingEditingId, long profileBeingEditedId, long activityId, ActivityMembership.Role newRole) {
        boolean isCreatorOrOrganiser = false;
        boolean isAdmin = profileRepo.getOne(profileDoingEditingId).getAuthLevel() < 2;
        Optional<ActivityMembership> membership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileDoingEditingId);
        if (membership.isPresent()) {
            ActivityMembership.Role editorRole = membership.get().getRole();
            if (editorRole.equals(ActivityMembership.Role.CREATOR) || editorRole.equals(ActivityMembership.Role.ORGANISER)) {
                isCreatorOrOrganiser = true;
            }
        }
        if (ActivityMembership.Role.ORGANISER.equals(newRole)) {
            return isAdmin || isCreatorOrOrganiser;
        } else {
            return isAdmin || isCreatorOrOrganiser || profileBeingEditedId == profileDoingEditingId;
        }
    }

    /**
     * Adds the members to the activity under the given roles. If the member is already a role, their role is changed.
     *
     * @param members    list of MembersRequest objects which contain member emails and their associated roles.
     * @param activityId id referring to the activity.
     * @throws IllegalArgumentException when activity or email is not found.
     */
    public void addMembers(List<MembersRequest> members, long activityId) {
        Optional<Activity> optionalActivity = activityRepo.findById(activityId);
        if (optionalActivity.isEmpty()) {
            throw new IllegalArgumentException(ActivityMessage.ACTIVITY_NOT_FOUND.getMessage());
        }
        Map<Profile, String> profiles = new HashMap<>();
        for (MembersRequest member : members) {
            List<Profile> profilesWithEmail = profileRepo.findByPrimaryEmail(member.getEmail());
            if (!profilesWithEmail.isEmpty()) {
                profiles.put(profilesWithEmail.get(0), member.getRole());
            } else {
                throw new IllegalArgumentException(ActivityResponseMessage.INVALID_EMAILS.toString());
            }
        }
        Activity activity = optionalActivity.get();
        for (Map.Entry<Profile, String> entry : profiles.entrySet()) {
            Profile profile = entry.getKey();
            String roleName = entry.getValue();
            Optional<ActivityMembership> optionalMembership = membershipRepo.findByActivity_IdAndProfile_Id(profile.getId(), activityId);
            ActivityMembership.Role role = ActivityMembership.Role.valueOf(roleName.toUpperCase());
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
     * Saves the given participation details of a user who participated in a specific activity to the repository and
     * creates notification of this new participation result.
     *
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
        notificationService.createNotification(NotificationType.PARTICIPANT_CREATED, activity, profile,
                profile.getFullName() + " added participation results to an activity called " + activity.getActivityName() + ".\n" +
                        "Outcome: " + participation.getOutcome() + "\nDetails: " + participation.getDetails());
        activityRepo.save(activity);
    }

    /**
     * Updates the fields of an existing participation and creates notification that participation result was edited.
     *
     * @param activityId      The ID of the activity
     * @param profileId       The ID of the profile
     * @param participationId The ID of the participation being changed
     * @param participation   An ActivityParticipation object which contains the new fields to be changed
     * @throws IllegalArgumentException if a participation does not exist
     */
    public void editParticipation(long activityId, long profileId, long participationId, ActivityParticipation participation) {
        checkParticipationHelper(activityId, profileId);
        Optional<ActivityParticipation> participationResult = participationRepo.findById(participationId);
        if (participationResult.isEmpty()) {
            throw new IllegalArgumentException(ActivityParticipationMessage.PARTICIPATION_NOT_FOUND.toString());
        }
        Profile profile = profileRepo.getOne(profileId);
        Activity activity = activityRepo.getOne(activityId);
        ActivityParticipation dbParticipation = participationResult.get();
        dbParticipation.updateActivityParticipation(participation);
        participationRepo.save(dbParticipation);
        notificationService.createNotification(NotificationType.PARTICIPATION_EDITED, activity, profile,
                profile.getFullName() + " edited participation results of activity called " + activity.getActivityName() + ".\n" +
                        "Outcome:" + participation.getOutcome() + "\nDetails: " + participation.getDetails());
    }

    /**
     * Checks if an activity and profile exists
     *
     * @param activityId The ID of the activity
     * @param profileId  The ID of the profile
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

    /**
     * Checks if a participation exists
     *
     * @param participationId the ID of the participation
     * @throws IllegalArgumentException If no such participation exists
     */
    public void checkParticipationExists(long participationId) {
        Optional<ActivityParticipation> participationResult = participationRepo.findById(participationId);
        if (participationResult.isEmpty()) {
            throw new IllegalArgumentException(ActivityParticipationMessage.PARTICIPATION_NOT_FOUND.toString());
        }

    }

    /**
     * Deletes the participation of a user in a particular activity
     *
     * @param activityId      The ID of the activity
     * @param profileId       The ID of the profile
     * @param participationId The ID of the participation being deleted
     * @throws IllegalArgumentException if a participation does not exist
     */
    public boolean removeParticipation(long activityId, long profileId, long participationId) {
        checkParticipationHelper(activityId, profileId);
        checkParticipationExists(participationId);
        ActivityParticipation participation = participationRepo.getOne(participationId);
        Profile profile = participation.getProfile();
        Activity activity = participation.getActivity();
        if (activity.getId() == activityId && profile.getId() == profileId) {
            participationRepo.delete(participation);
            profile.removeParticipation(participation);
            activity.removeParticipation(participation);
            return true;
        }
        return false;
    }

    /**
     * Checks the database to see if a participation with the given ID exists, returns the activity if it exists and
     * throws an error otherwise.
     *
     * @param participationId the id of the participation
     * @return the participation object.
     */
    public ActivityParticipation readParticipation(Long participationId) {
        Optional<ActivityParticipation> optionalActivityParticipation = participationRepo.findById(participationId);
        if (optionalActivityParticipation.isEmpty()) {
            throw new IllegalArgumentException(ActivityParticipationMessage.PARTICIPATION_NOT_FOUND.toString());
        }
        return optionalActivityParticipation.get();
    }

    /**
     * Retrieves a list of all participations for the given activity.
     *
     * @param activityId The ID of the activity.
     * @return A list of that activity's participations.
     * @throws IllegalArgumentException if no activity with that ID exists.
     */
    public List<ActivityParticipation> readParticipationsFromActivity(long activityId) {
        if (!activityRepo.existsById(activityId)) {
            throw new IllegalArgumentException(ActivityResponseMessage.INVALID_ACTIVITY.toString());
        }
        return participationRepo.findAllByActivityId(activityId);
    }


    /**
     * Method to clear all memberships that have a specified role
     *
     * @param activityId  the ID of the activity being cleared
     * @param roleToClear the ENUM String of the role to clear
     */
    public void clearActivityRoleList(Long editorID, Long activityId, String roleToClear) {
        List<ActivityMemberProfileResponse> memberships = getActivityMembers(activityId);
        for (ActivityMemberProfileResponse membership : memberships) {
            if (membership.getRole().name().equals(roleToClear)) {
                removeUserRoleFromActivity(editorID, membership.getId(), activityId);
            }
        }
    }

    /**
     * Gets a users role in an activity if it exists
     *
     * @param profileId  the id of the profile
     * @param activityId the id of the activity
     * @return the role of the user in the activity if they have one
     */
    public String getSingleActivityMembership(Long profileId, Long activityId) {
        Optional<ActivityMembership> optionalActivityMembership = membershipRepo.findByActivity_IdAndProfile_Id(activityId, profileId);
        if (optionalActivityMembership.isEmpty()) {
            throw new NoSuchElementException();
        }
        return optionalActivityMembership.get().getRole().name().toLowerCase();
    }

    /**
     * Wrapper method for getting a generic object from a repository.
     * @param repository The repository the object is being retrieved from.
     * @param id         The id of the object.
     * @param <T>        The type of the object.
     * @return the object from the repository with the given ID.
     * @throws NoSuchElementException if no object with that ID exists in the repository.
     */
    private <T> T getModelObjectById(JpaRepository<T, Long> repository, Long id) {
        Optional<T> optional = repository.findById(id);
        if (optional.isPresent()) {
            return optional.get();
        } else {
            throw new NoSuchElementException();
        }
    }
}

