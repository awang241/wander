package com.springvuegradle.controller;


import com.springvuegradle.dto.ActivityRoleUpdateRequest;
import com.springvuegradle.dto.ActivitiesResponse;
import com.springvuegradle.dto.ActivityRoleCountResponse;
import com.springvuegradle.dto.ActivityRoleRequest;
import com.springvuegradle.dto.ProfileSearchResponse;
import com.springvuegradle.enums.ActivityMessage;
import com.springvuegradle.enums.ActivityResponseMessage;
import com.springvuegradle.enums.AuthenticationErrorMessage;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.utilities.FieldValidationHelper;
import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.service.SecurityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * Class containing REST endpoints for activities
 */
@RestController
public class ActivityController {

    @Autowired
    private JwtUtil jwtUtil;

    @Autowired
    private SecurityService securityService;

    @Autowired
    private ActivityService activityService;

    /**
     * Way to access Activity Repository (Activity table in db).
     */
    @Autowired
    private ActivityRepository aRepo;

    @Autowired
    public ActivityController(ActivityService activityService, JwtUtil jwtUtil) {
        this.activityService = activityService;
        this.jwtUtil = jwtUtil;
    }


    /**
     * Endpoint for creating activities.
     * Creates a new Activity object given a set of JSON data and saves the new data to the database.
     *
     * @param newActivity contains data relating to the activity to add to the database.
     * @param id          referring to the profile
     * @return the created activity and/or status code.
     */
    @PostMapping("/profiles/{id}/activities")
    public ResponseEntity<String> createActivity(@PathVariable Long id,
                                                 @RequestBody Activity newActivity,
                                                 @RequestHeader("authorization") String token
    ) {
        return createActivity(id, newActivity, token, false);
    }

    public ResponseEntity<String> createActivity(Long id, Activity newActivity, String token, Boolean testing) {
        if (!testing) {
            if (token == null || token.isBlank()) {
                return new ResponseEntity<>("Authorization required", HttpStatus.UNAUTHORIZED);
            } else if (!securityService.checkEditPermission(token, id)) {
                return new ResponseEntity<>("Permission denied", HttpStatus.FORBIDDEN);
            }
        }
        String error = FieldValidationHelper.validateActivity(newActivity);

        if (error.equals("")) {
            activityService.create(newActivity, id);
            return new ResponseEntity<>("New activity has been created.", HttpStatus.CREATED);
        } else {
            return new ResponseEntity<>(error, HttpStatus.FORBIDDEN);
        }
    }

    /**
     * REST endpoint for editing an existing activity. Given a HTTP request containing a correctly formatted JSON file,
     * updates the given database entry. For more information on the JSON format, see the @JsonCreator-tagged constructor
     * in the Activity class.
     *
     * @param request The contents of HTTP request body, automatically mapped from a JSON file to an activity.
     * @return A HTTP response notifying the sender whether the edit was successful
     */
    @PutMapping("/profiles/{profileId}/activities/{activityId}")
    public ResponseEntity<String> updateActivity(@RequestBody Activity request,
                                                 @RequestHeader("authorization") String token,
                                                 @PathVariable Long profileId,
                                                 @PathVariable Long activityId) {

        ResponseEntity<String> errorOccurred = checkActivityModifyPermissions(token, profileId, activityId);
        if (errorOccurred != null) {
            return errorOccurred;
        }


        try {
            activityService.update(request, activityId);
            return new ResponseEntity<>(ActivityResponseMessage.EDIT_SUCCESS.toString(), HttpStatus.OK);
        } catch (IllegalArgumentException e) {
            HttpStatus status = HttpStatus.BAD_REQUEST;
            if (ActivityResponseMessage.SEMANTIC_ERRORS.contains(e.getMessage())) {
                status = HttpStatus.FORBIDDEN;
            } else if (ActivityResponseMessage.SYNTAX_ERRORS.contains(e.getMessage())) {
                status = HttpStatus.BAD_REQUEST;
            }
            return new ResponseEntity<>(e.getMessage(), status);
        } catch (Exception e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Checks validity of token, as well as if the user can edit or delete the activity.
     *
     * @param token      authentication token
     * @param profileId  The id of the user
     * @param activityId The id the activity
     * @return response entity with error message if unauthorised or forbidden. Null otherwise.
     */
    private ResponseEntity<String> checkActivityModifyPermissions(String token, Long profileId, Long activityId) {
        if (token == null || token.isBlank()) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                    HttpStatus.UNAUTHORIZED);
        } else if (!securityService.checkEditPermission(token, profileId) || !activityService.isProfileActivityCreator(jwtUtil.extractId(token), activityId)) {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(),
                    HttpStatus.FORBIDDEN);
        }
        return null;
    }

    /**
     * Queries the Database to find all the activities.
     *
     * @return a response with all the activities in the database.
     */
    @GetMapping("/activities")
    public ResponseEntity<List<Activity>> getActivitiesList() {
        List<Activity> allActivities = aRepo.findAll();
        return new ResponseEntity<>(allActivities, HttpStatus.OK);
    }

    /**
     * Quries the Database to find an activity
     * @param token authentication token
     * @param activityId the id of the activity
     * @return
     */
    @GetMapping("/activities/{activityId}")
    protected ResponseEntity<Activity> getActivity(@RequestHeader("authorization") String token,
                                                @PathVariable long activityId) {
        if (token == null) {
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
        else if (!jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(HttpStatus.FORBIDDEN);
        }
        Activity activity = activityService.getActivityByActivityId(activityId);
        if (activity == null) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        return new ResponseEntity<>(activity, HttpStatus.OK);
    }

    /**
     * Allows the changing of a profiles role within an activity memebership
     * @param role the role the user wants to change to
     * @param token the users authentication token
     * @param profileId the ID of the profile whose membership we are changing
     * @param activityId the ID of the activity the profile is a part of
     * @return an HTTP status code indicating the result of the operation
     */
    @PutMapping("/profiles/{profileId}/activities/{activityId}/role")
    public ResponseEntity<String> changeProfilesActivityRole(@RequestBody ActivityRoleUpdateRequest role,
                                                             @RequestHeader("authorization") String token,
                                                             @PathVariable Long profileId,
                                                             @PathVariable Long activityId){
        if (token == null || token.isBlank()) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                    HttpStatus.UNAUTHORIZED);
        } else if (!securityService.checkEditPermission(token, profileId)) {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(),
                    HttpStatus.FORBIDDEN);
        }
        try {
            activityService.setProfileRole(profileId, jwtUtil.extractId(token), activityId, ActivityMembership.Role.valueOf(role.getRole().toUpperCase()));
            return new ResponseEntity<>(HttpStatus.OK);
        } catch(IllegalArgumentException e){
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Gets the number of people for each role in an activity
     * @param token the authentication token of the user
     * @param activityId the ID of the activity we are checking roles for
     * @return the count of people who have a role in an activity
     */
    @GetMapping("/activities/{activityId}/rolecount")
    public ResponseEntity<ActivityRoleCountResponse> getActivityRoleCount(@RequestHeader("authorization") String token,
                                                        @PathVariable long activityId
    ) {
        if (!jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
        }
        try {
            return new ResponseEntity<ActivityRoleCountResponse>(activityService.getRoleCounts(activityId), HttpStatus.OK);
        } catch(IllegalArgumentException e){
            return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
        }
    }


    /**
     * Queries the Database to find all the activities of a user with their profile id.
     *
     * @return a response with all the activities of the user in the database.
     */
    @GetMapping("/profiles/{profileId}/activities")
    public ResponseEntity<List<Activity>> getAllUsersActivities(@RequestHeader("authorization") String token,
                                                                @PathVariable Long profileId) {

        return getAllUsersActivities(token, profileId, false);
    }

    public ResponseEntity<List<Activity>> getAllUsersActivities(String token, Long profileId, Boolean testing) {
        if (!testing && !jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
        }
        List<Activity> result;
        if (!testing && (jwtUtil.extractPermission(token) == 0 || jwtUtil.extractPermission(token) == 1)) {
            result = aRepo.findAll();
        } else {
            result = activityService.getActivitiesByProfileId(profileId);
        }
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    /**
     * Gets all user's activities by role
     *
     * @return a response with all the activities of the user in the database by role.
     */
    // fix mapping please
    @GetMapping("/profiles/{profileId}/activities/role")
    public ResponseEntity<ActivitiesResponse> getAllUsersActivitiesByRole(@RequestBody ActivityRoleRequest activityRoleRequest,
                                                                          @RequestHeader("authorization") String token,
                                                                          @PathVariable Long profileId) {
        ActivitiesResponse activitiesResponse;
        HttpStatus status;
        if (token == null) {
            status = HttpStatus.UNAUTHORIZED;
            activitiesResponse = new ActivitiesResponse(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage());
        } else if (!jwtUtil.validateToken(token)) {
            status = HttpStatus.FORBIDDEN;
            activitiesResponse = new ActivitiesResponse(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage());
        } else {
            List<Activity> result = activityService.getActivitiesByProfileIdByRole(profileId, activityRoleRequest.getActivityRole());
            status = HttpStatus.OK;
            activitiesResponse = new ActivitiesResponse(result);
        }
        return new ResponseEntity<>(activitiesResponse, status);
    }

    /**
     * Deletes an activity from the repository given that it exists in the database.
     *
     * @param profileId  the id of the profile that created the activity
     * @param activityId the id of the activity to be deleted
     * @return http response code and feedback message on the result of the delete operation
     */
    @DeleteMapping("/profiles/{profileId}/activities/{activityId}")
    public @ResponseBody
    ResponseEntity<String> deleteActivity(@RequestHeader("authorization") String token,
                                          @PathVariable Long profileId,
                                          @PathVariable Long activityId) {
        return deleteActivity(token, profileId, activityId, false);

    }

    public ResponseEntity<String> deleteActivity(String token, Long profileId, Long activityId, Boolean testing) {
        if (!testing) {
            ResponseEntity<String> errorOccurred = checkActivityModifyPermissions(token, profileId, activityId);
            if (errorOccurred != null) {
                return errorOccurred;
            }
        }

        if (activityService.delete(activityId)) {
            return new ResponseEntity<>("The activity has been deleted from the database.", HttpStatus.OK);
        }
        return new ResponseEntity<>("The activity does not exist in the database.", HttpStatus.NOT_FOUND);
    }

    /**
     * Assigns an activityRole to a user for a specific activity.
     *
     * @param token      the user's authentication token.
     * @param profileId  the id of the user we want to assign the role to.
     * @param activityId the id of the activity we want to add the user's role to.
     * @param role       the role we want to give to the user for that specific activity.
     * @return response entity with message detailing whether it was a success or not.
     */
    @PostMapping("/profiles/{profileId}/activities/{activityId}/role")
    public ResponseEntity<String> addActivityRole(@RequestHeader("authorization") String token,
                                                  @PathVariable Long profileId,
                                                  @PathVariable Long activityId,
                                                  @RequestBody String role) {
        if (token == null || token.isBlank()) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                    HttpStatus.UNAUTHORIZED);
        }
        ArrayList possibleRoles = new ArrayList<>(Arrays.asList("participant", "follower"));
        Boolean checkFollowerOrParticipant = securityService.checkEditPermission(token, profileId) && possibleRoles.contains(role);
        possibleRoles.add("organiser");
        Boolean checkCreatorOrAdmin = activityService.isProfileActivityCreator(jwtUtil.extractId(token), activityId) && possibleRoles.contains(role);
        try {
            if (checkFollowerOrParticipant || checkCreatorOrAdmin) {
                activityService.addActivityRole(activityId, profileId, role);
            } else {
                return new ResponseEntity<>(ActivityMessage.INVALID_ROLE.getMessage(), HttpStatus.BAD_REQUEST);
            }
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return new ResponseEntity<>(ActivityMessage.SUCCESSFUL_CREATION.getMessage(), HttpStatus.CREATED);
    }

    /**
     * Removes a profiles activity membership from a specified activity
     *
     * @param profileId  the id of the profile that has membership with the activity
     * @param activityId the id of the specified activity
     * @return http response code and feedback message on the result of the delete operation
     */
    @DeleteMapping("/profiles/{profileId}/activities/{activityId}/membership")
    public @ResponseBody
    ResponseEntity<String> deleteActivityMembership(@RequestHeader("authorization") String token,
                                                    @PathVariable Long profileId,
                                                    @PathVariable Long activityId) {
        return deleteActivityMembership(token, profileId, activityId, false);

    }

    public ResponseEntity<String> deleteActivityMembership(String token, Long profileId, Long activityId, Boolean testing) {
        if (!testing) {
            if (token == null || token.isBlank()) {
                return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                        HttpStatus.UNAUTHORIZED);
            } else if (!securityService.checkEditPermission(token, profileId)) {
                return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(),
                        HttpStatus.FORBIDDEN);
            }
        }
        if (activityService.removeMembership(profileId, activityId)) {
            return new ResponseEntity<>(ActivityMessage.SUCCESSFUL_DELETION.getMessage(), HttpStatus.OK);
        }
        return new ResponseEntity<>(ActivityMessage.MEMBERSHIP_NOT_FOUND.getMessage(), HttpStatus.NOT_FOUND);
    }


    /**
     * Endpoint for getting all the activities with a given privacy level
     *
     * @param token        the user's authentication token.
     * @param privacyLevel the specified privacy level from the front end
     * @return a list of all activities with a given privacy level
     */
    @GetMapping("/activities/{privacyLevel}")
    public ResponseEntity<List<Activity>> getActivitiesWithPrivacyLevel(@RequestHeader("authorization") String token, @PathVariable String privacyLevel) {
        if (token == null || token.isBlank()) {
            return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
        }
        try {
            List<Activity> publicActivityList = activityService.getActivitiesWithPrivacyLevel(privacyLevel);
            return new ResponseEntity<>(publicActivityList, HttpStatus.OK);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * REST endpoint for editing the privacy level of an existing activity. Given a HTTP request containing a correctly formatted JSON file,
     * updates the given database entry. For more information on the JSON format, see the @JsonCreator-tagged constructor
     * in the Activity class.
     *
     * @param privacy The contents of HTTP request body, automatically mapped from a JSON file to an activity.
     * @return A HTTP response notifying the sender whether the edit was successful
     */
    @PutMapping("/profiles/{profileId}/activities/{activityId}/privacy")
    public ResponseEntity<String> editActivityPrivacy(@RequestBody String privacy,
                                                      @RequestHeader("authorization") String token,
                                                      @PathVariable Long profileId,
                                                      @PathVariable Long activityId) {

        if (token == null || token.isBlank() || !activityService.isProfileActivityCreator(jwtUtil.extractId(token), activityId)) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(), HttpStatus.UNAUTHORIZED);
        }
        try {
            activityService.editActivityPrivacy(privacy, activityId);
            return new ResponseEntity<>(ActivityResponseMessage.EDIT_SUCCESS.toString(), HttpStatus.OK);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
