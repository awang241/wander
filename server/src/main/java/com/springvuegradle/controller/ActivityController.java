package com.springvuegradle.controller;


import com.springvuegradle.dto.*;
import com.springvuegradle.enums.*;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.utilities.FieldValidationHelper;
import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.service.SecurityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

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
            HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
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
    public ResponseEntity<List<Activity>> getActivities(@RequestParam String privacyString,
                                                        @RequestHeader("authorization") String token ) {
        if (Boolean.TRUE.equals(FieldValidationHelper.isNullOrEmpty(privacyString))) {
            return new ResponseEntity<>(activityService.getAllActivities(), HttpStatus.OK);
        } else {
            if ((token == null || token.isBlank())) {
                return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
            }

            ActivityPrivacy privacy;
            try {
                privacy = ActivityPrivacy.valueOf(privacyString.toUpperCase());
            } catch (IllegalArgumentException e) {
                return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
            }

            List<Activity> publicActivityList = activityService.getActivitiesWithPrivacyLevel(privacy);
            return new ResponseEntity<>(publicActivityList, HttpStatus.OK);
        }
    }

    /**
     * REST endpoint to return the activity with the given ID
     * @param token authentication token
     * @param activityId the id of the activity
     * @return A response containing the requested activity if successful, or a empty response with the appropriate
     * error code otherwise.
     */
    @GetMapping("/activities/{activityId}")
    public ResponseEntity<Activity> getActivity(@RequestHeader("authorization") String token,
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
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
        try {
            return new ResponseEntity<>(activityService.getRoleCounts(activityId), HttpStatus.OK);
        } catch(IllegalArgumentException e){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
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
     * Queries the Database to find all the activities of a user with their profile id.
     * @param token      the users authentication token
     * @param profileId  the users ID
     * @param count      an integer for the amount of activities to be returned by the database
     * @param startIndex an integer for the starting index of activities to search from
     * @param role       the role of the user. Used to filter the activities
     * @return a response with all the activities of the user in the database.
     */
    @GetMapping("/profiles/{profileId}/activities")
    public ResponseEntity<SimplifiedActivitiesResponse> getAllUsersActivities(@RequestHeader("authorization") String token,
                                                                              @PathVariable Long profileId,
                                                                              @RequestParam("count") int count,
                                                                              @RequestParam("startIndex") int startIndex,
                                                                              @RequestParam("role") String role) {
        SimplifiedActivitiesResponse activitiesResponse = null;
        HttpStatus status = null;
        if (token == null) {
            status = HttpStatus.UNAUTHORIZED;
            activitiesResponse = new SimplifiedActivitiesResponse(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage());
        } else if (Boolean.FALSE.equals(jwtUtil.validateToken(token))) {
            status = HttpStatus.FORBIDDEN;
            activitiesResponse = new SimplifiedActivitiesResponse(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage());
        } else if (count <= 0) {
            status = HttpStatus.BAD_REQUEST;
            activitiesResponse = new SimplifiedActivitiesResponse(ProfileErrorMessage.INVALID_SEARCH_COUNT.getMessage());
        } else {
            int pageIndex = startIndex / count;
            PageRequest request = PageRequest.of(pageIndex, count);
            try {
                List<Activity> activityRoleMap = activityService.getActivitiesByProfileIdByRole(request, profileId, ActivityMembership.Role.valueOf(role.toUpperCase()));
                List<SimplifiedActivity> simplifiedActivities = activityService.createSimplifiedActivities(activityRoleMap);
                activitiesResponse = new SimplifiedActivitiesResponse(simplifiedActivities);
                status = HttpStatus.OK;
            } catch (IllegalArgumentException e) {

            }

        }
        return new ResponseEntity<>(activitiesResponse, status);
    }

//    /**
//     * Queries the Database to find all the activities of a user with their profile id.
//     *
//     * @return a response with all the activities of the user in the database.
//     */
//    @GetMapping("/profiles/{profileId}/activities")
//    public ResponseEntity<SimplifiedActivitiesResponse> getAllUsersActivities(@RequestHeader("authorization") String token,
//                                                                @PathVariable Long profileId,
//                                                                @RequestParam("count") int count,
//                                                                @RequestParam("startIndex") int startIndex,
//                                                                @RequestParam("role") String role) {
//
//        return getAllUsersActivities(token, profileId, count, startIndex, role, false);
//    }
//
//
//    public ResponseEntity<SimplifiedActivitiesResponse> getAllUsersActivities(String token, Long profileId,
//            int count, int startIndex, String role, Boolean testing) {
//        SimplifiedActivitiesResponse activitiesResponse = null;
//        HttpStatus status = null;
//        if (token == null && !testing) {
//            status = HttpStatus.UNAUTHORIZED;
//            activitiesResponse = new SimplifiedActivitiesResponse(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage());
//        } else if (!testing && Boolean.FALSE.equals(jwtUtil.validateToken(token))) {
//            status = HttpStatus.FORBIDDEN;
//            activitiesResponse = new SimplifiedActivitiesResponse(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage());
//        } else if (count <= 0) {
//            status = HttpStatus.BAD_REQUEST;
//            activitiesResponse = new SimplifiedActivitiesResponse(ProfileErrorMessage.INVALID_SEARCH_COUNT.getMessage());
//        } else {
//            int pageIndex = startIndex / count;
//            PageRequest request = PageRequest.of(pageIndex, count);
//            try {
//                List<Activity> activityRoleMap = activityService.getActivitiesByProfileIdByRole(request, profileId, ActivityMembership.Role.valueOf(role.toUpperCase()));
//                List<SimplifiedActivity> simplifiedActivities = activityService.createSimplifiedActivities(activityRoleMap);
//                activitiesResponse = new SimplifiedActivitiesResponse(simplifiedActivities);
//                status = HttpStatus.OK;
//            } catch (IllegalArgumentException e) {
//
//            }
//
//        }
//        return new ResponseEntity<>(activitiesResponse, status);
//    }


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
        List<String> possibleRoles = new ArrayList<>(Arrays.asList("participant", "follower"));
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
        if (token == null || token.isBlank()) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                    HttpStatus.UNAUTHORIZED);
        } else if (!securityService.checkEditPermission(token, profileId)) {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(),
                    HttpStatus.FORBIDDEN);
        }
        if (activityService.removeMembership(profileId, activityId)) {
            return new ResponseEntity<>(ActivityMessage.SUCCESSFUL_DELETION.getMessage(), HttpStatus.OK);
        }
        return new ResponseEntity<>(ActivityMessage.MEMBERSHIP_NOT_FOUND.getMessage(), HttpStatus.NOT_FOUND);
    }


    /**
     * Removes all member roles of a specified level from a specified activity
     * @param token the auth token of the request
     * @param activityId The ID of the activity to have a role cleared
     * @param roleToClear A string of "organizer", "participant" or "follower" to clear
     * @return http response code for the outcome of the delete
     */
    @DeleteMapping("/activities/{activityId}/clearRole")
    public @ResponseBody
    ResponseEntity<String> clearRoleOfActivity(@RequestHeader("authorization") String token,
                                               @PathVariable Long activityId,
                                               @RequestBody ActivityRoleUpdateRequest roleToClear){
        ResponseEntity response = null;
        if (token == null || token.isBlank()) {
            response = new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                    HttpStatus.UNAUTHORIZED);
        } else if (!jwtUtil.validateToken(token)){
            response = new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                    HttpStatus.UNAUTHORIZED);
        }
        //TODO: Check Whether the user token is admin or activity owner here *in a way that can be mocked*
        if (response == null) {
            String roleString = roleToClear.getRole().toUpperCase();
            try{
                Arrays.asList(ActivityRoleLevel.values()).contains(ActivityRoleLevel.valueOf(roleString));
                Boolean success = activityService.clearActivityRoleList(activityId, roleString);
                if (success) {
                    response = new ResponseEntity<>(ActivityMessage.SUCCESSFUL_DELETION.getMessage(), HttpStatus.OK);
                } else {
                    response = new ResponseEntity<>(ActivityMessage.UNSUCCESSFUL, HttpStatus.NOT_MODIFIED);
                }
            } catch (IllegalArgumentException e) {
                response =  new ResponseEntity<>(HttpStatus.BAD_REQUEST);
            }
        }
        return response;
    }

    /**
     * REST endpoint for editing the privacy level of an existing activity. Given a HTTP request containing a correctly formatted JSON file,
     * updates the given database entry. For more information on the JSON format, see the @JsonCreator-tagged constructor
     * in the Activity class.
     * @param privacy The contents of HTTP request body, automatically mapped from a JSON file to an activity.
     * @return A HTTP response notifying the sender whether the edit was successful
     */
    @PutMapping("/profiles/{profileId}/activities/{activityId}/privacy")
    public ResponseEntity<String> editActivityPrivacy(@RequestBody PrivacyRequest privacyRequest,
                                                      @RequestHeader("authorization") String token,
                                                      @PathVariable Long profileId,
                                                      @PathVariable Long activityId) {

        if (token == null || token.isBlank() || !activityService.isProfileActivityCreator(jwtUtil.extractId(token), activityId)) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(), HttpStatus.UNAUTHORIZED);
        }
        try {
            activityService.editActivityPrivacy(privacyRequest.getPrivacy(), activityId);
            return new ResponseEntity<>(ActivityResponseMessage.EDIT_SUCCESS.toString(), HttpStatus.OK);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
