package com.springvuegradle.controller;


import com.springvuegradle.enums.ActivityResponseMessage;
import com.springvuegradle.enums.AuthenticationErrorMessage;
import com.springvuegradle.model.Activity;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.utilities.FieldValidationHelper;
import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.service.SecurityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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
     * @param token authentication token
     * @param profileId The id of the user
     * @param activityId The id the activity
     * @return response entity with error message if unauthorised or forbidden. Null otherwise.
     */
    public ResponseEntity<String> checkActivityModifyPermissions(String token, Long profileId, Long activityId) {
        if (token == null || token.isBlank()) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                    HttpStatus.UNAUTHORIZED);
        } else if (!securityService.checkEditPermission(token, profileId) || !activityService.checkActivityCreator(jwtUtil.extractId(token), activityId)) {
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
}
