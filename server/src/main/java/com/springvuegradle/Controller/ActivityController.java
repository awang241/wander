package com.springvuegradle.Controller;


import com.springvuegradle.Controller.enums.ActivityResponseMessage;
import com.springvuegradle.Controller.enums.AuthenticationErrorMessage;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Utilities.FieldValidationHelper;
import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.service.ActivityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Class containing REST endpoints for activities
 */
@RestController
public class ActivityController {

    @Autowired
    private JwtUtil jwtUtil;

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
     * Way to access Activity Repository (Activity table in db).
     */


    private boolean checkEditPermission(String token, Long id) {
        if (jwtUtil.validateToken(token) && (jwtUtil.extractPermission(token) == 0 || jwtUtil.extractPermission(token) == 1 || (jwtUtil.extractId(token).equals(id)))) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Endpoint for creating activities.
     * Creates a new Activity object given a set of JSON data and saves the new data to the database.
     * @param newActivity contains data relating to the activity to add to the database.
     * @param id referring to the profile
     * @return the created activity and/or status code.
     */
    @PostMapping("/profiles/{id}/activities")
    public ResponseEntity<String> createActivity (@RequestHeader("authorization") String token,
                                                  @RequestBody Activity newActivity,
                                                  @PathVariable Long id) {
        return createActivity(token, newActivity, id, false);
    }

    public ResponseEntity<String> createActivity(String token, Activity newActivity, Long id, Boolean testing) {
        if (!testing) {
            if (token == null || token.isBlank()) {
                return new ResponseEntity<>("Authorization required", HttpStatus.UNAUTHORIZED);
            } else if (!checkEditPermission(token, id)) {
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
     * @param request The contents of HTTP request body, automatically mapped from a JSON file to an activity.
     * @return A HTTP response notifying the sender whether the edit was successful
     */
    @PutMapping("/profiles/{profileId}/activity/{activityId}")
    public ResponseEntity<String> updateActivity(@RequestBody Activity request,
                                                 @RequestHeader("authorization") String token,
                                                 @PathVariable Long profileId,
                                                 @PathVariable Long activityId) {
        if (token == null || token.isBlank()) {
            return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                                        HttpStatus.UNAUTHORIZED);
        } else if (!checkEditPermission(token, profileId)) {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(),
                    HttpStatus.FORBIDDEN);
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
     * Queries the Database to find all the activities.
     * @return a response with all the activities in the database.
     */
    @GetMapping("/activities")
    public ResponseEntity<List<Activity>> getActivitiesList() {
        List<Activity> allActivities = aRepo.findAll();
        return new ResponseEntity<>(allActivities, HttpStatus.OK);
    }


    /**
     * Queries the Database to find all the activities of a user with their profile id.
     * @return a response with all the activities of the user in the database.
     */
    @GetMapping("/profiles/{profileId}/activities")
    public ResponseEntity<List<Activity>> getAllUsersActivities(@RequestHeader("authorization") String token,
                                                                @PathVariable Long profileId) {
        if (!jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
        }
        return getAllUsersActivities(profileId);
    }

    public ResponseEntity<List<Activity>> getAllUsersActivities(Long profileId) {
        List<Activity> allUserActivities = activityService.getActivitiesByProfileId(profileId);
        return new ResponseEntity<>(allUserActivities, HttpStatus.OK);
    }

    /**
     * Deletes an activity from the repository given that it exists in the database.
     * @param profileId the id of the profile that created the activity
     * @param activityId the id of the activity to be deleted
     * @return http response code and feedback message on the result of the delete operation
     */
    @DeleteMapping("/profiles/{profileId}/activities/{activityId}")
    public @ResponseBody ResponseEntity<String> deleteActivity(@RequestHeader("authorization") String token,
                                                               @PathVariable Long profileId,
                                                               @PathVariable Long activityId) {
        return deleteActivity(token, profileId, activityId, false);

    }
    public ResponseEntity<String> deleteActivity(String token, Long profileId, Long activityId, Boolean testing) {
        if (!testing) {
            if (token == null || token.isBlank()) {
                return new ResponseEntity<>(AuthenticationErrorMessage.AUTHENTICATION_REQUIRED.getMessage(),
                        HttpStatus.UNAUTHORIZED);
            } else if (!checkEditPermission(token, profileId)) {
                return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(),
                        HttpStatus.FORBIDDEN);
            }
        }
        Optional<Activity> result = aRepo.findById(activityId);
        if (result.isPresent()) {
            Activity activityToDelete = result.get();
            //activityService.delete(activityId);
            aRepo.delete(activityToDelete);
            System.out.println(aRepo.count());
            return new ResponseEntity<>("The activity has been deleted from the database.", HttpStatus.OK);
        } else {
            return new ResponseEntity<>("The activity does not exist in the database.", HttpStatus.NOT_FOUND);
        }
    }


}
