package com.springvuegradle.Controller;


import com.springvuegradle.Model.Activity;
import com.springvuegradle.Repositories.ActivityRepository;
import com.springvuegradle.Utilities.FieldValidationHelper;
import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.service.ActivityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Class containing REST endpoints for activities
 */
@RestController
public class ActivityController {

    private ActivityService activityService;
    private JwtUtil jwtUtil;

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
    public ResponseEntity<String> createActivity (@RequestBody Activity newActivity, @PathVariable Long id) {
        String error = FieldValidationHelper.validateActivity(newActivity);
        if (error.equals("")) {
            aRepo.save(newActivity);
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
        return null;
    }

}
