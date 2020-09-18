package com.springvuegradle.controller;

import com.springvuegradle.dto.responses.ActivityLocationResponse;
import com.springvuegradle.model.ActivityType;
import com.springvuegradle.service.ActivitySearchService;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.service.SecurityService;
import com.springvuegradle.utilities.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class ActivitySearchController {

    @Autowired
    private JwtUtil jwtUtil;

    @Autowired
    private SecurityService securityService;

    @Autowired
    private ActivityService activityService;

    @Autowired
    private ActivitySearchService activitySearchService;


    /**
     * Gets all activities in a given range that are visible to a user
     * @param token the users authentication token
     * @param distance the distance in metres the user wishes to see activities within
     * @param latitude the latitude of the location of the search center
     * @param longitude the longitude of the location of the search center
     * @return a list of activities that are within a specific range of a location and visible to the user
     */
    @GetMapping("activities/distance")
    public ResponseEntity<List<ActivityLocationResponse>> getActivitiesInRange(@RequestParam Integer distance,
                                                                               @RequestParam Double latitude,
                                                                               @RequestParam Double longitude,
                                                                               @RequestParam(required = false) String[] activityTypes,
                                                                               @RequestParam(required = false) String searchMethod,
                                                                               @RequestHeader("authorization") String token) {
        if (!jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
        boolean isAdmin = jwtUtil.extractPermission(token) < 2;
        Long profileId = jwtUtil.extractId(token);
        try {
            List<ActivityType> activityTypeList = activityService.getActivityTypesFromStringArray(activityTypes);
            List<ActivityLocationResponse> activities = activitySearchService.getActivitiesInRange(profileId, isAdmin, distance, latitude, longitude, activityTypeList, searchMethod);
            return new ResponseEntity<>(activities, HttpStatus.OK);
        } catch(IllegalArgumentException e){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
    }
}
