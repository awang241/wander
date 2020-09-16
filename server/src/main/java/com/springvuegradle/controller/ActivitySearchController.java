package com.springvuegradle.controller;

import com.springvuegradle.dto.SimplifiedActivity;
import com.springvuegradle.dto.requests.ActivityRangeRequest;
import com.springvuegradle.service.ActivitySearchService;
import com.springvuegradle.service.SecurityService;
import com.springvuegradle.utilities.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class ActivitySearchController {

    @Autowired
    private JwtUtil jwtUtil;

    @Autowired
    private SecurityService securityService;

    @Autowired
    private ActivitySearchService activitySearchService;


    /**
     * Gets all activities in a given range that are visible to a user
     * @param token the users authentication token
     * @param request contains the latitude longitude coordinates of a point and from this point in which
     *               activities should be returned
     * @return a list of activities that are within a specific range of a location and visible to the user
     */
    @GetMapping("activities/range")
    public ResponseEntity<List<SimplifiedActivity>> getActivitiesInRange(@RequestBody ActivityRangeRequest request,
                                                                         @RequestHeader("authorization") String token) {
        if (!jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
        boolean isAdmin = jwtUtil.extractPermission(token) < 2;
        Double latitude = request.getLatitude();
        Double longitude = request.getLongitude();
        int distance = request.getDistance();
        Long profileId = jwtUtil.extractId(token);
        try {
            List<SimplifiedActivity> activities = activitySearchService.getActivitiesInRange(profileId, isAdmin, distance, latitude, longitude);
            return new ResponseEntity<>(activities, HttpStatus.OK);
        } catch(IllegalArgumentException e){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }


    }

}
