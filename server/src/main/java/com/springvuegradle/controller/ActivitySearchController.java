package com.springvuegradle.controller;

import com.springvuegradle.dto.SimplifiedActivitiesResponse;
import com.springvuegradle.dto.responses.ActivityLocationResponse;
import com.springvuegradle.service.ActivitySearchService;
import com.springvuegradle.service.ActivityService;
import com.springvuegradle.service.SecurityService;
import com.springvuegradle.utilities.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
            List<ActivityLocationResponse> activities = activitySearchService.getActivitiesByRangeAndActivityTypes(profileId, isAdmin, distance, latitude, longitude, activityTypes, searchMethod);
            return new ResponseEntity<>(activities, HttpStatus.OK);
        } catch(IllegalArgumentException e){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
    }




    /**
     * Endpoint for searching all activities by name. The request parameters will filter the results to those that match the
     * search criteria.
     *
     * The provided activity name can either be a single word or multiple space-separated words. The provided method can either be
     * "all" or "any". "All" means all the words in the searched string must match an activity's name. "Any" means any of the words
     * in the searched string could match an activity's name.
     * @param token token provided
     * @param name string pattern to be matched to profile nickname
     * @param method Whether the user is searching for a user with ALL the required activity types or any of them
     * @param count number of profiles to be returned.
     * @param startIndex index
     * @return response entity containing a list of simplified profiles and an OK status code if the request was successful;
     * otherwise an empty response with the appropriate error code is returned.
     */
    @GetMapping("/activities")
    public @ResponseBody
    ResponseEntity<List<SimplifiedActivitiesResponse>> getActivitiesByName(
            @RequestParam(name = "name", required = true) String name,
            @RequestParam(name = "method", required = false) String method,
            @RequestParam(name = "count") int count,
            @RequestParam(name = "startIndex") int startIndex,
            @RequestHeader("authorization") String token) {
        if (!jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        } else if (count <= 0) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        int pageIndex = startIndex / count;
        PageRequest request = PageRequest.of(pageIndex, count);
        boolean isAdmin = jwtUtil.extractPermission(token) < 2;
        Long profileId = jwtUtil.extractId(token);
        try {
            List<SimplifiedActivitiesResponse> activities = activitySearchService.getActivitiesByName(name, profileId, isAdmin, method, request);
            return new ResponseEntity<>(activities, HttpStatus.OK);
        } catch(IllegalArgumentException e){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
    }



}
