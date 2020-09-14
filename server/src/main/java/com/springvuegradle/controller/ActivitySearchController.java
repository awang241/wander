package com.springvuegradle.controller;

import com.springvuegradle.dto.SimplifiedActivity;
import com.springvuegradle.dto.requests.ActivityRangeRequest;
import com.springvuegradle.service.ActivitySearchService;
import com.springvuegradle.service.SecurityService;
import com.springvuegradle.utilities.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
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
     * Gets all activities in a given range
     * @param request
     * @return a list of activities that are within a specific range of a location and visible to the user
     */
    @GetMapping("activities/range")
    public List<SimplifiedActivity> getActivitiesInRange(ActivityRangeRequest request){
        Double latitude = request.getLatitude();
        Double longitude = request.getLongitude();
        int distance = request.getDistance();
        return activitySearchService.getActivitiesInRange(distance, latitude, longitude);
    }
}
