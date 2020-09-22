package com.springvuegradle.service;

import com.springvuegradle.dto.responses.ActivityLocationResponse;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.ActivityType;
import com.springvuegradle.repositories.ActivityRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Service-layer class that provides methods for searching activities.
 */
@Service
public class ActivitySearchService {

    @Autowired
    ActivityService activityService;

    @Autowired
    ActivityRepository activityRepository;

    @Autowired
    ProfileService profileService;


    static final int MINIMUM_LATITUDE = -90;
    static final int MINIMUM_LONGITUDE = -90;
    static final int MAXIMUM_LATITUDE = 90;
    static final int MAXIMUM_LONGITUDE = 180;


    /**
     * Returns a list of simplified activities which are both within the specified distance
     * of the specified point AND visible to the user
     *
     * @param isAdmin         whether the user searching is an admin
     * @param profileId       the ID of the user who is searching
     * @param maximumDistance activities only within this distance(specified in m) will be returned
     * @param latitude        the latitude we are searching within a distance of
     * @param longitude       the longitude we are searching within a distance of
     * @param activityTypes   A list of activity types that the resulting activities must contain
     * @param activityTypeSearchMethod All if all activities are required, Any if any of the activities in the list are required. Null if no
     *                                 activity type searching is required.
     * @return a list of simplified activities that are visible to the user and are within the required range
     */
    public List<ActivityLocationResponse> getActivitiesByRangeAndActivityTypes(Long profileId, boolean isAdmin, int maximumDistance, Double latitude, Double longitude, String[] activityTypes, String activityTypeSearchMethod) {
        List<ActivityType> activityTypeList = activityService.getActivityTypesFromStringArray(activityTypes);
        List<Activity> activities = getVisibleActivities(profileId, isAdmin);
        activities = filterActivitiesByDistance(activities, latitude, longitude, maximumDistance);
        activities = activityService.filterActivitiesByActivityTypes(activities, activityTypeList, activityTypeSearchMethod);
        return ActivityService.createActivityLocationResponse(activities);
    }

    /**
     * Gets a list of activities that are visible to a user
     * @param profileId the ID of the users profile
     * @param isAdmin whether the user is an admin or not
     * @return a list of activities that are visible to a user
     */
    public List<Activity> getVisibleActivities(Long profileId, boolean isAdmin){
        if (isAdmin) {
            return activityService.getAllActivities();
        } else {
           return activityRepository.findActivitiesUserCanSee(profileId, ActivityMembership.Role.CREATOR);
        }
    }


    /**
     * Filters a list of activities so only ones within a certain distance of a point are returned
     * @param activities a list of activities
     * @param latitude the latitude of the point at the center of the search
     * @param longitude the longitude of the point at the center of the search
     * @param maximumDistance the maximum distance away from the center activities can be
     * @return  list of activities so only ones within a certain distance of a point
     */
    public List<Activity> filterActivitiesByDistance(List<Activity> activities, Double latitude, Double longitude, Integer maximumDistance){
        if (!(isInRange(latitude, MINIMUM_LATITUDE, MAXIMUM_LATITUDE) && isInRange(longitude, MINIMUM_LONGITUDE, MAXIMUM_LONGITUDE))) {
            throw new IllegalArgumentException("Invalid location specified!");
        }
        HashMap<Activity, Double> activityDistanceHashMap = new HashMap<>();
        for (Activity activity : activities) {
            Double activityDistanceFromPoint = distance(latitude, activity.getLatitude(), longitude, activity.getLongitude());
            if (activityDistanceFromPoint < maximumDistance) {
                activityDistanceHashMap.put(activity, activityDistanceFromPoint);
            }
        }
        return sortActivitiesByDistance(activityDistanceHashMap);
    }


    /**
     * Takes a map of activity distance and returns activities in order of distance increasing
     * @param activityDistanceHashMap a hashmap of activities and their distance from the search point.
     * @return a list of activities sorted by distance from the center of the search
     */
    private List<Activity> sortActivitiesByDistance(HashMap<Activity, Double> activityDistanceHashMap) {
        List<Map.Entry<Activity, Double>> list = new LinkedList<>(activityDistanceHashMap.entrySet());
        Collections.sort(list, Comparator.comparing(Map.Entry::getValue));
        ArrayList<Activity> sortedActivities = new ArrayList<>();
        for (Map.Entry<Activity, Double> aa : list) {
            sortedActivities.add(aa.getKey());
        }
        return sortedActivities;
    }

    /**
     * Method based on https://stackoverflow.com/questions/3694380/calculating-distance-between-two-points-using-latitude-longitude
     * Calculate distance between two points in latitude and longitude without
     * taking into account height difference
     *
     * @param lat1 the latitude of the first location
     * @param lat2 the latitude of the second location
     * @param lon1 the longitude of the first location
     * @param lon2 the longitude of the second location
     * @return Distance in Meters between the two coordinates
     */
    public double distance(double lat1, double lat2, double lon1,
                           double lon2) {

        final int R = 6371; // Radius of the earth

        double latDistance = Math.toRadians(lat2 - lat1);
        double lonDistance = Math.toRadians(lon2 - lon1);
        double a = Math.sin(latDistance / 2) * Math.sin(latDistance / 2)
                + Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2))
                * Math.sin(lonDistance / 2) * Math.sin(lonDistance / 2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        double distance = R * c * 1000; // convert to meters

        distance = Math.pow(distance, 2);

        return Math.sqrt(distance);
    }

    /**
     * Ensures a value is in between a range and not null
     * @param number the longitude to be checked
     * @param low    the number should be greater than or equal to this
     * @param high the number should be less than or equal to this
     * @return true if the longitude is valid
     */
    public boolean isInRange(Double number, int low, int high) {
        if (number == null) {
            return false;
        }
        return number >= low && number <= high;
    }
}
