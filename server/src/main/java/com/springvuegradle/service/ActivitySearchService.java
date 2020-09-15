package com.springvuegradle.service;

import com.springvuegradle.dto.SimplifiedActivity;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.repositories.ActivityRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class ActivitySearchService {

    @Autowired
    ActivityService activityService;

    @Autowired
    ActivityRepository activityRepository;

    @Autowired
    ProfileService profileService;


    /**
     * Returns a list of simplified activities which are both within the specified distance
     * of the specified point AND visible to the user
     * @param isAdmin whether the user searching is an admin
     * @param profileId the ID of the user who is searching
     * @param maximumDistance activities only within this distance(specified in m) will be returned
     * @param latitude the latitude we are searching within a distance of
     * @param longitude the longitude we are searching within a distance of
     * @return a list of simplified activities that are visible to the user and are within the required range
     */
    public List<SimplifiedActivity> getActivitiesInRange(Long profileId, boolean isAdmin, int maximumDistance, Double latitude, Double longitude) {
        if (!(isValidLatitude(latitude) && isValidLongitude(longitude))) {
            throw new IllegalArgumentException("Invalid latitude and longitude");
        }
        List<Activity> visibleActivities;
        if (isAdmin) {
            visibleActivities = activityService.getAllActivities();
        } else {
            visibleActivities = activityRepository.findActivitiesUserCanSee(profileId, ActivityMembership.Role.CREATOR);
        }
        List<Activity> activitiesInRange = new ArrayList<>();
        for (Activity activity : visibleActivities) {
            Double activityDistanceFromPoint = distance(latitude, activity.getLatitude(), longitude, activity.getLongitude());
            if (activityDistanceFromPoint < maximumDistance) {
                activitiesInRange.add(activity);
            }
        }
        return activityService.createSimplifiedActivities(activitiesInRange);
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
    public static double distance(double lat1, double lat2, double lon1,
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
     * Ensures a latidude is valid
     *
     * @param latitude the latitude to be checked
     * @return true if the latitude is valid
     */
    private static boolean isValidLatitude(Double latitude) {
        if (latitude == null) {
            return false;
        }
        return latitude >= -90 && latitude <= 90;
    }

    /**
     * Ensures a longitude is valid
     *
     * @param longitude the longitude to be checked
     * @return true if the longitude is valid
     */
    private static boolean isValidLongitude(Double longitude) {
        if (longitude == null) {
            return false;
        }
        return longitude >= -90 && longitude <= 180;
    }
}
