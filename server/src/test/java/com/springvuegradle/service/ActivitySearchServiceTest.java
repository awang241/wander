package com.springvuegradle.service;

import com.springvuegradle.dto.responses.ActivityLocationResponse;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.ActivityMembershipRepository;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.repositories.ProfileRepository;
import com.springvuegradle.utilities.ActivityTestUtils;
import com.springvuegradle.utilities.ProfileTestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivitySearchServiceTest {

    @Autowired
    ProfileRepository profileRepository;

    @Autowired
    ActivityRepository activityRepository;

    @Autowired
    ActivityMembershipRepository activityMembershipRepository;

    @Autowired
    ActivityService activityService;

    private Activity publicActivityWellington;
    private Activity publicActivityChristchurch;
    private Activity publicActivityDelaware;

    private Activity membersActivitySydney;
    private Activity membersActivityChristchurch;

    private Activity privateActivityChristchurch;
    private Activity privateActivityManila;

    private Profile creator;

    Double WELLINGTON_LATITUDE = -41.3D;
    Double WELLINGTON_LONGITUDE = 174.8D;

    Double CHRISTCHURCH_LATITUDE = -43.5D;
    Double CHRISTCHURCH_LONGITUDE = 172.6D;

    Double DELAWARE_LATITUDE = 38.9D;
    Double DELAWARE_LONGITUDE = -75.5D;

    Double SYDNEY_LATITUDE = -33.8D;
    Double SYDNEY_LONGITUDE = 151.0D;

    Double MANILA_LATITUDE = 14.6D;
    Double MANILA_LONGITUDE = 120.1D;

    String[] noActivityTypes = new String[] {};


    @BeforeEach
    void setup() {
        publicActivityWellington = ActivityTestUtils.createActivity("Wellington public", WELLINGTON_LATITUDE, WELLINGTON_LONGITUDE);
        publicActivityChristchurch = ActivityTestUtils.createActivity("Christchurch public", CHRISTCHURCH_LATITUDE, CHRISTCHURCH_LONGITUDE);
        publicActivityDelaware = ActivityTestUtils.createActivity("Delware public", DELAWARE_LATITUDE, DELAWARE_LONGITUDE);
        for(Activity activity: List.of(publicActivityWellington, publicActivityChristchurch, publicActivityDelaware)){
            activity.setPrivacyLevel(2);
        }

        membersActivityChristchurch = ActivityTestUtils.createActivity("Christchurch member", CHRISTCHURCH_LATITUDE, CHRISTCHURCH_LONGITUDE);
        membersActivitySydney = ActivityTestUtils.createActivity("Sydney member", SYDNEY_LATITUDE, SYDNEY_LONGITUDE);
        for(Activity activity: List.of(membersActivityChristchurch, membersActivitySydney)){
            activity.setPrivacyLevel(1);
        }

        privateActivityChristchurch = ActivityTestUtils.createActivity("Christchurch private", CHRISTCHURCH_LATITUDE, CHRISTCHURCH_LONGITUDE);
        privateActivityManila = ActivityTestUtils.createActivity("Manila private", MANILA_LATITUDE, MANILA_LONGITUDE);

        creator = ProfileTestUtils.createProfileJimmyAlternate();
        profileRepository.save(creator);

        List<Activity> activities = List.of(publicActivityWellington,
                publicActivityChristchurch,
                publicActivityDelaware,
                membersActivityChristchurch,
                membersActivitySydney,
                privateActivityChristchurch,
                privateActivityManila);

        for(Activity activity: activities){
            activityRepository.save(activity);
            ActivityMembership membership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
            membership.setActivity(activity);
            activityMembershipRepository.save(membership);
            activity.addMember(membership);
            activityRepository.save(activity);
        }
    }

    @AfterEach
    void tearDown() {
        activityRepository.deleteAll();
    }

    @Autowired
    private ActivitySearchService activitySearchService;

    @Test
        //Ensures admin can see all activities when searching with high distance
    void adminCanSeeAllActivitiesInWorld() {
        List<ActivityLocationResponse> expected = activityService.createActivityLocationResponse(List.of(
                publicActivityWellington,
                membersActivityChristchurch,
                privateActivityChristchurch,
                publicActivityChristchurch,
                membersActivitySydney,
                privateActivityManila,
                publicActivityDelaware));
        List<ActivityLocationResponse> actual = activitySearchService.getActivitiesByRangeAndActivityTypes(1L, true, 1000000000, WELLINGTON_LATITUDE, WELLINGTON_LONGITUDE, noActivityTypes, null);
        assertEquals(expected, actual);
    }

    @Test
    void adminCanSeeAllActivitiesWithinNewZealand() {
        assertEquals(activityService.createActivityLocationResponse(List.of(
                membersActivityChristchurch,
                privateActivityChristchurch,
                publicActivityChristchurch,
                publicActivityWellington
        )), activitySearchService.getActivitiesByRangeAndActivityTypes(1L, true, 450000, CHRISTCHURCH_LATITUDE, CHRISTCHURCH_LONGITUDE, noActivityTypes, null));
    }

    @Test
    void adminCanSeeALlActivitiesWithinOceaniaAndAsia() {
        List<ActivityLocationResponse> expected = activityService.createActivityLocationResponse(List.of(
                publicActivityChristchurch,
                publicActivityWellington,
                membersActivityChristchurch,
                privateActivityChristchurch,
                membersActivitySydney,
                privateActivityManila
        ));

        List<ActivityLocationResponse> actual = activitySearchService.getActivitiesByRangeAndActivityTypes(1L, true, 9000000, WELLINGTON_LATITUDE, WELLINGTON_LONGITUDE, noActivityTypes, null);
        assertTrue(expected.size() == actual.size() &&
                new HashSet(expected).equals(new HashSet(actual)));
    }

    @Test
    void userCanSeePublicActivitiesInChristchurch() {
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        profileRepository.save(profile);
        List<ActivityLocationResponse> expected = activityService.createActivityLocationResponse(List.of(
                publicActivityChristchurch
        ));
        List<ActivityLocationResponse> actual = activitySearchService.getActivitiesByRangeAndActivityTypes(profile.getId(), false, 100000, CHRISTCHURCH_LATITUDE + 0.1, CHRISTCHURCH_LONGITUDE + 0.1, noActivityTypes, null);
        assertTrue(expected.size() == actual.size() &&
                new HashSet(expected).equals(new HashSet(actual)));
    }


    @Test
    void userCanSeePublicActivitiesInNewZealand() {
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        profileRepository.save(profile);
        List<ActivityLocationResponse> expected = activityService.createActivityLocationResponse(List.of(
                publicActivityChristchurch,
                publicActivityWellington
        ));
        List<ActivityLocationResponse> actual = activitySearchService.getActivitiesByRangeAndActivityTypes(profile.getId(), false, 500000, CHRISTCHURCH_LATITUDE + 0.1, CHRISTCHURCH_LONGITUDE + 0.1, noActivityTypes, null);
        assertTrue(expected.size() == actual.size() &&
                new HashSet(expected).equals(new HashSet(actual)));
    }

    @Test
    void memberCanSeeNonPrivateActivitiesInNewZealand(){
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        profileRepository.save(profile);

        ActivityMembership membership = new ActivityMembership(membersActivityChristchurch, profile, ActivityMembership.Role.PARTICIPANT);
        membership.setActivity(membersActivityChristchurch);
        activityMembershipRepository.save(membership);
        membersActivityChristchurch.addMember(membership);
        activityRepository.save(membersActivityChristchurch);

        List<ActivityLocationResponse> expected = activityService.createActivityLocationResponse(List.of(
                publicActivityChristchurch,
                publicActivityWellington,
                membersActivityChristchurch
        ));
        List<ActivityLocationResponse> actual = activitySearchService.getActivitiesByRangeAndActivityTypes(profile.getId(), false, 500000, CHRISTCHURCH_LATITUDE + 0.1, CHRISTCHURCH_LONGITUDE + 0.1, noActivityTypes, null);
        assertTrue(expected.size() == actual.size() &&
                new HashSet(expected).equals(new HashSet(actual)));
    }

    @Test
    void creatorOfActivityCanSeeTheirOwnActivitysNewZealand(){
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        profileRepository.save(profile);
        List<ActivityLocationResponse> expected = activityService.createActivityLocationResponse(List.of(
                publicActivityChristchurch,
                publicActivityWellington,
                membersActivityChristchurch,
                privateActivityChristchurch
        ));
        List<ActivityLocationResponse> actual = activitySearchService.getActivitiesByRangeAndActivityTypes(creator.getId(), false, 500000, WELLINGTON_LATITUDE + 0.1, WELLINGTON_LONGITUDE + 0.1, noActivityTypes, null);
        assertTrue(expected.size() == actual.size() &&
                new HashSet(expected).equals(new HashSet(actual)));
    }


    @Test
    void userCanSeePublicActivitiesInWorld() {
        Profile profile = ProfileTestUtils.createProfileJimmyAlternate();
        profileRepository.save(profile);
        List<ActivityLocationResponse> expected = activityService.createActivityLocationResponse(List.of(
                publicActivityDelaware,
                publicActivityChristchurch,
                publicActivityWellington
        ));
        List<ActivityLocationResponse> actual = activitySearchService.getActivitiesByRangeAndActivityTypes(profile.getId(), false, 500000000, DELAWARE_LATITUDE, DELAWARE_LONGITUDE, noActivityTypes, null);
        assertTrue(expected.size() == actual.size() &&
                new HashSet(expected).equals(new HashSet(actual)));
    }

    @Test
    void inRangeSucceedsWithValidNumbersTest() {
        assertTrue(activitySearchService.isInRange(7D, 5, 10));
    }

    @Test
    void inRangeFailsWithNullTest() {
        assertFalse(activitySearchService.isInRange(null, 4, 15));
    }

    @Test
    void inRangeFailsWithOutOFRangeTest() {
        assertFalse(activitySearchService.isInRange(33D, 4, 15));
    }

    @Test
    void inRangeSuceedsWithBoundaryTest() {
        assertTrue(activitySearchService.isInRange(5D, 5, 10));
    }


    @Test
    void distanceBetweenSameLocationsTest() {
        assertEquals(0, activitySearchService.distance(50, 50, 50, 50));
    }

    @Test
    void distanceBetweenDifferentLocationsTest() {
        assertThat(activitySearchService.distance(10, 30, 30, 50)).isBetween(3040600D, 3040610D);
    }

    @Test
    void getActivitiesByNameWhenMethodIsAnyReturnsActivitiesMatchingAnyKeywordTest() {
        Pageable pageable = PageRequest.of(0, 5);
        String keywords = "Christchurch Manila";
        //List<String> keywords = List.of("Christchurch", "Manila");
        Set<Activity> expectedActivities = Set.of(
                publicActivityChristchurch, privateActivityChristchurch, privateActivityManila, membersActivityChristchurch);

        Page<Activity> activities = activitySearchService.getActivitiesByName(keywords, creator.getId(), true, "any", pageable);
        Set<Activity> actualActivities = activities.toSet();
        assertEquals(expectedActivities, actualActivities);
    }

    @Test
    void getActivitiesByNameWhenMethodIsAllReturnsActivitiesMatchingAllKeywordsTest() {
        Pageable pageable = PageRequest.of(0, 5);
        String keywords = "Christchurch Public";
        //List<String> keywords = List.of("Christchurch", "Public");
        Set<Activity> expectedActivities = Set.of(publicActivityChristchurch);

        Page<Activity> activities = activitySearchService.getActivitiesByName(keywords, creator.getId(), true, "all", pageable);
        Set<Activity> actualActivities = activities.toSet();
        assertEquals(expectedActivities, actualActivities);
    }

    @Test
    void getActivitiesByNameWhenNoActivitiesMatchReturnsEmptyPageTest() {
        Pageable pageable = PageRequest.of(0, 5);
        String keywords = "\"Don't match any activities\"";
        //List<String> keywords = List.of("Don't match any activities");

        Page<Activity> activities = activitySearchService.getActivitiesByName(keywords, creator.getId(), false, "all", pageable);
        assertTrue(activities.isEmpty());
    }

    @Test
    void getActivitiesByNameWhenKeywordsAreEmptyReturnsEmptyPageTest() {
        Pageable pageable = PageRequest.of(0, 5);
        String keywords = null;
        //List<String> keywords = List.of();

        Page<Activity> activities = activitySearchService.getActivitiesByName(keywords, creator.getId(), false, "all", pageable);
        assertTrue(activities.isEmpty());
    }

    @Test
    void getActivitiesByNameWhenKeywordIsEmptyStringAndUserIsAdminReturnsAllActivitiesTest() {
        Pageable pageable = PageRequest.of(0, Math.toIntExact(activityRepository.count()));
        String keywords = "";
        //List<String> keywords = List.of("");
        Set<Activity> expectedActivities = new HashSet<>(activityRepository.findAll());
        Page<Activity> activities = activitySearchService.getActivitiesByName(keywords, creator.getId(), true, "all", pageable);
        assertEquals(expectedActivities, activities.toSet());
    }

    /**
     * A visible activity means the activity is public, the activity is restricted and the user is a member, or the
     * user is the creator of the activity.
     */
    @Test
    void getActivitiesByNameWhenUserIsNotAdminOnlyReturnsVisibleActivitiesTest() {
        Pageable pageable = PageRequest.of(0, Math.toIntExact(activityRepository.count()));
        String keywords = "";
        //List<String> keywords = List.of("");

        Activity newActivity = ActivityTestUtils.createNormalActivity();
        newActivity = activityRepository.save(newActivity);
        Profile profile = profileRepository.save(ProfileTestUtils.createProfileNoPassportCountry());
        activityMembershipRepository.save(new ActivityMembership(membersActivitySydney, profile, ActivityMembership.Role.FOLLOWER));
        activityMembershipRepository.save(new ActivityMembership(newActivity, profile, ActivityMembership.Role.CREATOR));

        Set<Activity> expectedActivities = Set.of(newActivity, membersActivitySydney, publicActivityDelaware,
                publicActivityChristchurch, publicActivityWellington);
        Page<Activity> activities = activitySearchService.getActivitiesByName(keywords, profile.getId(), false, "all", pageable);
        assertEquals(expectedActivities, activities.toSet());
    }

    @Test
    void splitKeywordsTest() {
        String keywordString = "blue cheese \"armadillo hat\"";
        Set<String> expected = Set.of("blue", "cheese", "armadillo hat");
        assertEquals(expected, new HashSet<String>(activitySearchService.splitKeywordString(keywordString)));
    }

    @Test
    void splitKeywordsWhenStringIsEmptyTest() {
        String keywordString = "";
        Set<String> expected = Set.of("");
        assertEquals(expected, new HashSet<String>(activitySearchService.splitKeywordString(keywordString)));
    }

    @Test
    void splitKeywordsWhenStringIsBlankTest() {
        String keywordString = "    ";
        Set<String> expected = Set.of("");
        assertEquals(expected, new HashSet<String>(activitySearchService.splitKeywordString(keywordString)));
    }

    @Test
    void splitKeywordsWhenStringHasMultipleQuotedKeywordsTest() {
        String keywordString = "blue cheese \"CSSE Fun Run\" fish \"Funnus Runnus\" umbrella ";
        Set<String> expected = Set.of("blue", "cheese", "CSSE Fun Run", "Funnus Runnus", "umbrella", "fish");
        assertEquals(expected, new HashSet<String>(activitySearchService.splitKeywordString(keywordString)));
    }

    @Test
    void splitKeywordsWhenStringHasAdjacentQuotedKeywordsTest() {
        String keywordString = "blue cheese \"CSSE Fun Run\"\"Funnus Runnus\" umbrella ";
        Set<String> expected = Set.of("blue", "cheese", "CSSE Fun Run", "Funnus Runnus", "umbrella");
        assertEquals(expected, new HashSet<String>(activitySearchService.splitKeywordString(keywordString)));
    }

    @Test
    void splitKeywordsWhenStringIsNullReturnsEmptyListTest() {
        String keywordString = null;
        assertTrue(activitySearchService.splitKeywordString(keywordString).isEmpty());
    }
}
