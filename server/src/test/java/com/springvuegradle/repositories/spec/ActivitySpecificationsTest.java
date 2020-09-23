package com.springvuegradle.repositories.spec;

import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.ActivityMembershipRepository;
import com.springvuegradle.repositories.ActivityRepository;
import com.springvuegradle.repositories.ProfileRepository;
import com.springvuegradle.utilities.ActivityTestUtils;
import com.springvuegradle.utilities.ProfileTestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;


@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivitySpecificationsTest {

    private Activity publicRace, publicBlazerTag, restrictRace, privateCycle, privateRace;
    private Profile jimmy, jimmyTwo, quick, maurice;

    @Autowired
    ActivityRepository activityRepository;
    @Autowired
    ActivityMembershipRepository membershipRepository;
    @Autowired
    ProfileRepository profileRepository;

    @BeforeEach
    private void setUp() {
        publicRace = ActivityTestUtils.createNormalActivity();
        publicBlazerTag = ActivityTestUtils.createNormalActivityTwo();
        privateCycle = ActivityTestUtils.createNormalActivityThree();
        privateRace = ActivityTestUtils.createActivity("My Own race", 47.0, 110.0);
        restrictRace = ActivityTestUtils.createActivity("Friends Only race", 10.0, 1.0);

        jimmy = ProfileTestUtils.createProfileJimmy();
        jimmy.setPassports(new HashSet<>());
        maurice = ProfileTestUtils.createNormalProfileMaurice();
        jimmyTwo = ProfileTestUtils.createProfileJimmyAlternate();
        quick = ProfileTestUtils.createProfileNicknameMatchesJimmySurname();

        publicRace.setPrivacyLevel(2);
        publicBlazerTag.setPrivacyLevel(2);
        privateCycle.setPrivacyLevel(0);
        privateRace.setPrivacyLevel(0);
        restrictRace.setPrivacyLevel(1);
    }

    @Test
    void hasPrivacyLevelWithPublicLevelReturnsPublicActivitiesTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        activityRepository.save(privateCycle);
        activityRepository.save(restrictRace);

        Set<Activity> expectedActivities = new HashSet<>(Arrays.asList(publicBlazerTag, publicRace));

        Specification<Activity> spec = ActivitySpecifications.hasPrivacyLevel(2);
        List<Activity> result = activityRepository.findAll(spec);
        Set<Activity> actualActivities = new HashSet<>(result);
        assertEquals(expectedActivities, actualActivities);
    }

    @Test
    void hasPrivacyLevelWithOneLevelOverHighestReturnsEmptyListTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        activityRepository.save(privateCycle);
        activityRepository.save(restrictRace);

        Specification<Activity> spec = ActivitySpecifications.hasPrivacyLevel(3);
        List<Activity> result = activityRepository.findAll(spec);
        assertTrue(result.isEmpty());
    }

    @Test
    void hasPrivacyLevelWithOneLevelUnderLowestReturnsEmptyListTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        activityRepository.save(privateCycle);
        activityRepository.save(restrictRace);

        Specification<Activity> spec = ActivitySpecifications.hasPrivacyLevel(-1);
        List<Activity> result = activityRepository.findAll(spec);
        assertTrue(result.isEmpty());
    }

    @Test
    void nameContainsWithPatternRaceReturnsAllRacesTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        privateCycle = activityRepository.save(privateCycle);
        privateRace = activityRepository.save(privateRace);
        restrictRace = activityRepository.save(restrictRace);

        Set<Activity> expectedActivities = new HashSet<>(Arrays.asList(publicRace, privateRace, restrictRace, privateCycle));

        Specification<Activity> spec = ActivitySpecifications.nameContains("race");
        List<Activity> result = activityRepository.findAll(spec);
        Set<Activity> actualActivities = new HashSet<>(result);
        assertEquals(expectedActivities, actualActivities);
    }

    @Test
    void nameContainsWithEmptyPatternReturnsAllActivitiesTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        privateCycle = activityRepository.save(privateCycle);

        Set<Activity> expectedActivities = new HashSet<>(activityRepository.findAll());

        Specification<Activity> spec = ActivitySpecifications.nameContains("");
        List<Activity> result = activityRepository.findAll(spec);
        Set<Activity> actualActivities = new HashSet<>(result);
        assertEquals(expectedActivities, actualActivities);
    }

    @Test
    void hasMemberWithRoleAndNoMatchesReturnsEmptyListTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        jimmy = profileRepository.save(jimmy);

        Specification<Activity> spec = ActivitySpecifications.hasMember(jimmy.getId(), ActivityMembership.Role.ORGANISER);
        List<Activity> result = activityRepository.findAll(spec);
        assertTrue(result.isEmpty());
    }

    @Test
    void hasMemberWithNoRoleAndNoMatchesReturnsEmptyListTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        jimmy = profileRepository.save(jimmy);

        Specification<Activity> spec = ActivitySpecifications.hasMember(jimmy.getId());
        List<Activity> result = activityRepository.findAll(spec);
        assertTrue(result.isEmpty());
    }

    @Test
    void hasMemberWithNoRoleReturnsAllActivitiesThatHaveThatMemberTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        privateCycle = activityRepository.save(privateCycle);
        privateRace = activityRepository.save(privateRace);
        restrictRace = activityRepository.save(restrictRace);
        jimmy = profileRepository.save(jimmy);
        membershipRepository.save(new ActivityMembership(publicRace, jimmy, ActivityMembership.Role.ORGANISER));
        membershipRepository.save(new ActivityMembership(publicBlazerTag, jimmy, ActivityMembership.Role.FOLLOWER));
        membershipRepository.save(new ActivityMembership(privateCycle, jimmy, ActivityMembership.Role.CREATOR));
        membershipRepository.save(new ActivityMembership(restrictRace, jimmy, ActivityMembership.Role.PARTICIPANT));

        Set<Activity> expectedActivities = new HashSet<>(Arrays.asList(publicRace, publicBlazerTag, restrictRace, privateCycle));

        Specification<Activity> spec = ActivitySpecifications.hasMember(jimmy.getId());
        List<Activity> result = activityRepository.findAll(spec);
        Set<Activity> actualActivities = new HashSet<>(result);
        assertEquals(expectedActivities, actualActivities);
    }

    @Test
    void hasMemberWithRoleReturnsAllActivitiesThatHaveThatMemberAsThatRoleTest() {
        publicRace = activityRepository.save(publicRace);
        publicBlazerTag = activityRepository.save(publicBlazerTag);
        privateCycle = activityRepository.save(privateCycle);
        privateRace = activityRepository.save(privateRace);
        restrictRace = activityRepository.save(restrictRace);
        jimmy = profileRepository.save(jimmy);
        membershipRepository.save(new ActivityMembership(publicRace, jimmy, ActivityMembership.Role.ORGANISER));
        membershipRepository.save(new ActivityMembership(publicBlazerTag, jimmy, ActivityMembership.Role.FOLLOWER));
        membershipRepository.save(new ActivityMembership(privateCycle, jimmy, ActivityMembership.Role.CREATOR));
        membershipRepository.save(new ActivityMembership(restrictRace, jimmy, ActivityMembership.Role.PARTICIPANT));

        Set<Activity> expectedActivities = Collections.singleton(publicRace);

        Specification<Activity> spec = ActivitySpecifications.hasMember(jimmy.getId(), ActivityMembership.Role.ORGANISER);
        List<Activity> result = activityRepository.findAll(spec);
        Set<Activity> actualActivities = new HashSet<>(result);
        assertEquals(expectedActivities, actualActivities);
    }
}