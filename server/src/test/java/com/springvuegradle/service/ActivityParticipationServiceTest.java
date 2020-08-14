package com.springvuegradle.service;

import com.springvuegradle.model.*;
import com.springvuegradle.utilities.InitialDataHelper;
import com.springvuegradle.repositories.*;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
public class ActivityParticipationServiceTest {

    @Autowired
    ActivityService service;
    @Autowired
    ProfileRepository profileRepository;
    @Autowired
    ActivityRepository activityRepository;
    @Autowired
    ActivityTypeRepository typeRepository;
    @Autowired
    ActivityMembershipRepository activityMembershipRepository;
    @Autowired
    ActivityParticipationRepository activityParticipationRepository;
    @Autowired
    EmailRepository emailRepository;

    @BeforeEach
    void setUp() {
        InitialDataHelper.init(typeRepository, profileRepository, emailRepository);
    }

    /**
     * Needs to be run after each test to ensure the repositories are emptied.
     */
    @AfterEach
    void tearDown() {
        activityMembershipRepository.deleteAll();
        emailRepository.deleteAll();
        profileRepository.deleteAll();
        activityRepository.deleteAll();
        typeRepository.deleteAll();
    }

    @Test
    void createParticipationSuccessfulTest() {
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();
        ActivityParticipation participation = ActivityTestUtils.createNormalParticipation();

        assertEquals(0, activityParticipationRepository.count(), "Sanity check: repo is empty before testing");
        service.createParticipation(activityId, profileId, participation);
        assertEquals(1, activityParticipationRepository.count());
    }

    @Test
    void createParticipationWhenProfileDoesntExistThrowsExceptionTest() {
        long profileId = profileRepository.getLastInsertedId();
        long fakeProfileId = 987654323;
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();
        ActivityParticipation participation = ActivityTestUtils.createNormalParticipation();

        assertThrows(IllegalArgumentException.class, () -> service.createParticipation(activityId, fakeProfileId, participation));
    }

    @Test
    void createParticipationWhenActivityDoesntExistThrowsExceptionTest() {
        long profileId = profileRepository.getLastInsertedId();
        long fakeActivityId = 987654323;
        ActivityParticipation participation = ActivityTestUtils.createNormalParticipation();

        assertThrows(IllegalArgumentException.class, () -> service.createParticipation(fakeActivityId, profileId, participation));
    }

    @Test
    void readParticipationsFromActivityNormalTest() {
        long creatorId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), creatorId);
        Profile participant1 = ProfileTestUtils.createNormalProfileMaurice();
        Profile participant2 = ProfileTestUtils.createProfileJimmy();
        participant1.setPassports(new HashSet<>());
        participant2.setPassports(new HashSet<>());
        participant1 = profileRepository.save(participant1);
        participant2 = profileRepository.save(participant2);
        long activityId = activityRepository.getLastInsertedId();
        ActivityParticipation participation1 = ActivityTestUtils.createNormalParticipation();
        ActivityParticipation participation2 = ActivityTestUtils.createADifferentParticipation();
        service.createParticipation(activityId, participant1.getId(), participation1);
        service.createParticipation(activityId, participant2.getId(), participation2);

        List<ActivityParticipation> result = service.readParticipationsFromActivity(activityId);
        assertEquals(2, result.size());
    }

    @Test
    void readParticipationsFromActivityWhenActivityHasNoParticipationsReturnsEmptyListTest() {
        long creatorId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), creatorId);
        long activityId = activityRepository.getLastInsertedId();

        List<ActivityParticipation> result = service.readParticipationsFromActivity(activityId);
        assertTrue(result.isEmpty());
    }

    @Test
    void readParticipationsFromActivityWhenActivityDoesNotExistThrowsIllegalArgumentExceptionTest() {
        long invalidId = -1;
        assertThrows(IllegalArgumentException.class, () -> service.readParticipationsFromActivity(-1));
    }
}
