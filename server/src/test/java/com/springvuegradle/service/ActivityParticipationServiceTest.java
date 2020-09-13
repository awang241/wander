package com.springvuegradle.service;

import com.springvuegradle.model.*;
import com.springvuegradle.utilities.ActivityTestUtils;
import com.springvuegradle.utilities.FormatHelper;
import com.springvuegradle.utilities.InitialDataHelper;
import com.springvuegradle.repositories.*;
import com.springvuegradle.utilities.ProfileTestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Optional;

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
    void editParticipationOutcomeSuccessTest() {
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();

        ActivityParticipation activityParticipation = ActivityTestUtils.createNormalParticipation();
        service.createParticipation(activityId, profileId, activityParticipation);
        long participationId = activityParticipation.getId();
        ActivityParticipation editedParticipation = ActivityTestUtils.createEditedNormalParticipation();
        service.editParticipation(activityId, profileId, participationId, editedParticipation);
        Optional<ActivityParticipation> result = activityParticipationRepository.findById(participationId);
        ActivityParticipation updatedParticipation = result.get();

        assertEquals(activityParticipation.getId(), updatedParticipation.getId());
        assertEquals("Won the game", activityParticipation.getOutcome());
    }

    @Test
    void editParticipationDetailsSuccessTest(){
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();

        ActivityParticipation activityParticipation = ActivityTestUtils.createNormalParticipation();
        service.createParticipation(activityId, profileId, activityParticipation);
        long participationId = activityParticipation.getId();
        ActivityParticipation editedParticipation = ActivityTestUtils.createEditedNormalParticipation();
        service.editParticipation(activityId, profileId, participationId, editedParticipation);
        Optional<ActivityParticipation> result = activityParticipationRepository.findById(participationId);
        ActivityParticipation updatedParticipation = result.get();

        assertEquals(activityParticipation.getId(), updatedParticipation.getId());
        assertEquals("Played League as a pro", activityParticipation.getDetails());
    }

    @Test
    void editParticipationStartTimeSuccessTest(){
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();

        ActivityParticipation activityParticipation = ActivityTestUtils.createNormalParticipation();
        service.createParticipation(activityId, profileId, activityParticipation);
        long participationId = activityParticipation.getId();
        ActivityParticipation editedParticipation = ActivityTestUtils.createEditedNormalParticipation();
        service.editParticipation(activityId, profileId, participationId, editedParticipation);
        Optional<ActivityParticipation> result = activityParticipationRepository.findById(participationId);
        ActivityParticipation updatedParticipation = result.get();

        assertEquals(activityParticipation.getId(), updatedParticipation.getId());
        assertEquals(FormatHelper.parseOffsetDateTime("2020-02-20T12:00:00+1300"), activityParticipation.getStartTime());
    }

    @Test
    void editParticipationEndTimeSuccessTest(){
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();

        ActivityParticipation activityParticipation = ActivityTestUtils.createNormalParticipation();
        service.createParticipation(activityId, profileId, activityParticipation);
        long participationId = activityParticipation.getId();
        ActivityParticipation editedParticipation = ActivityTestUtils.createEditedNormalParticipation();
        service.editParticipation(activityId, profileId, participationId, editedParticipation);
        Optional<ActivityParticipation> result = activityParticipationRepository.findById(participationId);
        ActivityParticipation updatedParticipation = result.get();

        assertEquals(activityParticipation.getId(), updatedParticipation.getId());
        assertEquals(FormatHelper.parseOffsetDateTime("2020-02-20T16:00:00+1300"), activityParticipation.getEndTime());
    }

    @Test
    void editParticipationWhereParticipationDoesNotExistThrowErrorTest(){
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();

        long participationId = 20;
        ActivityParticipation editedParticipation = ActivityTestUtils.createEditedNormalParticipation();
        assertThrows(IllegalArgumentException.class, ()->service.editParticipation(activityId, profileId, participationId, editedParticipation));
    }

    @Test
    void deleteParticipationSuccessTest() {
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();
        ActivityParticipation participation = ActivityTestUtils.createNormalParticipation();
        service.createParticipation(activityId, profileId, participation);
        service.removeParticipation(activityId, profileId, participation.getId());
        assertEquals(0, activityParticipationRepository.count());
    }

    @Test
    void deleteParticipationFailParticipationIdDoesNotExistTest() {
        long profileId = profileRepository.getLastInsertedId();
        service.create(ActivityTestUtils.createNormalActivity(), profileId);
        long activityId = activityRepository.getLastInsertedId();
        long failParticipationId = 20;
        assertThrows(IllegalArgumentException.class, ()->service.removeParticipation(activityId, profileId, failParticipationId));
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