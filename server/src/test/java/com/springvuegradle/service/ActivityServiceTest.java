package com.springvuegradle.service;

import com.springvuegradle.controller.ActivityController;
import com.springvuegradle.dto.ActivityRoleCountResponse;
import com.springvuegradle.enums.ActivityMessage;
import com.springvuegradle.enums.ActivityPrivacy;
import com.springvuegradle.model.*;
import com.springvuegradle.dto.responses.ActivityMemberProfileResponse;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.*;
import com.springvuegradle.utilities.FormatHelper;
import com.springvuegradle.utilities.InitialDataHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.security.AccessControlException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ActivityServiceTest {

    @Autowired
    ActivityController controller;
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
    EmailRepository emailRepository;
    @Autowired
    ActivityParticipationRepository activityParticipationRepository;
    @Autowired
    NotificationRepository notificationRepository;

    /**
     * Needs to be run before each test to create new test profiles and repositories.
     */
    @BeforeEach
    void setUp() {
        InitialDataHelper.init(typeRepository, profileRepository, emailRepository);
    }

    /**
     * Needs to be run after each test to ensure the repositories are emptied.
     */
    @AfterEach
    void tearDown() {
        emailRepository.deleteAll();
        notificationRepository.deleteAll();
        activityMembershipRepository.deleteAll();
        profileRepository.deleteAll();
        activityRepository.deleteAll();

        typeRepository.deleteAll();
    }

    /**
     * Test to create a basic new activity
     **/
    @Test
    void createNewActivityTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity trackRace = createNormalActivity();

        service.create(trackRace, profile.getId());
        List<Activity> result = activityRepository.findByActivityNames(trackRace.getActivityName());

        assertEquals("Kaikoura Coast Track race", result.get(0).getActivityName());
    }

    /**
     * Test to check that an activity is saved under an activity type
     **/
    @Test
    void findActivityByActivityTypeTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity trackRace = createNormalActivity();

        service.create(trackRace, profile.getId());
        activityRepository.findByActivityNames(trackRace.getActivityName());

        List<ActivityType> activityTypeList = typeRepository.findByActivityTypeName("Tramping");
        ActivityType activityType = activityTypeList.get(0);

        assertEquals(1, activityType.getActivities().size());
    }

    /**
     * Test to edit an already existing activity
     **/
    @Test
    void updateActivityWithNormalDataSavesActivityTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivityKaikoura(), actualActivity = null;
        Activity activityBefore = activityRepository.findById(activityId).get();
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.update(expectedActivity, activityId, profile.getId());
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(activityId, actualActivity.getId());
        assertEquals(activityBefore, actualActivity);
    }

    /**
     * Test to edit an activity which doesn't already exist
     **/
    @Test
    void updateActivityNotInDatabaseThrowsException() {
        Profile profile = profileRepository.save(createNormalProfileBen());
        assertThrows(IllegalArgumentException.class, ()-> service.update(createNormalActivityKaikoura(), 0L, profile.getId()));
    }

    /**
     * Test to create an activity with no name
     **/
    @Test
    void updateActivityWithBlankNameTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    /**
     * Test to edit an activity with no start date
     **/
    @Test
    void updateActivityWithDurationAndNoStartDateTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    /**
     * Test to edit an activity with no end date
     **/
    @Test
    void updateActivityWithDurationAndNoEndDateTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    /**
     * Test to edit an activity with end date before start date
     **/
    @Test
    void updateActivityWithMisorderedDateThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    /**
     * Test to edit an activity with invalid activity types
     **/
    @Test
    void updateActivityWithInvalidActivityTypesThrowsExceptionTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    /**
     * Test to edit an activity with no activity types selected
     **/
    @Test
    void updateActivityWithNoActivityTypesTest() {
        activityRepository.save(createNormalActivitySilly());
        Long activityId = activityRepository.getLastInsertedId();
        Activity expectedActivity = createNormalActivitySilly(), actualActivity = null;
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        assertEquals(expectedActivity, actualActivity);
    }

    /**
     * Test to delete an activity
     **/
    @Test
    void deleteActivitySuccessTest() {
        Profile profile = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        activityMembershipRepository.save(new ActivityMembership(activity, profile, ActivityMembership.Role.CREATOR));
        service.delete(activity.getId(), profile.getId());
        assertEquals(0, activityRepository.count());
    }

    /**
     * Test to remove a profiles membership from an activity they have membership with
     **/
    @Test
    void removeActivityMemberShipSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        Profile admin = createNormalProfileBen();
        admin.setAuthLevel(1);
        profileRepository.save(bennyBoi);
        profileRepository.save(admin);
        ActivityMembership testMemberShip = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.PARTICIPANT);
        activityMembershipRepository.save(testMemberShip);
        service.removeUserRoleFromActivity(admin.getId(), bennyBoi.getId(), activity.getId());
        assertEquals(0, activityMembershipRepository.count());
    }

    /**
     * Test to remove a profiles membership from an activity which they are not a part of
     */
    @Test
    void removeMembershipWhereMembershipDoesNotExistThrowsExceptionTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        profileRepository.save(bennyBoi);
        Profile johnnyBoi = createNormalProfileJohnny();
        profileRepository.save(johnnyBoi);
        ActivityMembership testMemberShip = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.ORGANISER);
        activityMembershipRepository.save(testMemberShip);
        assertThrows(NoSuchElementException.class, () -> service.removeUserRoleFromActivity(bennyBoi.getId(), johnnyBoi.getId(), activity.getId()));
        assertEquals(1, activityMembershipRepository.count());
    }

    @Test
    void removeMembershipWhenEditedIsOrganiserSucceedsTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        profileRepository.save(bennyBoi);
        Profile johnnyBoi = createNormalProfileJohnny();
        profileRepository.save(johnnyBoi);
        ActivityMembership bennyMembership = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.ORGANISER);
        activityMembershipRepository.save(bennyMembership);
        ActivityMembership johnnyMemberShip = new ActivityMembership(activity, johnnyBoi, ActivityMembership.Role.ORGANISER);
        activityMembershipRepository.save(johnnyMemberShip);

        service.removeUserRoleFromActivity(bennyBoi.getId(), johnnyBoi.getId(), activity.getId());
        assertEquals(1, activityMembershipRepository.count());
    }

    @Test
    void removeMembershipWhenMemberIsCreatorThrowsExceptionTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        profileRepository.save(bennyBoi);
        Profile johnnyBoi = createNormalProfileJohnny();
        profileRepository.save(johnnyBoi);
        ActivityMembership bennyMembership = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.ORGANISER);
        activityMembershipRepository.save(bennyMembership);
        ActivityMembership johnnyMemberShip = new ActivityMembership(activity, johnnyBoi, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(johnnyMemberShip);

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                service.removeUserRoleFromActivity(bennyBoi.getId(), johnnyBoi.getId(), activity.getId()));
        assertEquals(exception.getMessage(), ActivityMessage.EDITING_CREATOR.toString());
        assertEquals(2, activityMembershipRepository.count());
    }

    @Test
    void removeMembershipWhenUnauthorisedThrowsExceptionTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        profileRepository.save(bennyBoi);
        Profile johnnyBoi = createNormalProfileJohnny();
        profileRepository.save(johnnyBoi);
        ActivityMembership bennyMembership = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.PARTICIPANT);
        activityMembershipRepository.save(bennyMembership);
        ActivityMembership johnnyMemberShip = new ActivityMembership(activity, johnnyBoi, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(johnnyMemberShip);

        AccessControlException exception = assertThrows(AccessControlException.class, () ->
                service.removeUserRoleFromActivity(bennyBoi.getId(), johnnyBoi.getId(), activity.getId()));
    }

    @Test
    void removeMembershipWhenRemovingSelfSucceedsTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        profileRepository.save(bennyBoi);
        ActivityMembership bennyMembership = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.FOLLOWER);
        activityMembershipRepository.save(bennyMembership);

        service.removeUserRoleFromActivity(bennyBoi.getId(), bennyBoi.getId(), activity.getId());
        assertEquals(0, activityMembershipRepository.count());
    }

    /**
     * Test to delete an activity that doesn't exist
     **/
    @Test
    void deleteActivityDoesNotExistTest() {
        assertFalse(service.delete((long) 1, (long) 2));
    }

    /**
     * Test that a creator of an activity that is private can still view the activity.
     * Only the creator of the private activity can view it.
     */
    @Test
    void getPrivateActivityByIdAsCreatorServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(0);
        service.addActivityRole(activity.getId(), profile.getId(), "CREATOR");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * When an activity is private, organisers should not be able to view the activity.
     * Only the creator can view the private activity.
     */
    @Test
    void getPrivateActivityByIdAsOrganiserServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(0);
        service.addActivityRole(activity.getId(), profile.getId(), "ORGANISER");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertNull(activityResult);
    }

    /**
     * When an activity is private, participants should not be able to view the activity.
     * Only the creator can view a private activity.
     */
    @Test
    void getPrivateActivityByIdAsParticipantServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(0);
        service.addActivityRole(activity.getId(), profile.getId(), "PARTICIPANT");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertNull(activityResult);
    }

    /**
     * When an activity is private, followers should not be able to view the activity.
     * Only the creator can view a private activity.
     */
    @Test
    void getPrivateActivityByIdAsFollowerServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(0);
        service.addActivityRole(activity.getId(), profile.getId(), "FOLLOWER");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertNull(activityResult);
    }

    /**
     * When an activity is private, authenticated logged in users should not be able to view the activity.
     * Only the creator can view a private activity.
     */
    @Test
    void getPrivateActivityByIdAsUserServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(0);
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertNull(activityResult);
    }

    /**
     * Tests that a public activity can be viewed by any valid user.
     */
    @Test
    void getPublicActivityByIdAsUserServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(2);
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that a public activity can still be be viewed by the creator
     */
    @Test
    void getPublicActivityByIdAsCreatorServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(2);
        service.addActivityRole(activity.getId(), profile.getId(), "CREATOR");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that a public activity can still be be viewed by an organiser
     */
    @Test
    void getPublicActivityByIdAsOrganiserServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(2);
        service.addActivityRole(activity.getId(), profile.getId(), "ORGANISER");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that a public activity can still be be viewed by a participant
     */
    @Test
    void getPublicActivityByIdAsParticipantServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(2);
        service.addActivityRole(activity.getId(), profile.getId(), "PARTICIPANT");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that a public activity can still be be viewed by a follower
     */
    @Test
    void getPublicActivityByIdAsFollowerServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(2);
        service.addActivityRole(activity.getId(), profile.getId(), "FOLLOWER");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that a activity set to friends only cannot be viewed by a user without a role in the activity.
     */
    @Test
    void getFriendsOnlyActivityByIdAsUserServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(1);
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertNull(activityResult);
    }

    /**
     * Tests that an activity set to friends only can be viewed by the creator.
     */
    @Test
    void getFriendsOnlyActivityByIdAsCreatorServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(1);
        service.addActivityRole(activity.getId(), profile.getId(), "CREATOR");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that an activity set to friends only can be viewed by organisers.
     */
    @Test
    void getFriendsOnlyActivityByIdAsOrganiserServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(1);
        service.addActivityRole(activity.getId(), profile.getId(), "ORGANISER");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that an activity set to friends only can be viewed by participants.
     */
    @Test
    void getFriendsOnlyActivityByIdAsParticipantServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(1);
        service.addActivityRole(activity.getId(), profile.getId(), "PARTICIPANT");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     * Tests that an activity set to friends only can be viewed by followers.
     */
    @Test
    void getFriendsOnlyActivityByIdAsFollowerServiceTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivity());
        activity.setPrivacyLevel(1);
        service.addActivityRole(activity.getId(), profile.getId(), "FOLLOWER");
        Activity activityResult = service.getActivityByActivityId(profile.getId(), activity.getId(), profile.getAuthLevel());
        assertEquals(activity, activityResult);
    }

    /**
     *  Tests activity cannot be fetched with an invalid activity id.
     */
    @Test
    void getActivityByInvalidActivityIdTest() {
        long activityId = 10;
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity failedResult = service.getActivityByActivityId(profile.getId(), activityId, profile.getAuthLevel());
        assertNull(failedResult);
    }

    /**
     * Tests that you can add a user as a participant to an activity.
     */
    @Test
    void addNormalUserRoleToActivityTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        service.addActivityRole(activity.getId(), profile.getId(), "participant");
        assertEquals(1, activityMembershipRepository.findActivityMembershipsByActivity_IdAndRole(activity.getId(), ActivityMembership.Role.PARTICIPANT).size());
    }

    /**
     * Tests that you can add a user as a organiser to an activity.
     */
    @Test
    void creatorAddsOrganiserRoleToActivityTest() {
        Profile ben = profileRepository.save(createNormalProfileBen());
        Profile johnny = profileRepository.save(createNormalProfileJohnny());
        controller.createActivity(ben.getId(), createNormalActivityKaikoura(), null, true);
        service.addActivityRole(activityRepository.getLastInsertedId(), johnny.getId(), "organiser");
        assertEquals(1, activityMembershipRepository.findActivityMembershipsByActivity_IdAndRole(activityRepository.getLastInsertedId(), ActivityMembership.Role.ORGANISER).size());
    }

    /**
     * Tests you can edit an activity's privacy level to public
     */
    @Test
    void editActivityPrivacyToPublicTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.editActivityPrivacy("public", activity.getId(), profile.getId());
        assertEquals(2, activity.getPrivacyLevel());
    }

    /**
     * Tests you can edit the activity's privacy level to friends only
     */
    @Test
    void editActivityPrivacyToFriendsTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.editActivityPrivacy("restricted", activity.getId(), profile.getId());
        assertEquals(1, activity.getPrivacyLevel());
    }

    /**
     * Tests you can edit the activity's privacy level to private
     */
    @Test
    void editActivityPrivacyToPrivateTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.editActivityPrivacy("private", activity.getId(), profile.getId());
        assertEquals(0, activity.getPrivacyLevel());
    }

    /**
     * Tests when attempting to get private activities that a user is a role of (PARTICIPANT)
     * does not work.
     * Private activities can only be seen by the CREATOR or ADMIN
     */
    @Test
    void getActivitiesByProfileIdByRolePrivateParticipantTest() {
        int startIndex = 0;
        int count = 5;
        Profile benny = createNormalProfileBen();
        profileRepository.save(benny);
        Profile johnny = createNormalProfileJohnny();
        profileRepository.save(johnny);
        Activity activity = createNormalActivity();
        activity.setPrivacyLevel(0);
        controller.createActivity(benny.getId(), activity, null, true);
        service.addActivityRole(activity.getId(), johnny.getId(), "participant");
        List<Activity> list = service.getActivitiesByProfileIdByRole(johnny.getId(), ActivityMembership.Role.PARTICIPANT, startIndex, count, johnny.getAuthLevel());
        assertEquals(0, list.size());
    }

    /**
     * Tests when attempting to get public activities that a user is a role of (PARTICIPANT)
     * does work.
     * Public activities can be seen by all users with a role in the activity.
     */
    @Test
    void getActivitiesByIdByRolePublicParticipantTest() {
        int startIndex = 0;
        int count = 5;
        Profile benny = createNormalProfileBen();
        profileRepository.save(benny);
        Profile johnny = createNormalProfileJohnny();
        profileRepository.save(johnny);
        Activity activity = createNormalActivity();
        activity.setPrivacyLevel(2);
        controller.createActivity(johnny.getId(), activity, null, true);
        service.addActivityRole(activity.getId(), johnny.getId(), "participant");
        List<Activity> list = service.getActivitiesByProfileIdByRole(johnny.getId(), ActivityMembership.Role.PARTICIPANT, startIndex, count, johnny.getAuthLevel());
        assertEquals(1, list.size());
    }

    /**
     * Tests when attempting to get public activities that a user is a role of (ORGANISER)
     * does work.
     * Public activities can be seen by all users with a role in the activity.
     */
    @Test
    void getActivitiesByIdByRoleMemberOrganiserTest() {
        int startIndex = 0;
        int count = 5;
        Profile benny = createNormalProfileBen();
        profileRepository.save(benny);
        Profile johnny = createNormalProfileJohnny();
        profileRepository.save(johnny);
        Activity activity = createNormalActivity();
        activity.setPrivacyLevel(1);
        controller.createActivity(benny.getId(), activity, null, true);
        service.addActivityRole(activity.getId(), johnny.getId(), "organiser");
        List<Activity> list = service.getActivitiesByProfileIdByRole(johnny.getId(), ActivityMembership.Role.ORGANISER, startIndex, count, johnny.getAuthLevel());
        assertEquals(1, list.size());
    }

    /**
     * Tests when attempting to get private activities that a user is a CREATOR of works.
     * Private activities can be seen by CREATORS only.
     */
    @Test
    void getActivitiesByIdByRolePrivateCreatorTest() {
        int startIndex = 0;
        int count = 5;
        Profile benny = createNormalProfileBen();
        profileRepository.save(benny);
        Activity activity = createNormalActivity();
        activity.setPrivacyLevel(0);
        controller.createActivity(benny.getId(), activity, null, true);
        List<Activity> list = service.getActivitiesByProfileIdByRole(benny.getId(), ActivityMembership.Role.CREATOR, startIndex, count, benny.getAuthLevel());
        assertEquals(1, list.size());
    }

    /**
     * Tests when attempting to get public activities that a user is a CREATOR of still works.
     Public activities can be seen by all users with a role in the activity.
     */
    @Test
    void getActivitiesByIdByRolePublicCreatorTest() {
        int startIndex = 0;
        int count = 5;
        Profile benny = createNormalProfileBen();
        profileRepository.save(benny);
        Activity activity = createNormalActivity();
        activity.setPrivacyLevel(2);
        controller.createActivity(benny.getId(), activity, null, true);
        List<Activity> list = service.getActivitiesByProfileIdByRole(benny.getId(), ActivityMembership.Role.CREATOR, startIndex, count, benny.getAuthLevel());
        assertEquals(1, list.size());
    }

    /**
     * Test getting all public activities.
     */
    @Test
    void getPublicActivitiesSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.editActivityPrivacy("public", activity.getId(), profile.getId());
        assertEquals(1, service.getActivitiesWithPrivacyLevel(ActivityPrivacy.PUBLIC).size());
    }

    /**
     * Test getting all private activities.
     */
    @Test
    void getPrivateActivitiesSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.editActivityPrivacy("private", activity.getId(), profile.getId());
        assertEquals(1, service.getActivitiesWithPrivacyLevel(ActivityPrivacy.PRIVATE).size());
    }

    /**
     * Test getting all friends activities.
     */
    @Test
    void getFriendsActivitiesSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.editActivityPrivacy("restricted", activity.getId(), profile.getId());
        assertEquals(1, service.getActivitiesWithPrivacyLevel(ActivityPrivacy.FRIENDS).size());
    }


    /**
     * Test getting all activities that are shared with friends only.
     */
    @Test
    void getActivitiesDifferentPrivacyLevelTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        service.editActivityPrivacy("restricted", activity.getId(), profile.getId());
        assertTrue(service.getActivitiesWithPrivacyLevel(ActivityPrivacy.PUBLIC).isEmpty());
    }

    /**
     * Tests sharing with an invalid privacy level throws an error.
     */
    @Test
    void editInvalidPrivacyActivitiesTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Profile profile = profileRepository.save(createNormalProfileBen());
        assertThrows(IllegalArgumentException.class, ()->service.editActivityPrivacy("everyone", activity.getId(),profile.getId()));
    }
    /**
     * Ensures an activity with no relationships throws an exception
     */
    @Test
    void getActivityRoleCountWithZeroRolesTest(){
        Activity activity = activityRepository.save(createNormalActivity());
        assertThrows(IllegalArgumentException.class, ()->service.getRoleCounts(activity.getId()));
    }
    /**
     * Ensures a non existent activity throws an exception
     */
    @Test
    void getActivityRoleCountOfNonExistentActivityTest(){
        assertThrows(IllegalArgumentException.class, ()->service.getRoleCounts(-1));
    }

    /**
     * Ensures an activity with a creator returns the correct number
     */
    @Test
    void getActivityRoleCountWithCreatorTest(){
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile creator = profileRepository.save(createNormalProfileBen());
        activityMembershipRepository.save(new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR));
        assertEquals(new ActivityRoleCountResponse(0, 0 ,0), service.getRoleCounts(activity.getId()));
    }

    /**
     * Ensures an activity with multiple roles returns the correct number
     */
    @Test
    void getActivityRoleCountWithMultipleRolesTest(){
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile follower = profileRepository.save(createNormalProfileBen());
        Profile participant = profileRepository.save(createNormalProfileBen());
        activityMembershipRepository.save(new ActivityMembership(activity, creator, ActivityMembership.Role.ORGANISER));
        activityMembershipRepository.save(new ActivityMembership(activity, participant, ActivityMembership.Role.PARTICIPANT));
        activityMembershipRepository.save(new ActivityMembership(activity, follower, ActivityMembership.Role.FOLLOWER));
        assertEquals(new ActivityRoleCountResponse(1, 1, 1), service.getRoleCounts(activity.getId()));
    }

    /**
     * Test that a FOLLOWER cannot change the role to an ORGANISER.
     * Throws an exception error
     */
    @Test
    void setProfileRoleToOrganiserAsFollowerThrowsIllegalArgumentExceptionTest() {
        Profile followerBen = profileRepository.save(createNormalProfileBen());
        Profile followerJohnny = profileRepository.save(createNormalProfileJohnny());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, followerBen, ActivityMembership.Role.FOLLOWER);
        ActivityMembership followerMembership = new ActivityMembership(activity, followerJohnny, ActivityMembership.Role.FOLLOWER);
        activityMembershipRepository.save(creatorMembership);
        activityMembershipRepository.save(followerMembership);
        assertThrows(IllegalArgumentException.class, ()-> service.setProfileRole(followerBen.getId(), followerJohnny.getId(), activity.getId(), ActivityMembership.Role.ORGANISER));
    }

    /**
     * Tests that an exception error is thrown when attempting to change a role of a non existent user.
     */
    @Test
    void setProfileRoleForNonexistentMembershipThrowsIllegalArgumentExceptionTest() {
        Profile editor = profileRepository.save(createNormalProfileBen());
        assertThrows(IllegalArgumentException.class, ()-> service.setProfileRole(0, editor.getId(), 3, ActivityMembership.Role.FOLLOWER));
    }

    /**
     * Tests that setting a role to an ORGANISER as a CREATOR works.
     */
    @Test
    void setProfileRoleToOrganiserTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile follower = profileRepository.save(createNormalProfileJohnny());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        ActivityMembership followerMembership = new ActivityMembership(activity, follower, ActivityMembership.Role.FOLLOWER);
        activityMembershipRepository.save(creatorMembership);
        activityMembershipRepository.save(followerMembership);

        service.setProfileRole(follower.getId(), creator.getId(), activity.getId(), ActivityMembership.Role.ORGANISER);
        Optional<ActivityMembership> updatedMembership = activityMembershipRepository.findByActivity_IdAndProfile_Id(activity.getId(), follower.getId());
        if (updatedMembership.isEmpty()) {
            fail("Test membership could not be found");
        } else {
            assertEquals(ActivityMembership.Role.ORGANISER, updatedMembership.get().getRole());
        }
    }

    /**
     * Tests that setting a role to an ORGANISER as an ADMIN works.
     */
    @Test
    void setProfileRoleToOrganiserAsAdmin() {
        Profile admin = profileRepository.save(createNormalProfileBen());
        admin.setAuthLevel(1);
        Profile follower = profileRepository.save(createNormalProfileJohnny());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership membership = new ActivityMembership(activity, follower, ActivityMembership.Role.FOLLOWER);
        activityMembershipRepository.save(membership);
        service.setProfileRole(follower.getId(), admin.getId(), activity.getId(), ActivityMembership.Role.ORGANISER);
        assertEquals(ActivityMembership.Role.ORGANISER,
                activityMembershipRepository.findByActivity_IdAndProfile_Id(activity.getId(), follower.getId()).get().getRole());
    }

    /**
     * Tests that setting a role to an ORGANISER as a CREATOR works.
     */
    @Test
    void setProfileRoleToOrganiserAsCreator() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile follower = profileRepository.save(createNormalProfileJohnny());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership followerMembership = new ActivityMembership(activity, follower, ActivityMembership.Role.FOLLOWER);
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(followerMembership);
        activityMembershipRepository.save(creatorMembership);
        service.setProfileRole(follower.getId(), creator.getId(), activity.getId(), ActivityMembership.Role.ORGANISER);
        assertEquals(ActivityMembership.Role.ORGANISER,
                activityMembershipRepository.findByActivity_IdAndProfile_Id(activity.getId(), follower.getId()).get().getRole());
    }

    /**
     *  Tests that setting a profile role to CREATOR as an invalid profile doesn't work.
     */
    @Test
    void setProfileRoleToCreatorThrowsIllegalArgumentExceptionTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile follower = profileRepository.save(createNormalProfileJohnny());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        ActivityMembership followerMembership = new ActivityMembership(activity, follower, ActivityMembership.Role.FOLLOWER);
        activityMembershipRepository.save(creatorMembership);
        activityMembershipRepository.save(followerMembership);

        assertThrows(IllegalArgumentException.class, ()-> service.setProfileRole(follower.getId(), 1, activity.getId(), ActivityMembership.Role.CREATOR));
    }

    /**
     * Tests you cannot change the role of the CREATOR of an activity.
     */
    @Test
    void setProfileRoleFromCreatorThrowsIllegalArgumentExceptionTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(creatorMembership);

        assertThrows(IllegalArgumentException.class, ()-> service.setProfileRole(creator.getId(), creator.getId(), activity.getId(), ActivityMembership.Role.FOLLOWER));
    }



    @Test
    void getProfilesFromActivityWithOnlyCreatorTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        emailRepository.save(new Email("ben10@hotmail.com", true, creator));
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(creatorMembership);
        List<ActivityMemberProfileResponse> response = Collections.singletonList(new ActivityMemberProfileResponse(creator.getId(), creator.getFirstname(), creator.getLastname(), creator.getPrimary_email(), ActivityMembership.Role.CREATOR));
        assertEquals(response, service.getActivityMembers(activity.getId()));
    }

    /**
     * Test the service method for getSingleActivityMembership
     */
    @Test
    void getSingleActivityMembershipTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        emailRepository.save(new Email("ben10@hotmail.com", true, creator));
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(creatorMembership);
        String role = service.getSingleActivityMembership(creator.getId(), activity.getId());
        assertEquals("creator", role);
    }


    /**
     * Ensures getting multiple profiles linked to an activity works as expected
     */
    @Test
    void getProfilesWithRolesFromActivityWithMultipleRolesTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile followerOne = profileRepository.save(createNormalProfileBen("ben11@hotmail.com"));
        Profile followerTwo = profileRepository.save(createNormalProfileBen("ben12@hotmail.com"));
        Profile organiser = profileRepository.save(createNormalProfileBen("ben13@hotmail.com"));
        Profile participant = profileRepository.save(createNormalProfileBen("ben14@hotmail.com"));
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        emailRepository.save(new Email("ben10@hotmail.com", true, creator));
        emailRepository.save(new Email("ben11@hotmail.com", true, followerOne));
        emailRepository.save(new Email("ben12@hotmail.com", true, followerTwo));
        emailRepository.save(new Email("ben13@hotmail.com", true, organiser));
        emailRepository.save(new Email("ben14@hotmail.com", true, participant));
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        ActivityMembership followerOneMembership = new ActivityMembership(activity, followerOne, ActivityMembership.Role.FOLLOWER);
        ActivityMembership followerTwoMembership = new ActivityMembership(activity, followerTwo, ActivityMembership.Role.FOLLOWER);
        ActivityMembership organiserMembership = new ActivityMembership(activity, organiser, ActivityMembership.Role.ORGANISER);
        ActivityMembership participantMembership = new ActivityMembership(activity, participant, ActivityMembership.Role.PARTICIPANT);
        List<ActivityMembership> memberships = Arrays.asList(creatorMembership, followerOneMembership, followerTwoMembership, organiserMembership, participantMembership);
        activityMembershipRepository.saveAll(memberships);
        List<ActivityMemberProfileResponse> response = new ArrayList<>();
        for(ActivityMembership membership: memberships){
            response.add(new ActivityMemberProfileResponse(membership.getProfile().getId(), membership.getProfile().getFirstname(), membership.getProfile().getLastname(), membership.getProfile().getPrimary_email(), membership.getRole()));
        }
        assertEquals(response, service.getActivityMembers(activity.getId()));
    }

    @Test
    void getProfilesFromNonExistentActivityTest() {
        assertThrows(IllegalArgumentException.class, () -> service.getActivityMembers(-1));
    }

    @Test
    void getActivityMembersByRoleNormalTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile followerOne = profileRepository.save(createNormalProfileBen());
        Profile followerTwo = profileRepository.save(createNormalProfileBen());
        Profile organiser = profileRepository.save(createNormalProfileBen());
        Profile participant = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        ActivityMembership followerOneMembership = new ActivityMembership(activity, followerOne, ActivityMembership.Role.FOLLOWER);
        ActivityMembership followerTwoMembership = new ActivityMembership(activity, followerTwo, ActivityMembership.Role.FOLLOWER);
        ActivityMembership organiserMembership = new ActivityMembership(activity, organiser, ActivityMembership.Role.ORGANISER);
        ActivityMembership participantMembership = new ActivityMembership(activity, participant, ActivityMembership.Role.PARTICIPANT);
        List<ActivityMembership> memberships = Arrays.asList(creatorMembership, followerOneMembership, followerTwoMembership, organiserMembership, participantMembership);
        activityMembershipRepository.saveAll(memberships);

        Pageable pageable = PageRequest.of(0, 2);
        List<Profile> expectedProfiles = new ArrayList<>();
        expectedProfiles.add(followerOne);
        expectedProfiles.add(followerTwo);
        Page<Profile> actualProfiles = service.getActivityMembersByRole(activity.getId(), ActivityMembership.Role.FOLLOWER, pageable);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()));
        assertEquals(expectedProfiles.size(),actualProfiles.getSize());
    }

    @Test
    void deleteMembersFromActivityAsAdminTest(){

    }

    @Test
    void getActivityMembersByRoleWithPaginationNormalTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);

        Map<Long, Profile> followers = new HashMap<>();
        for (int i = 0; i < 10; i++) {
            Profile follower = profileRepository.save(createNormalProfileBen());
            ActivityMembership membership = new ActivityMembership(activity, follower, ActivityMembership.Role.FOLLOWER);
            activityMembershipRepository.save(membership);
            followers.put(follower.getId(), follower);
        }
        int pageSize = 2;
        int index = 3;
        Pageable pageable = PageRequest.of(index, pageSize);
        Page<Profile> actual = service.getActivityMembersByRole(activity.getId(), ActivityMembership.Role.FOLLOWER, pageable);
        assertEquals(pageSize, actual.getNumberOfElements());
    }

    @Test
    void getActivityMembersByRoleWithNoMembersOfThatRoleTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile participant = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        ActivityMembership participantMembership = new ActivityMembership(activity, participant, ActivityMembership.Role.PARTICIPANT);
        List<ActivityMembership> memberships = Arrays.asList(creatorMembership, participantMembership);
        activityMembershipRepository.saveAll(memberships);

        Pageable pageable = PageRequest.of(0, 2);
        Page<Profile> actual = service.getActivityMembersByRole(activity.getId(), ActivityMembership.Role.FOLLOWER, pageable);
        assertTrue(actual.isEmpty());
    }

    @Test
    void getActivityMembersWithRoleWithNonExistentActivityTest() {
        assertThrows(IllegalArgumentException.class, () ->
                service.getActivityMembersByRole(-1, ActivityMembership.Role.CREATOR, null));
    }

    @Test
    void clearRolesOfActivityThatDoesntExistThrowsExceptionTest(){
        assertThrows(IllegalArgumentException.class, ()-> service.clearActivityRoleList(915730971L, "FOLLOWER"));
    }

    /**
     *  Tests you can get an participation object that exists in the database using the readParticipation method.
     */
    @Test
    void successfullyReadParticipationWhereParticipationExistsTest() {
        ActivityParticipation participation = createNormalParticipationWithoutProfileActivity();
        activityParticipationRepository.save(participation);
        assertEquals(participation, service.readParticipation(participation.getId()));
    }

    /**
     *  Tests the readParticipation method that it throws an error when a participation with the given id does not exist
     *  in the database.
     */
    @Test
    void readParticipationWhereParticipationDoesNotExistThrowsError1Test() {
        assertThrows(IllegalArgumentException.class, ()->service.readParticipation((long)1));
    }
    @Test
    void readParticipationWhereParticipationDoesNotExistThrowsError2Test() {
        ActivityParticipation participation = createNormalParticipationWithoutProfileActivity();
        activityParticipationRepository.save(participation);
        assertThrows(IllegalArgumentException.class, ()->service.readParticipation(participation.getId()+1));
    }

    /**
     * Example participations to use in tests
     */

    public static ActivityParticipation createNormalParticipationWithoutProfileActivity() {
        return new ActivityParticipation("The final score was 2 - 1.", "University Wins", "2020-02-20T08:00:00+1300",
                "2020-02-20T10:15:00+1300");
    }

    /**
     * Example activities to use in tests
     **/

    public static Activity createNormalActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Tramping", "Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    public Activity createNormalActivityKaikoura() {
        Activity activity =  new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Hiking"}, false, "2020-02-20T08:00:00+1300",
                "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
        Set<ActivityType> updatedActivityType = new HashSet<>();
        for(ActivityType activityType : activity.retrieveActivityTypes()){
            List<ActivityType> resultActivityTypes = typeRepository.findByActivityTypeName(activityType.getActivityTypeName());{
                updatedActivityType.add(resultActivityTypes.get(0));
            }
        }
        activity.setActivityTypes(updatedActivityType);
        return activity;
    }

    private Activity createNormalActivitySilly() {
        return new Activity("Wibble", "A bald man", new String[]{"Hockey"}, true,
                "2020-02-20T08:00:00+1300","2020-02-20T08:00:00+1300", "K2");
    }

    private Activity createBadActivityNoName() {
        Activity activity = createNormalActivityKaikoura();
        activity.setActivityName(null);
        return activity;
    }

    private Activity createBadActivityBlankName() {
        Activity activity = createNormalActivityKaikoura();
        activity.setActivityName("");
        return activity;
    }

    private Activity createBadActivityDurationAndNoStartDate() {
        Activity activity = createNormalActivityKaikoura();
        activity.setStartTime(null);
        return activity;
    }

    private Activity createBadActivityDurationAndNoEndDate() {
        Activity activity = createNormalActivityKaikoura();
        activity.setEndTime(null);
        return activity;
    }

    private Activity createBadActivityMisorderedDates() {
        Activity activity = createNormalActivityKaikoura();
        activity.setEndTime(FormatHelper.parseOffsetDateTime("2020-01-20T08:00:00+1300"));
        return activity;
    }

    private Activity createBadActivityNoActivityTypes() {
        return new Activity("", "A big and nice race on a lovely peninsula",null, false,
                "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private Activity createBadActivityEmptyActivityTypes() {
        return new Activity("", "A big and nice race on a lovely peninsula", new String[]{},
                false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private Activity createBadActivityInvalidActivityTypes() {
        return new Activity("", "A big and nice race on a lovely peninsula", new String[]{"nugts"},
                false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    /**
     * Saves original activity to repo, then applies update and returns the updated activity.
     * @param original the original activity to save to the repo
     * @param update the update to be applied to the original activity
     * @return The updated activity from repository
     */
    private Activity updateAndGetResult(Activity original, Activity update) {
        activityRepository.save(original);
        Long activityId = activityRepository.getLastInsertedId();
        Activity actualActivity = null;
        Profile profile = profileRepository.save(createNormalProfileBen());

        service.update(update, activityId, profile.getId());
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        return actualActivity;
    }


    public static Profile createNormalProfileBen() {

        return new Profile(null, "Ben", "Sales", "James", "Ben10", "ben10@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "Wooooooow", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    public static Profile createNormalProfileBen(String email) {

        return new Profile(null, "Ben", "Sales", "James", "Ben10", email, new String[]{"additional@email.com"}, "hushhush",
                "Wooooooow", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    public static Profile createNormalProfileJohnny() {
        return new Profile(null, "Johnny", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    /**
     * @return a valid profile object.
     */
    public static Profile createNormalProfileMim() {
        return new Profile(null, "Mim", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }

    private Map<Long, Profile> populateProfiles() {
        Profile johnny = createNormalProfileJohnny(),
                mim = createNormalProfileMim();
        Map<Long, Profile> map = new HashMap<>();
        profileRepository.save(johnny);
        map.put(profileRepository.getLastInsertedId(), johnny);
        profileRepository.save(mim);
        map.put(profileRepository.getLastInsertedId(), mim);
        return map;
    }
}