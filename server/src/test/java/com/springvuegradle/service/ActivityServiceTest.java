package com.springvuegradle.service;

import com.springvuegradle.controller.ActivityController;
import com.springvuegradle.dto.responses.ActivityMemberProfileResponse;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.ActivityType;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.*;
import com.springvuegradle.utilities.FormatHelper;
import com.springvuegradle.utilities.InitialDataHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

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

    private static final String MISSING_EXCEPTION = "Exception should have been thrown.";

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
        activityMembershipRepository.deleteAll();
        emailRepository.deleteAll();
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
        service.update(expectedActivity, activityId);
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
        assertThrows(IllegalArgumentException.class, ()-> service.update(createNormalActivityKaikoura(), 0L));
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
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        service.delete(activity.getId());
        assertEquals(0, activityRepository.count());
    }

    /**
     * Test to remove a profiles membership from an activity they have membership with
     **/
    @Test
    void removeActivityMemberShipSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        profileRepository.save(bennyBoi);
        ActivityMembership testMemberShip = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.PARTICIPANT);
        activityMembershipRepository.save(testMemberShip);
        service.removeMembership(bennyBoi.getId(), activity.getId());
        assertEquals(0, activityMembershipRepository.count());
    }

    /**
     * Test to remove a profiles membership from an activity which they are not a part of
     */
    @Test
    void removeActivityMemberShipFailTest() {
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile bennyBoi = createNormalProfileBen();
        profileRepository.save(bennyBoi);
        Profile johnnyBoi = createNormalProfileJohnny();
        profileRepository.save(johnnyBoi);
        ActivityMembership testMemberShip = new ActivityMembership(activity, bennyBoi, ActivityMembership.Role.PARTICIPANT);
        activityMembershipRepository.save(testMemberShip);
        service.removeMembership(johnnyBoi.getId(), activity.getId());
        assertEquals(1, activityMembershipRepository.count());
    }

    /**
     * Test to delete an activity that doesn't exist
     **/
    @Test
    void deleteActivityDoesNotExistTest() {
        assertFalse(service.delete((long) 1));
    }

    @Test
    void getActivityByIdServiceTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        Activity activityResult = service.getActivityByActivityId(activity.getId());
        assertEquals(activity, activityResult);
    }

    @Test
    void getActivityByIdFailedTest() {
        long activityId = 10;
        Activity failedResult = service.getActivityByActivityId(activityId);
        assertEquals(null, failedResult);
    }

    void addNormalUserRoleToActivityTest() {
        Profile ben = createNormalProfileBen();
        Profile profile = profileRepository.save(ben);
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        service.addActivityRole(activity.getId(), profile.getId(), "participant");
        assertEquals(1, activityMembershipRepository.findActivityMembershipsByActivity_IdAndRole(activity.getId(), ActivityMembership.Role.PARTICIPANT).size());
    }

    @Test
    void creatorAddsOrganiserRoleToActivityTest() {
        Profile ben = profileRepository.save(createNormalProfileBen());
        Profile johnny = profileRepository.save(createNormalProfileJohnny());
        controller.createActivity(ben.getId(), createNormalActivityKaikoura(), null, true);
        service.addActivityRole(activityRepository.getLastInsertedId(), johnny.getId(), "organiser");
        assertEquals(1, activityMembershipRepository.findActivityMembershipsByActivity_IdAndRole(activityRepository.getLastInsertedId(), ActivityMembership.Role.ORGANISER).size());
    }

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

    @Test
    void setProfileRoleToOrganizerAsFollowerThrowsIllegalArgumentExceptionTest() {
        Profile followerBen = profileRepository.save(createNormalProfileBen());
        Profile followerJohnny = profileRepository.save(createNormalProfileJohnny());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, followerBen, ActivityMembership.Role.FOLLOWER);
        ActivityMembership followerMembership = new ActivityMembership(activity, followerJohnny, ActivityMembership.Role.FOLLOWER);
        activityMembershipRepository.save(creatorMembership);
        activityMembershipRepository.save(followerMembership);
        assertThrows(IllegalArgumentException.class, ()-> service.setProfileRole(followerBen.getId(), followerJohnny.getId(), activity.getId(), ActivityMembership.Role.ORGANISER));
    }

    @Test
    void setProfileRoleToOrganizerAsAdmin() {
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

    @Test
    void setProfileRoleToOrganizerAsCreator() {
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

    @Test
    void setProfileRoleFromCreatorThrowsIllegalArgumentExceptionTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(creatorMembership);

        assertThrows(IllegalArgumentException.class, ()-> service.setProfileRole(creator.getId(), creator.getId(), activity.getId(), ActivityMembership.Role.FOLLOWER));
    }

    @Test
    void setProfileRoleForNonexistentMembershipThrowsIllegalArgumentExceptionTest() {
        Profile editor = profileRepository.save(createNormalProfileBen());
        assertThrows(IllegalArgumentException.class, ()-> service.setProfileRole(0, editor.getId(), 3, ActivityMembership.Role.FOLLOWER));
    }

    @Test
    void getProfilesFromActivityWithOnlyCreatorTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        activityMembershipRepository.save(creatorMembership);
        List<ActivityMemberProfileResponse> response = Arrays.asList(new ActivityMemberProfileResponse(creator.getId(), creator.getFirstname(), creator.getLastname(), ActivityMembership.Role.CREATOR));
        assertEquals(response, service.getActivityMembers(activity.getId()));
    }

    @Test
    /**
     * Ensures getting multiple profiles linked to an activity works as expected
     */
    void getProfilesWithRolesFromActivityWithMultipleRolesTest() {
        Profile creator = profileRepository.save(createNormalProfileBen());
        Profile followerOne = profileRepository.save(createNormalProfileBen());
        Profile followerTwo = profileRepository.save(createNormalProfileBen());
        Profile organizer = profileRepository.save(createNormalProfileBen());
        Profile participant = profileRepository.save(createNormalProfileBen());
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        ActivityMembership creatorMembership = new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR);
        ActivityMembership followerOneMembership = new ActivityMembership(activity, followerOne, ActivityMembership.Role.FOLLOWER);
        ActivityMembership followerTwoMembership = new ActivityMembership(activity, followerTwo, ActivityMembership.Role.FOLLOWER);
        ActivityMembership organizerMembership = new ActivityMembership(activity, organizer, ActivityMembership.Role.ORGANISER);
        ActivityMembership participantMembership = new ActivityMembership(activity, participant, ActivityMembership.Role.PARTICIPANT);
        List<ActivityMembership> memberships = Arrays.asList(creatorMembership, followerOneMembership, followerTwoMembership, organizerMembership, participantMembership);
        activityMembershipRepository.saveAll(memberships);
        List<ActivityMemberProfileResponse> response = new ArrayList<>();
        for(ActivityMembership membership: memberships){
            response.add(new ActivityMemberProfileResponse(membership.getProfile().getId(), membership.getProfile().getFirstname(), membership.getProfile().getLastname(), membership.getRole()));
        }
        assertEquals(response, service.getActivityMembers(activity.getId()));
    }

    @Test
    void getProfilesFromNonExistentActivityTest() {
        assertThrows(IllegalArgumentException.class, () -> service.getActivityMembers(-1));
    }


    /**
     * Example activities to use in tests
     **/
    static Activity createNormalActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Tramping", "Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ");
    }

    private Activity createNormalActivityKaikoura() {
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

        service.update(update, activityId);
        Optional<Activity> result = activityRepository.findById(activityId);
        if (result.isPresent()) {
            actualActivity = result.get();
        } else {
            fail("Error: original activity is missing");
        }
        return actualActivity;
    }


    static Profile createNormalProfileBen() {
        return new Profile(null, "Ben", "Sales", "James", "Ben10", "ben10@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "Wooooooow", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    static Profile createNormalProfileJohnny() {
        return new Profile(null, "Johnny", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{});
    }

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileMim() {
        return new Profile(null, "Mim", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }
}