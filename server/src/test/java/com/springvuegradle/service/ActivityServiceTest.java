package com.springvuegradle.service;

import com.springvuegradle.controller.ActivityController;
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

    private Map<Long, Profile> profileIds = null;
    private static final String MISSING_EXCEPTION = "Exception should have been thrown.";

    /**
     * Needs to be run before each test to create new test profiles and repositories.
     */
    @BeforeEach
    void setUp() {
        profileIds = populateProfiles();
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

        assertEquals(result.get(0).getActivityName(), "Kaikoura Coast Track race");
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
        assertThrows(IllegalArgumentException.class, ()->{ service.update(createNormalActivityKaikoura(), 0L);});
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
    void editActivityPrivacyToPublicTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        service.editActivityPrivacy("public", activity.getId());
        assertEquals(2, activity.getPrivacyLevel());
    }

    @Test
    void editActivityPrivacyToFriendsTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        service.editActivityPrivacy("friends", activity.getId());
        assertEquals(1, activity.getPrivacyLevel());
    }

    @Test
    void editActivityPrivacyToPrivateTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        service.editActivityPrivacy("private", activity.getId());
        assertEquals(0, activity.getPrivacyLevel());
    }

    @Test
    void getActivitiesByProfileIdByRoleTest() {
        Profile ben = profileRepository.save(createNormalProfileBen());
        controller.createActivity(ben.getId(),createNormalActivity(), null, true);
        long id = 0;
        List<Activity> list = service.getActivitiesByProfileIdByRole(ben.getId(), "creator");
        assertEquals(1, list.size());
    }

    @Test
    void getPublicActivitiesSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        service.editActivityPrivacy("public", activity.getId());
        assertEquals(1, service.getActivitiesWithPrivacyLevel("public").size());
    }

    @Test
    void getPrivateActivitiesSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        service.editActivityPrivacy("private", activity.getId());
        assertEquals(1, service.getActivitiesWithPrivacyLevel("private").size());
    }

    @Test
    void getFriendsActivitiesSuccessTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        service.editActivityPrivacy("friends", activity.getId());
        assertEquals(1, service.getActivitiesWithPrivacyLevel("friends").size());
    }

    @Test
    void getActivitiesDifferentPrivacyLevelTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        service.editActivityPrivacy("friends", activity.getId());
        assertEquals(0, service.getActivitiesWithPrivacyLevel("public").size());
    }

    @Test
    void editInvalidPrivacyActivitiesTest() {
        Activity activity = activityRepository.save(createNormalActivity());
        assertThrows(IllegalArgumentException.class, ()->service.editActivityPrivacy("everyone", activity.getId()));
    }
    /**
     * Ensures an activity with no relationships returns the correct number
     */
    @Test
    void getActivityRoleCountWithZeroRolesTest(){
        Activity activity = activityRepository.save(createNormalActivity());
        assertEquals(0, service.getRoleCount(activity.getId()));
    }

    /**
     * Ensures an activity with a creator returns the correct number
     */
    @Test
    void getActivityRoleCountWithCreatorTest(){
        Activity activity = activityRepository.save(createNormalActivityKaikoura());
        Profile creator = profileRepository.save(createNormalProfileBen());
        activityMembershipRepository.save(new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR));
        assertEquals(1, service.getRoleCount(activity.getId()));
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
        activityMembershipRepository.save(new ActivityMembership(activity, creator, ActivityMembership.Role.CREATOR));
        activityMembershipRepository.save(new ActivityMembership(activity, participant, ActivityMembership.Role.PARTICIPANT));
        activityMembershipRepository.save(new ActivityMembership(activity, follower, ActivityMembership.Role.FOLLOWER));
        assertEquals(3, service.getRoleCount(activity.getId()));
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
        Set<ActivityType> updatedActivityType = new HashSet<ActivityType>();
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