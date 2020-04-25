package com.springvuegradle.Controller;

import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.PassportCountry;
import com.springvuegradle.Model.Profile;

import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.PassportCountryRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.dto.ChangePasswordRequest;
import com.springvuegradle.dto.EmailAddRequest;
import com.springvuegradle.dto.EmailUpdateRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the Profile_Controller class, these tests are run separate from the actual repository. Spring Boot configures
 * an in-memory H2 database specifically for testing.
 * @author Hamesh Ravji
 */
@ExtendWith(SpringExtension.class)
@DataJpaTest
class ProfileControllerTest {

    @Autowired
    private ProfileRepository repo;

    @Autowired
    private EmailRepository erepo;

    @Autowired
    private ActivityTypeRepository arepo;

    @Autowired
    private PassportCountryRepository pcrepo;
    @Autowired
    private Profile_Controller profileController;

    /**
     * This tests to ensure profiles structured correctly can be added to the database.
     */
    @Test
    void createProfileTest() {
        Profile jimmy = createNormalProfileJimmy();
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile maurice = createNormalProfileMaurice();
        ResponseEntity<String> response_entity_new = profileController.createProfile(maurice);
        assertEquals(HttpStatus.CREATED, response_entity_new.getStatusCode());

        expected_in_repo = 2;
        assertEquals(expected_in_repo, repo.count());

        Profile dummyJimmy = createInvalidCountryProfileJimmy();
        ResponseEntity<String> response_entity_dummy = profileController.createProfile(dummyJimmy);
        assertEquals(HttpStatus.FORBIDDEN, response_entity_dummy.getStatusCode());

        expected_in_repo = 2;
        assertEquals(expected_in_repo, repo.count());
    }

    @Test
    void testCreateProfileWithActivityTypes(){
        Profile profile = createProfileWithActivityTypes();
        ResponseEntity<String> response_entity = profileController.createProfile(profile);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());
    }

    @Test
    void testCreateProfileWithInvalidActivityTypes(){
        Profile profile = createProfileWithInvalidActivityTypes();
        ResponseEntity<String> response_entity = profileController.createProfile(profile);
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());
    }

    @Test
    void testCreateProfileWithMinimalFields() {
        Profile testProfile = createProfileWithMinimalFields();
        assertEquals(0, repo.count(), "Sanity check: profile repository is empty");
        assertEquals(0, erepo.count(), "Sanity check: email repository is empty");
        ResponseEntity<String> responseEntity = profileController.createProfile(testProfile);

        System.out.println(responseEntity.getBody());

        List<Profile> result = repo.findAll();
        assertTrue(result.contains(testProfile));
    }

    /**
     * This method tests that the profile without the mandatory fields filled in is not saved to the database.
     */
    @Test
    void createProfileWithoutMandatoryFieldsTest() {
        Profile dummy_maurice = createInvalidFieldsProfileMaurice();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(dummy_maurice);
        System.out.println(response_entity.getBody());
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());
        String actual_error_message = response_entity.getBody();
        String expected_error_message = "The email field is blank.\n" +
                "The First Name field is blank.\n" +
                "The Last Name field is blank.\n" +
                "The Password is not long enough.\n" +
                "The fitness level isn't valid.\n" +
                "ActivityType random does not exist in the database.\n" +
                "The Gender field must contain either 'male', 'female' or 'non-Binary'.\n";
        assertEquals(expected_error_message, actual_error_message);
        assertEquals(expected_in_repo, repo.count());
    }

    /**
     * This test simulates a user trying to create an account with an email address which is already being used on
     * another account.
     */
    @Test
    void createProfileExistingEmailTest() {
        Profile jimmy = createNormalProfileJimmy();
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity_jimmy = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity_jimmy.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile dup_jimmy = createNormalProfileJimmy();

        ResponseEntity<String> response_entity_dup_jimmy = profileController.createProfile(dup_jimmy);
        assertEquals(HttpStatus.FORBIDDEN, response_entity_dup_jimmy.getStatusCode());

        assertEquals(expected_in_repo, repo.count());

        String actual_error_message = response_entity_dup_jimmy.getBody();
        String expected_error_message = "An email address you have entered is already in use by another Profile.\n";
        assertEquals(expected_error_message, actual_error_message);
    }

    /**
     * This method tests that a profile filled correctly where the email address does not exist already in the database
     * is saved to the database.
     */
    @Test
    void testGetProfileNormal() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        long expected_id = repo.findAll().get(0).getId();

        ResponseEntity<Profile> response_entity_new = profileController.getProfile(expected_id);
        assertEquals(HttpStatus.OK, response_entity_new.getStatusCode());
        Profile db_jimmy = response_entity_new.getBody();
        assertEquals(jimmy, db_jimmy);
    }

    /**
     * Tests to see what response status code is when trying to get a profile that does not exist.
     */
    @Test
    void getProfileDoesNotExistTest() {
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());
        ResponseEntity<Profile> response_entity = profileController.getProfile(1L);
        assertEquals(HttpStatus.NOT_FOUND, response_entity.getStatusCode());
    }

    /**
     * Tests adding emails to a profile.
     */
    @Test
    void addEmailToProfileTest() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ResponseEntity<String> add_emails_response = profileController.addEmails(new EmailAddRequest(new ArrayList<String>(Collections.singleton("randomEmail@gmail.com"))), db_profile.getId(), null, true);
        assertEquals(HttpStatus.CREATED, add_emails_response.getStatusCode());

        db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        List<String> emails_from_db_profile = new ArrayList<>(db_profile.getAdditional_email());

        assertEquals("randomEmail@gmail.com", emails_from_db_profile.get(0));
    }

    /**
     * Tests to make sure changing the activityTypes works
     */
    @Test
    void changeActivityTypesTest() {
        Profile originalProfile = createNormalActivityTypesProfile();
        profileController.createProfile(originalProfile);
        long profileId = repo.findByPrimaryEmail(originalProfile.getPrimary_email()).get(0).getId();
        Profile expectedProfile = repo.findById(profileId).get();

        Profile updatedProfile = createUpdatedActivityTypesProfile();
        assertEquals(expectedProfile.getActivityTypes().size(), 3, "Check profile saved successfully");
        profileController.updateProfile(updatedProfile, profileId);

        expectedProfile = repo.findById(profileId).get();
        assertEquals(expectedProfile.getActivityTypes().size(), 1, "Check activityTypes updated successfully");
        assertEquals(expectedProfile.getActivityTypes(), updatedProfile.getActivityTypes(), "Check activityTypes updated successfully");
    }

//    @Test
//    void testEditProfileNormal(){
//        Profile testProfile = createNormalProfileJimmy();
//        Profile updateData = createNormalProfileMaurice();
//        Profile expectedProfile = createNormalProfileMaurice();
//        Set<PassportCountry> realPassports = new HashSet<>();
//        for (PassportCountry passportCountry: expectedProfile.getPassportObjects()){
//            realPassports.add(pcrepo.findByCountryName(passportCountry.getCountryName()).get(0));
//        }
//        expectedProfile.setPassword(profileController.hashPassword(testProfile.getPassword()));
//        expectedProfile.setPassports(realPassports);
//
//        Set<ActivityType> realActivityTypes = new HashSet<>();
//        for (ActivityType activityType: expectedProfile.getActivityTypeObjects()){
//            realActivityTypes.add(arepo.findByActivityTypeName(activityType.getActivityTypeName()).get(0));
//        }
//        expectedProfile.setActivityTypes(realActivityTypes);
//
//        profileController.createProfile(testProfile);
//        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();
//        assertEquals(testProfile, repo.findById(id).get(), "Sanity check: profile and ID saved successfully");
//
//        ResponseEntity<String> actualResponse = profileController.updateProfile(updateData, id);
//
//        Profile updatedProfile = repo.findById(id).get();
//        assertEquals(expectedProfile, updatedProfile, "Check profile updated successfully");
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//    }

    @Test
    void testEditProfileWithInvalidData(){
        Profile testProfile = createNormalProfileJimmy();
        Profile updateData = createInvalidFieldsProfileMaurice();
        Profile expectedProfile = createNormalProfileJimmy();
        Set<PassportCountry> realPassports = new HashSet<>();
        for (PassportCountry passportCountry: expectedProfile.getPassportObjects()){
            realPassports.add(pcrepo.findByCountryName(passportCountry.getCountryName()).get(0));
        }
        expectedProfile.setPassword(profileController.hashPassword(testProfile.getPassword()));
        expectedProfile.setPassports(realPassports);
        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();
        assertEquals(testProfile, repo.findById(id).get(), "Sanity check: profile and ID saved successfully");

        ResponseEntity<String> actualResponse = profileController.updateProfile(updateData, id);

        Profile updatedProfile = repo.findById(id).get();
        assertEquals(expectedProfile, updatedProfile, "Check profile left unchanged");
        assertEquals(HttpStatus.BAD_REQUEST, actualResponse.getStatusCode());
    }

    @Test
    void testAddEmailsNormal(){
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();
        ResponseEntity<String> expectedResponse = new ResponseEntity<>("Emails added successfully.", HttpStatus.CREATED);
        assertEquals(testProfile, repo.findById(id).get(), "Sanity check: profile and ID saved successfully");

        List<String> newEmails = Arrays.asList("newEmail@xtra.co.nz", "newEmail2@yahoo.co.nz");
        Set<String> expectedAdditionalEmails = new HashSet<String>(testProfile.getAdditional_email());
        expectedAdditionalEmails.addAll(newEmails);
        EmailAddRequest testRequest = new EmailAddRequest(newEmails);

        ResponseEntity<String> actualResponse =  profileController.addEmails(testRequest, id, null, true);

        Profile updatedProfile = repo.findById(id).get();
        assertEquals(testProfile.getPrimary_email(), updatedProfile.getPrimary_email(), "Check that the primary email is unchanged");
        assertEquals(expectedAdditionalEmails, updatedProfile.getAdditional_email(), "Check that the emails have been added successfully");
        assertEquals(expectedResponse, actualResponse, "Check response has correct message and status code (201).");
    }

    @Test
    void testEditProfileActivityTypes(){
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        ArrayList<String> newActivityTypes = new ArrayList<String>(Arrays.asList("Football", "Tennis"));
        assertEquals(profileController.editActivityTypes(newActivityTypes, testProfile.getId()), new ResponseEntity<>(HttpStatus.OK));
    }

    @Test
    void testEditInvalidProfileActivityTypes(){
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        ArrayList<String> newActivityTypes = new ArrayList<String>(Arrays.asList("Not real ActivityType", "Tennis"));
        assertEquals(profileController.editActivityTypes(newActivityTypes, testProfile.getId()), new ResponseEntity<>(HttpStatus.BAD_REQUEST));
    }

    @Test
    void testAddEmailsWithInvalidProfileID(){
        List<String> newEmails = Arrays.asList("newEmail@xtra.co.nz", "newEmail2@yahoo.co.nz");
        EmailAddRequest testRequest = new EmailAddRequest(newEmails);
        ResponseEntity<String> expectedResponse = new ResponseEntity<>("That profile does not exist.", HttpStatus.FORBIDDEN);
        long dummyID = 1657568479;
        assertEquals(0, repo.count());

        ResponseEntity<String> actualResponse =  profileController.addEmails(testRequest, dummyID, null, true);
        assertEquals(0, repo.count());
        assertEquals(expectedResponse, actualResponse, "Check response has correct message and status code (403).");
    }

    /**
     * Tests deleting a profile.
     */
    @Test
    void deleteProfileTest() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ResponseEntity<String> delete_profile_response = profileController.deleteProfile(db_profile.getId());
        assertEquals(HttpStatus.OK, delete_profile_response.getStatusCode());

        expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

    }

    /**
     * This tests adding two new additional emails and keeping the existing primary email.
     */
    @Test
    void editEmailsSuccessTest() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        System.out.println("error string: " + response_entity.getBody());
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        EmailUpdateRequest emailUpdateRequest = new EmailUpdateRequest(new ArrayList<>(Arrays.asList("a@g.c", "b@g.c")),
                "jimjam@hotmail.com", db_profile.getId().intValue());

        ResponseEntity<String> edit_emails_response = profileController.editEmails(emailUpdateRequest, db_profile.getId());
        assertEquals(HttpStatus.OK, edit_emails_response.getStatusCode());
    }

    /**
     * Tests changing the primary email to an invalid one and adding two additional emails.
     */
    @Test
    void editEmailsInvalidPrimary() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        EmailUpdateRequest emailUpdateRequest = new EmailUpdateRequest(new ArrayList<>(Arrays.asList("a@g.c", "b@g.c")),
                "jimjamhotmail.com", db_profile.getId().intValue());

        ResponseEntity<String> edit_emails_response = profileController.editEmails(emailUpdateRequest, db_profile.getId());
        assertEquals(HttpStatus.BAD_REQUEST, edit_emails_response.getStatusCode());
    }

    /**
     * Tests changing the primary email to another valid one and adding two new additional emails.
     */
    @Test
    void editEmailsChangePrimaryAddAdditionalsTest() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        EmailUpdateRequest emailUpdateRequest = new EmailUpdateRequest(new ArrayList<>(Arrays.asList("a@g.c", "b@g.c")),
                "jimjam@google.com", db_profile.getId().intValue());

        ResponseEntity<String> edit_emails_response = profileController.editEmails(emailUpdateRequest, db_profile.getId());
        assertEquals(HttpStatus.OK, edit_emails_response.getStatusCode());
    }

    /**
     * Tests to make sure changing the password works.
     */
    @Test
    void changePasswordTest() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hushhush", "12345678", "12345678");

        ResponseEntity<String> change_password_response = profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);
        assertEquals(HttpStatus.OK, change_password_response.getStatusCode());

        db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        assertEquals(profileController.hashPassword("12345678"), db_profile.getPassword());
    }

    /**
     * Tests to make sure the current password isn't changed when the new passwords are inconsistent.
     */
    @Test
    void changePasswordInconsistentNewPasswordsTest() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hushhush", "12345678", "87654321");

        ResponseEntity<String> change_password_response = profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);
        assertEquals(HttpStatus.BAD_REQUEST, change_password_response.getStatusCode());

        db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        assertEquals(profileController.hashPassword("hushhush"), db_profile.getPassword());
    }

    /**
     * Tests to make sure that the password is not changed if the user enters the wrong current password.
     */
    @Test
    void changePasswordIncorrectCurrentTest() {
        Profile jimmy = createNormalProfileJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, repo.count());

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hashhash", "12345678", "12345678");

        ResponseEntity<String> change_password_response = profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);
        assertEquals(HttpStatus.BAD_REQUEST, change_password_response.getStatusCode());

        db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        assertEquals(profileController.hashPassword("hushhush"), db_profile.getPassword());
    }

    /**
     * Needs to be run before each test to ensure the repository starts empty.
     */
    @BeforeEach
    void setup() {
        repo.deleteAll();
        erepo.deleteAll();
        arepo.deleteAll();
        arepo.save(new ActivityType("Football"));
        arepo.save(new ActivityType("Tennis"));
        arepo.save(new ActivityType("Hockey"));
        arepo.save(new ActivityType("Basketball"));
        arepo.save(new ActivityType("Hiking"));
        arepo.save(new ActivityType("Rock Climbing"));
    }

    /* Below are a set of ready-made Profile objects which can be used for various tests. */

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{}, 5);
    }

    /**
     * @return a valid profile object with activityType types
     */
    static Profile createProfileWithActivityTypes() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{"Football", "Tennis"}, 5);
    }

    /**
     * @return a profile object with invalid activityType types
     */
    static Profile createProfileWithInvalidActivityTypes() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{"Not a real activityType", "Tennis"}, 5);
    }

    /**
     * @return a valid profile object with 3 activityTypes.
     */
    static Profile createNormalActivityTypesProfile() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{"Football", "Hockey", "Basketball"}, 5);
    }

    /**
     * @return a valid profile object with updated activityTypes.
     */
    static Profile createUpdatedActivityTypesProfile() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{"Hiking"}, 5);
    }

    /**
     * @return a profile object with an invalid country name.
     */
    static Profile createInvalidCountryProfileJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"Cowabunga"}, new String[]{}, 5);
    }

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{"New Zealand", "China"}, new String[]{}, 5);
    }

    /**
     * @return an invalid profile object with fields containing empty strings.
     */
    static Profile createInvalidFieldsProfileMaurice() {
        return new Profile(null, "", "", "Jack", "Jacky", "", new String[]{"additionaldoda@email.com"}, "hush",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "Male", 10, new String[]{"New Zealand", "India"}, new String[]{"random"}, 5);
    }

    /**
     * @return a profile with the minimal required fields to be successfully created
     */
    private Profile createProfileWithMinimalFields() {
        return new Profile(null, "Steven", "Stevenson", "", "",
                "steven@steven.com", new String[]{}, "12345678", "", new GregorianCalendar(1992,
                Calendar.JUNE, 10), "male", 0, new String[]{}, new String[]{}, 5);
    }
}

