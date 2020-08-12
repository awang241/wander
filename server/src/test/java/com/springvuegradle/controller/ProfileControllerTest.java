package com.springvuegradle.controller;

import com.springvuegradle.model.ActivityType;
import com.springvuegradle.model.PassportCountry;
import com.springvuegradle.model.Profile;

import com.springvuegradle.model.ProfileLocation;
import com.springvuegradle.repositories.*;
import com.springvuegradle.dto.requests.ChangePasswordRequest;
import com.springvuegradle.dto.requests.EmailAddRequest;
import com.springvuegradle.dto.requests.EmailUpdateRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
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
    private ActivityMembershipRepository amrepo;

    @Autowired
    private ProfileRepository repo;

    @Autowired
    private EmailRepository erepo;

    @Autowired
    private ActivityTypeRepository atrepo;

    @Autowired
    private PassportCountryRepository pcrepo;

    @Autowired
    private ActivityTypeRepository arepo;

    @Autowired
    private Profile_Controller profileController;

    /**
     * Tests the profile is being created correctly
     */
    @Test
    void createProfileTest() {
        Profile jimmy = createNormalProfileJimmy();

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());
    }

    /**
     * Tests the profile is being added to the database correctly
     */
    @Test
    void saveToDatabaseTest() {
        Profile maurice = createNormalProfileMaurice();
        ResponseEntity<String> response_entity_new = profileController.createProfile(maurice);

        Profile jimmy = createNormalProfileJimmy();
        ResponseEntity<String> response_entity = profileController.createProfile(jimmy);

        int expected_in_repo = 2;
        assertEquals(expected_in_repo, repo.count());
    }

    /**
     * Tests if the database rejects invalid profiles correctly
     */
    @Test
    void invalidProfileTest() {
        Profile dummyJimmy = createInvalidCountryProfileJimmy();
        ResponseEntity<String> response_entity_dummy = profileController.createProfile(dummyJimmy);
        assertEquals(HttpStatus.FORBIDDEN, response_entity_dummy.getStatusCode());
    }

    /**
     * Tests an invalid profile is not getting added to the database
     */
    @Test
    void invalidProfileToDatabaseTest() {
        Profile dummyJimmy = createInvalidCountryProfileJimmy();
        ResponseEntity<String> response_entity_dummy = profileController.createProfile(dummyJimmy);
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());
    }

    /**
     * Tests if a profile with activity types can be made
     */
    @Test
    void createProfileWithActivityTypesTest(){
        Profile profile = createProfileWithActivityTypes();
        ResponseEntity<String> response_entity = profileController.createProfile(profile);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());
    }

    /**
     * Tests if a profile with invalid activity types is rejected
     */
    @Test
    void createProfileWithInvalidActivityTypesTest(){
        Profile profile = createProfileWithInvalidActivityTypes();
        ResponseEntity<String> response_entity = profileController.createProfile(profile);
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());
    }

    /**
     * Tests if a profile with minimal fields can be made
     */
    @Test
    void createProfileWithMinimalFieldsTest() {
        Profile testProfile = createProfileWithMinimalFields();
        ResponseEntity<String> responseEntity = profileController.createProfile(testProfile);

        System.out.println(responseEntity.getBody());

        List<Profile> result = repo.findAll();
        assertTrue(result.contains(testProfile));
    }

    /**
     * This method tests that the profile without the mandatory fields filled in cannot be created.
     */
    @Test
    void createProfileWithoutMandatoryFieldsTest() {
        Profile dummy_maurice = createInvalidFieldsProfileMaurice();

        ResponseEntity<String> response_entity = profileController.createProfile(dummy_maurice);
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());
    }

    /**
     * Tests if a profile without mandatory fields cannot be saved to the database
     */
    @Test
    void profileWithoutMandatoryFieldsToDatabaseTest() {
        Profile dummy_maurice = createInvalidFieldsProfileMaurice();

        int expected_in_repo = 0;

        ResponseEntity<String> response_entity = profileController.createProfile(dummy_maurice);
        assertEquals(expected_in_repo, repo.count());
    }

    /**
     * Tests if a profile without mandatory fields generate correct error messages
     */
    @Test
    void errorMessageTest() {
        Profile dummy_maurice = createInvalidFieldsProfileMaurice();

        ResponseEntity<String> response_entity = profileController.createProfile(dummy_maurice);
        System.out.println(response_entity.getBody());
        String actual_error_message = response_entity.getBody();
        String expected_error_message = "The email field is blank.\n" +
                "The First Name field is blank.\n" +
                "The Last Name field is blank.\n" +
                "The Password is not long enough.\n" +
                "The fitness level isn't valid.\n" +
                "ActivityType random does not exist in the database.\n" +
                "The Gender field must contain either 'male', 'female' or 'non-Binary'.\n";
        assertEquals(expected_error_message, actual_error_message);
    }

    /**
     * This test simulates a user trying to create an account with an email address which is already being used on
     * another account.
     */
    @Test
    void createProfileExistingEmailTest() {
        Profile jimmy = createNormalProfileJimmy();

        profileController.createProfile(jimmy);

        Profile dup_jimmy = createNormalProfileJimmy();

        ResponseEntity<String> response_entity_dup_jimmy = profileController.createProfile(dup_jimmy);
        assertEquals(HttpStatus.FORBIDDEN, response_entity_dup_jimmy.getStatusCode());
    }

    /**
     * Tests response when deleting a profile.
     */
    @Test
    void deleteProfileResponseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ResponseEntity<String> delete_profile_response = profileController.deleteProfile(db_profile.getId());
        assertEquals(HttpStatus.OK, delete_profile_response.getStatusCode());
    }

    /**
     * Tests response when deleting a profile that doesn't exist.
     */
    @Test
    void deleteInvalidProfileResponseTest() {
        ResponseEntity<String> delete_profile_response = profileController.deleteProfile(999999999999l);
        assertEquals(HttpStatus.NOT_FOUND, delete_profile_response.getStatusCode());
    }

    /**
     * Tests response when deleting a profile.
     */
    @Test
    void deleteProfileTest() {
        int expected_in_repo;
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        profileController.deleteProfile(db_profile.getId());

        expected_in_repo = 0;
        assertEquals(expected_in_repo, repo.count());
    }

    /**
     * Tests if an error message generated by already existing email address is correct when creating a new account
     */
    @Test
    void existingEmailErrorMessageTest() {
        Profile jimmy = createNormalProfileJimmy();

        profileController.createProfile(jimmy);

        Profile dup_jimmy = createNormalProfileJimmy();

        ResponseEntity<String> response_entity_dup_jimmy = profileController.createProfile(dup_jimmy);

        String actual_error_message = response_entity_dup_jimmy.getBody();
        String expected_error_message = "An email address you have entered is already in use by another Profile.\n";
        assertEquals(expected_error_message, actual_error_message);
    }

    /**
     * Tests the get method for a valid Profile returns the right resposne
     */
    @Test
    void getProfileNormalResponseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        long expected_id = repo.findAll().get(0).getId();
        ResponseEntity<Profile> response_entity_new = profileController.getProfile(expected_id);
        assertEquals(HttpStatus.OK, response_entity_new.getStatusCode());
    }

    /**
     * Tests the get method for returns the right valid profile from the database
     */
    @Test
    void getProfileFromDatabaseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        long expected_id = repo.findAll().get(0).getId();
        ResponseEntity<Profile> response_entity_new = profileController.getProfile(expected_id);
        Profile db_jimmy = response_entity_new.getBody();
        assertEquals(jimmy, db_jimmy);
    }

    /**
     * Tests to see what response status code is when trying to get a profile that does not exist.
     */
    @Test
    void getProfileDoesNotExistTest() {
        ResponseEntity<Profile> response_entity = profileController.getProfile(1L);
        assertEquals(HttpStatus.NOT_FOUND, response_entity.getStatusCode());
    }


    /**
     * Tests that adding an email successfully returns the correct response
     */
    @Test
    void addEmailToProfileResponseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ResponseEntity<String> add_emails_response = profileController.addEmails(new EmailAddRequest(new ArrayList<String>(Collections.singleton("randomEmail@gmail.com"))), db_profile.getId(), null, true);
        assertEquals(HttpStatus.CREATED, add_emails_response.getStatusCode());
    }

    /**
     * Tests that adding an email is correctly saved into the database
     */
    @Test
    void addEmailToProfileCheckDatabaseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);

        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        profileController.addEmails(new EmailAddRequest(new ArrayList<String>(Collections.singleton("randomEmail@gmail.com"))), db_profile.getId(), null, true);
        List<String> emails_from_db_profile = new ArrayList<>(db_profile.getAdditional_email());

        assertEquals("randomEmail@gmail.com", emails_from_db_profile.get(0));
    }

    /**
     * Tests that activity types are saved correctly when creating a new profile with activity types
     * Profile should be able to save more than 1 activity type.
     */
    @Test
    void profileWithActivityTypesTest() {
        Profile originalProfile = createNormalActivityTypesProfile();
        profileController.createProfile(originalProfile);
        long profileId = repo.findByPrimaryEmail(originalProfile.getPrimary_email()).get(0).getId();
        Profile expectedProfile = repo.findById(profileId).get();

        assertEquals(expectedProfile.getActivityTypes().size(), 3, "Check profile saved successfully");
    }

    /**
     * Tests when a profiles activity types are changed, the profile and its activity types are successfully updated.
     */
    @Test
    void updateProfilesActivityTypesTest() {
        Profile originalProfile = createNormalActivityTypesProfile();
        profileController.createProfile(originalProfile);
        long profileId = repo.findByPrimaryEmail(originalProfile.getPrimary_email()).get(0).getId();
        Profile updatedProfile = createUpdatedActivityTypesProfile();

        profileController.updateProfile(updatedProfile, profileId);

        Profile expectedProfile = repo.findById(profileId).get();
        assertEquals(expectedProfile.getActivityTypes(), updatedProfile.getActivityTypes(), "Check activityTypes updated successfully");
    }

    /**
     * Tests to check that when a valid and existing profile updates their passport countries, it returns the correct resposne
     */
    @Test
    void editProfilesPassportCountryResponseTest(){
        Profile testProfile = createNormalProfileJimmy();
        Profile updateData = createNormalProfileMaurice();
        Profile expectedProfile = createNormalProfileMaurice();

        Set<PassportCountry> realPassports = new HashSet<>();
        for (PassportCountry passportCountry: expectedProfile.getPassportObjects()){
            realPassports.add(pcrepo.findByCountryName(passportCountry.getCountryName()).get(0));
        }

        expectedProfile.setPassports(realPassports);
        updateData.setPassports(realPassports);
        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();

        ResponseEntity<String> actualResponse = profileController.updateProfile(updateData, id);
        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    /**
     * Tests to check that when a valid and existing profile updates their activity types, it returns the correct response
     */
    @Test
    void editProfilesActivityTypeResponseTest() {
        Profile testProfile = createNormalProfileJimmy();
        Profile updateData = createNormalProfileMaurice();
        Profile expectedProfile = createNormalProfileMaurice();
        Set<PassportCountry> realPassports = new HashSet<>();
        updateData.setPassports(realPassports);

        Set<ActivityType> realActivityTypes = new HashSet<>();
        for (ActivityType activityType: expectedProfile.getActivityTypeObjects()){
            realActivityTypes.add(atrepo.findByActivityTypeName(activityType.getActivityTypeName()).get(0));
        }
        expectedProfile.setActivityTypes(realActivityTypes);

        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();

        ResponseEntity<String> actualResponse = profileController.updateProfile(updateData, id);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }


    /**
     * Tests to check that when a valid and existing profile updates their passport country, it updates correctly
     * in the database
     */
    @Test
    void editProfilePassportCountryWithPasswordTest(){
        Profile testProfile = createNormalProfileJimmy();
        Profile updateData = createNormalProfileMaurice();
        Profile expectedProfile = createNormalProfileMaurice();
        Set<PassportCountry> realPassports = new HashSet<>();
        for (PassportCountry passportCountry: expectedProfile.getPassportObjects()){
            realPassports.add(pcrepo.findByCountryName(passportCountry.getCountryName()).get(0));
        }
        expectedProfile.setPassword(Profile_Controller.hashPassword(testProfile.getPassword()));
        expectedProfile.setPassports(realPassports);
        updateData.setPassports(realPassports);

        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();

        profileController.updateProfile(updateData, id);

        Profile updatedProfile = repo.findById(id).get();
        assertEquals(expectedProfile, updatedProfile, "Check profile updated successfully");
    }

    /**
     * Tests to check that when a valid and existing profile updates their activity types, it updates correctly
     * in the database
     */
    @Test
    void editProfileActivityTypesWithPasswordTest(){
        Profile testProfile = createNormalProfileJimmy();
        Profile updateData = createNormalProfileMaurice();
        Profile expectedProfile = createNormalProfileMaurice();
        Set<PassportCountry> realPassports = new HashSet<>();
        updateData.setPassports(realPassports);

        expectedProfile.setPassword(Profile_Controller.hashPassword(testProfile.getPassword()));
        expectedProfile.setPassports(realPassports);
        updateData.setPassports(realPassports);

        Set<ActivityType> realActivityTypes = new HashSet<>();
        for (ActivityType activityType: expectedProfile.getActivityTypeObjects()){
            realActivityTypes.add(atrepo.findByActivityTypeName(activityType.getActivityTypeName()).get(0));
        }
        expectedProfile.setActivityTypes(realActivityTypes);

        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();

        profileController.updateProfile(updateData, id);

        Profile updatedProfile = repo.findById(id).get();
        assertEquals(expectedProfile, updatedProfile, "Check profile updated successfully");
    }

    /**
     * Tests to check editing a profile with invalid data returns the correct response
     */
    @Test
    void editProfileWithInvalidDataResponseTest(){
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

        ResponseEntity<String> actualResponse = profileController.updateProfile(updateData, id);

        assertEquals(HttpStatus.BAD_REQUEST, actualResponse.getStatusCode());
    }

    /**
     * Test to check when editing a profile with invalid data will leave the profile in the database unchanged.
     */
    @Test
    void editProfileWithInvalidDataTest(){
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

        profileController.updateProfile(updateData, id);

        Profile updatedProfile = repo.findById(id).get();

        assertEquals(expectedProfile, updatedProfile, "Check profile left unchanged");
    }

    /**
     * Tests to check that adding new valid emails to a profile will be saved successfully
     */
    @Test
    void addEmailsToProfileTest() {
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();

        List<String> newEmails = Arrays.asList("newEmail@xtra.co.nz", "newEmail2@yahoo.co.nz");
        Set<String> expectedAdditionalEmails = new HashSet<String>(testProfile.getAdditional_email());
        expectedAdditionalEmails.addAll(newEmails);
        EmailAddRequest testRequest = new EmailAddRequest(newEmails);

        profileController.addEmails(testRequest, id, null, true);

        Profile updatedProfile = repo.findById(id).get();
        assertEquals(expectedAdditionalEmails, updatedProfile.getAdditional_email(), "Check that the emails have been added successfully");
    }

    /**
     * Tests to check that adding new emails to a profile will not change the primary email address
     */
    @Test
    void addEmailsKeepsPrimaryTest() {
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();

        List<String> newEmails = Arrays.asList("newEmail@xtra.co.nz", "newEmail2@yahoo.co.nz");
        Set<String> expectedAdditionalEmails = new HashSet<String>(testProfile.getAdditional_email());
        expectedAdditionalEmails.addAll(newEmails);
        EmailAddRequest testRequest = new EmailAddRequest(newEmails);

        profileController.addEmails(testRequest, id, null, true);

        Profile updatedProfile = repo.findById(id).get();
        assertEquals(testProfile.getPrimary_email(), updatedProfile.getPrimary_email(), "Check that the primary email is unchanged");
    }

    /**
     * Tests to check that adding new valid emails to the profile will return the correct response
     */
    @Test
    void addValidEmailsResponseTest(){
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        long id = repo.findByPrimaryEmail(testProfile.getPrimary_email()).get(0).getId();
        ResponseEntity<String> expectedResponse = new ResponseEntity<>("Emails added successfully.", HttpStatus.CREATED);

        List<String> newEmails = Arrays.asList("newEmail@xtra.co.nz", "newEmail2@yahoo.co.nz");
        Set<String> expectedAdditionalEmails = new HashSet<String>(testProfile.getAdditional_email());
        expectedAdditionalEmails.addAll(newEmails);
        EmailAddRequest testRequest = new EmailAddRequest(newEmails);

        ResponseEntity<String> actualResponse =  profileController.addEmails(testRequest, id, null, true);

        assertEquals(expectedResponse, actualResponse, "Check response has correct message and status code (201).");
    }

    /**
     * Test the edit of a profile with valid activity types
     */
    @Test
    void editProfileActivityTypesTest(){
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        ArrayList<String> newActivityTypes = new ArrayList<String>(Arrays.asList("Football", "Tennis"));
        assertEquals(profileController.editActivityTypes(newActivityTypes, testProfile.getId()), new ResponseEntity<>(HttpStatus.OK));
    }

    /**
     * Test the edit of a profile with invalid activity types
     */
    @Test
    void testEditInvalidProfileActivityTypes(){
        Profile testProfile = createNormalProfileJimmy();
        profileController.createProfile(testProfile);
        ArrayList<String> newActivityTypes = new ArrayList<String>(Arrays.asList("Not real ActivityType", "Tennis"));
        assertEquals(profileController.editActivityTypes(newActivityTypes, testProfile.getId()), new ResponseEntity<>(HttpStatus.BAD_REQUEST));
    }

    /**
     * Test that you cannot add emails to a profile ID that is not valid
     */
    @Test
    void addEmailsWithToInvalidProfileIDTest(){
        List<String> newEmails = Arrays.asList("newEmail@xtra.co.nz", "newEmail2@yahoo.co.nz");
        EmailAddRequest testRequest = new EmailAddRequest(newEmails);
        long dummyID = 1657568479;

        profileController.addEmails(testRequest, dummyID, null, true);
        assertEquals(0, repo.count());
    }

    /**
     * Test the response for adding emails to an invalid profile id
     */
    @Test
    void addEmailsWithInvalidProfileIDResponseTest(){
        List<String> newEmails = Arrays.asList("newEmail@xtra.co.nz", "newEmail2@yahoo.co.nz");
        EmailAddRequest testRequest = new EmailAddRequest(newEmails);
        ResponseEntity<String> expectedResponse = new ResponseEntity<>("That profile does not exist.", HttpStatus.FORBIDDEN);
        long dummyID = 1657568479;

        ResponseEntity<String> actualResponse =  profileController.addEmails(testRequest, dummyID, null, true);

        assertEquals(expectedResponse, actualResponse, "Check response has correct message and status code (403).");
    }



    /**
     * This tests adding two new additional emails and that adding new emails will keep the existing primary email.
     */
    @Test
    void editEmailsKeepsPrimaryTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        EmailUpdateRequest emailUpdateRequest = new EmailUpdateRequest(new ArrayList<>(Arrays.asList("a@g.c", "b@g.c")),
                "jimjam@hotmail.com", db_profile.getId().intValue());

        ResponseEntity<String> edit_emails_response = profileController.editEmails(emailUpdateRequest, db_profile.getId());
        assertEquals(HttpStatus.OK, edit_emails_response.getStatusCode());
    }



    /**
     * Tests the changing of primary email to an invalid one and adding two additional emails.
     */
    @Test
    void editEmailsInvalidPrimary() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
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
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        EmailUpdateRequest emailUpdateRequest = new EmailUpdateRequest(new ArrayList<>(Arrays.asList("a@g.c", "b@g.c")),
                "jimjam@google.com", db_profile.getId().intValue());

        ResponseEntity<String> edit_emails_response = profileController.editEmails(emailUpdateRequest, db_profile.getId());
        assertEquals(HttpStatus.OK, edit_emails_response.getStatusCode());
    }

    /**
     * Tests to make sure changing the password returns the correct response
     */
    @Test
    void changePasswordResponseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);

        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hushhush", "12345678", "12345678");

        ResponseEntity<String> change_password_response = profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);
        assertEquals(HttpStatus.OK, change_password_response.getStatusCode());
    }

    /**
     * Tests to make sure changing the password works
     */
    @Test
    void changePasswordTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hushhush", "12345678", "12345678");

        profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);

        db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        assertEquals(profileController.hashPassword("12345678"), db_profile.getPassword());
    }

    /**
     * Tests to make sure the current password isn't changed when the new passwords are inconsistent by checking
     * the response returned
     */
    @Test
    void changePasswordInconsistentNewPasswordsResponseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hushhush", "12345678", "87654321");

        ResponseEntity<String> change_password_response = profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);
        assertEquals(HttpStatus.BAD_REQUEST, change_password_response.getStatusCode());
    }

    /**
     * Tests to make sure the current password isn't changed when the new passwords are inconsistent.
     */
    @Test
    void changePasswordInconsistentNewPasswordsTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hushhush", "12345678", "87654321");

        profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);

        db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        assertEquals(profileController.hashPassword("hushhush"), db_profile.getPassword());
    }

    /**
     * Tests check the response if the user tries to change the password with the wrong current password.
     */
    @Test
    void changePasswordIncorrectCurrentResponseTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hashhash", "12345678", "12345678");

        ResponseEntity<String> change_password_response = profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);
        assertEquals(HttpStatus.BAD_REQUEST, change_password_response.getStatusCode());
    }

    /**
     * Tests to make sure that the password is not changed if the user enters the wrong current password.
     */
    @Test
    void changePasswordIncorrectCurrentTest() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        Profile db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        ChangePasswordRequest newPasswordRequest = new ChangePasswordRequest(db_profile.getId(), "hashhash", "12345678", "12345678");

        profileController.changePassword(newPasswordRequest, db_profile.getId(), null, true);

        db_profile = repo.findByPrimaryEmail(jimmy.getPrimary_email()).get(0);
        assertEquals(profileController.hashPassword("hushhush"), db_profile.getPassword());
    }

    /**
     * Test if a list of simplified profiles can be created from normal profiles
     */
//    @Test
//    @Disabled
//    void createSimplifiedProfileListTest() {
//        //Make and save new profiles to mock repo
//        Profile jimmy = createNormalProfileJimmy();
//        profileController.createProfile(jimmy);
//        Profile maurice = createNormalProfileMaurice();
//        profileController.createProfile(maurice);
//
//        ResponseEntity<List<SimplifiedProfileResponse>> response_2 = profileController.getUserProfiles(0);
//        assertEquals(2, response_2.getBody().size());
//    }

    /**
     * Test to add a location to a profile
     */
    @Test
    void addLocationTest(){
        Profile testProfile = createNormalProfileJimmy();

        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        testProfile.setLocation(location);
        ProfileLocation updatedLocation = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");

        profileController.createProfile(testProfile);
        assertEquals(testProfile.getProfileLocation(), updatedLocation);
    }

    /**
     * Test to edit a location in an already existing profile returns the correct response
     */
    @Test
    void editLocationResponseTest(){
        Profile testProfile = createNormalProfileJimmy();
        Profile updateData = createNormalProfileJimmy();
        Profile expectedProfile = createNormalProfileJimmy();

        Set<PassportCountry> realPassports = new HashSet<>();
        for (PassportCountry passportCountry: expectedProfile.getPassportObjects()){
            realPassports.add(pcrepo.findByCountryName(passportCountry.getCountryName()).get(0));
        }
        expectedProfile.setPassword(Profile_Controller.hashPassword(testProfile.getPassword()));
        expectedProfile.setPassports(realPassports);
        updateData.setPassports(realPassports);

        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        testProfile.setLocation(location);
        ProfileLocation updatedLocation = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        expectedProfile.setLocation(updatedLocation);
        updateData.setLocation(updatedLocation);

        profileController.createProfile(testProfile);
        ResponseEntity<String> actualResponse = profileController.updateProfile(updateData, testProfile.getId());

        Profile updatedProfile = repo.findById(testProfile.getId()).get();
        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    /**
     * Test to edit a location in an already existing profile returns the correct location
     */
    @Test
    void editLocationDataTest(){
        Profile testProfile = createNormalProfileJimmy();
        Profile updateData = createNormalProfileJimmy();
        Profile expectedProfile = createNormalProfileJimmy();

        Set<PassportCountry> realPassports = new HashSet<>();
        for (PassportCountry passportCountry: expectedProfile.getPassportObjects()){
            realPassports.add(pcrepo.findByCountryName(passportCountry.getCountryName()).get(0));
        }
        expectedProfile.setPassword(Profile_Controller.hashPassword(testProfile.getPassword()));
        expectedProfile.setPassports(realPassports);
        updateData.setPassports(realPassports);

        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        testProfile.setLocation(location);
        ProfileLocation updatedLocation = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        expectedProfile.setLocation(updatedLocation);
        updateData.setLocation(updatedLocation);

        profileController.createProfile(testProfile);
        ResponseEntity<String> actualResponse = profileController.updateProfile(updateData, testProfile.getId());

        Profile updatedProfile = repo.findById(testProfile.getId()).get();
        assertEquals(expectedProfile.getProfileLocation(), updatedProfile.getProfileLocation(), "Check profile updated");
    }

    /**
     * Tests the get method for a valid Profile returns the right response with a valid auth level
     */
    @Test
    void getProfileNormalResponseTestWithAuthLevel() {
        Profile jimmy = createNormalProfileJimmy();
        profileController.createProfile(jimmy);
        long expected_id = repo.findAll().get(0).getId();
        ResponseEntity<Profile> response_entity_new = profileController.getProfile(expected_id);
        assertEquals(HttpStatus.OK, response_entity_new.getStatusCode());
        assertEquals(5, response_entity_new.getBody().getAuthLevel());
    }



    /**
     * Needs to be run before each test to ensure the repository starts empty.
     */
    @BeforeEach
    void setup() {
        amrepo.deleteAll();
        erepo.deleteAll();
        repo.deleteAll();
        atrepo.deleteAll();
        arepo.deleteAll();
        atrepo.save(new ActivityType("Football"));
        atrepo.save(new ActivityType("Tennis"));
        atrepo.save(new ActivityType("Hockey"));
        atrepo.save(new ActivityType("Basketball"));
        atrepo.save(new ActivityType("Hiking"));
        atrepo.save(new ActivityType("Rock Climbing"));
    }

    /* Below are a set of ready-made Profile objects which can be used for various tests. */

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{});
    }

    /**
     * @return a valid profile object with activityType types
     */
    static Profile createProfileWithActivityTypes() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{"Football", "Tennis"});
    }

    /**
     * @return a profile object with invalid activityType types
     */
    static Profile createProfileWithInvalidActivityTypes() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{"Not a real activityType", "Tennis"});
    }

    /**
     * @return a valid profile object with 3 activityTypes.
     */
    static Profile createNormalActivityTypesProfile() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{"Football", "Hockey", "Basketball"});
    }

    /**
     * @return a valid profile object with updated activityTypes.
     */
    static Profile createUpdatedActivityTypesProfile() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{"Hiking"});
    }

    /**
     * @return a profile object with an invalid country name.
     */
    static Profile createInvalidCountryProfileJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"Cowabunga"}, new String[]{});
    }

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{"New Zealand", "China"}, new String[]{});
    }

    /**
     * @return an invalid profile object with fields containing empty strings.
     */
    static Profile createInvalidFieldsProfileMaurice() {
        return new Profile(null, "", "", "Jack", "Jacky", "", new String[]{"additionaldoda@email.com"}, "hush",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "Male", 10, new String[]{"New Zealand", "India"}, new String[]{"random"});
    }

    /**
     * @return a profile with the minimal required fields to be successfully created
     */
    private Profile createProfileWithMinimalFields() {
        return new Profile(null, "Steven", "Stevenson", "", "",
                "steven@steven.com", new String[]{}, "12345678", "", new GregorianCalendar(1992,
                Calendar.JUNE, 10), "male", 0, new String[]{}, new String[]{});
    }
}

