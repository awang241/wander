package com.springvuegradle.service;

import com.springvuegradle.Controller.Profile_Controller;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileSearchCriteria;
import com.springvuegradle.Model.ProfileTestUtils;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import io.cucumber.java.bs.A;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileLocation;
import com.springvuegradle.Repositories.*;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class ProfileServiceTest {

    @Autowired
    private ProfileRepository profileRepository;
    @Autowired
    private EmailRepository emailRepository;
    @Autowired
    private ProfileService testService;
    @Autowired
    private Profile_Controller controller;

    @Autowired
    ProfileService profileService;

    @Autowired
    ProfileLocationRepository profileLocationRepository;

    @AfterEach
    void tearDown() {
        profileLocationRepository.deleteAll();
    }

    private Profile jimmyOne, jimmyTwo, steven, maurice, nicknamedQuick;
    private List<Profile> profilesWithSameSurnameAsJimmy;


    @BeforeEach
    public void setUp(){
        emailRepository.deleteAll();
        profileRepository.deleteAll();
        jimmyOne = ProfileTestUtils.createProfileJimmy();
        jimmyOne.setPassports(new HashSet<>());
        jimmyTwo = ProfileTestUtils.createProfileJimmyAlternate();
        nicknamedQuick = ProfileTestUtils.createProfileNicknameMatchesJimmySurname();
        steven = ProfileTestUtils.createProfileWithMinimalFields();
        maurice = ProfileTestUtils.createNormalProfileMaurice();
        maurice.setPassports(new HashSet<>());
        profilesWithSameSurnameAsJimmy = ProfileTestUtils.createProfilesWithSameSurnameAsJimmy();
    }

    @Test
    void getUsersWithNoCriteriaReturnsAllUsersTest() {
        jimmyOne = profileRepository.save(jimmyOne);
        nicknamedQuick = profileRepository.save(nicknamedQuick);
        maurice = profileRepository.save(maurice);
        steven = profileRepository.save(steven);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.add(nicknamedQuick);
        expectedProfiles.add(maurice);
        expectedProfiles.add(steven);

        PageRequest request = PageRequest.of(0, (int) profileRepository.count());
        Page<Profile> result = testService.getUsers(new ProfileSearchCriteria(), request);
        assertTrue(result.getContent().containsAll(expectedProfiles), "Check no duplicates in result page");
        assertEquals(expectedProfiles.size(), result.getSize());
    }

    @Test
    void getUsersByFirstNameNormalTest() {
        jimmyOne = profileRepository.save(jimmyOne);
        jimmyTwo = profileRepository.save(jimmyTwo);
        nicknamedQuick = profileRepository.save(nicknamedQuick);
        maurice = profileRepository.save(maurice);
        profilesWithSameSurnameAsJimmy = profileRepository.saveAll(profilesWithSameSurnameAsJimmy);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.add(jimmyTwo);

        PageRequest request = PageRequest.of(0, (int) profileRepository.count());
        ProfileSearchCriteria criteria = new ProfileSearchCriteria(jimmyOne.getFirstname(), null,
                null, null, null);
        Page<Profile> actualProfiles = testService.getUsers(criteria, request);

        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(2, actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Test
    void getUsersByMiddleNameNormalTest() {
        jimmyOne = profileRepository.save(jimmyOne);
        jimmyTwo = profileRepository.save(jimmyTwo);
        nicknamedQuick = profileRepository.save(nicknamedQuick);
        maurice = profileRepository.save(maurice);
        steven = profileRepository.save(steven);
        profilesWithSameSurnameAsJimmy = profileRepository.saveAll(profilesWithSameSurnameAsJimmy);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(maurice);

        PageRequest request = PageRequest.of(0, (int) profileRepository.count());
        ProfileSearchCriteria criteria = new ProfileSearchCriteria(null, maurice.getMiddlename(),
                null, null, null);
        Page<Profile> actualProfiles = testService.getUsers(criteria, request);

        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Test
    void getUsersByLastNameNormalTest() {
        jimmyOne = profileRepository.save(jimmyOne);
        maurice = profileRepository.save(maurice);
        steven = profileRepository.save(steven);
        profilesWithSameSurnameAsJimmy = profileRepository.saveAll(profilesWithSameSurnameAsJimmy);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.addAll(profilesWithSameSurnameAsJimmy);
        ProfileSearchCriteria criteria = new ProfileSearchCriteria(null, null, jimmyOne.getLastname(),
                null, null);
        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));

        Page<Profile> actualProfiles = testService.getUsers(criteria, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Test
    void getUsersByFullNameNormalTest() {
        jimmyOne = profileRepository.save(jimmyOne);
        jimmyTwo = profileRepository.save(jimmyTwo);
        steven = profileRepository.save(steven);
        profilesWithSameSurnameAsJimmy = profileRepository.saveAll(profilesWithSameSurnameAsJimmy);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.add(jimmyTwo);

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));

        ProfileSearchCriteria criteria = new ProfileSearchCriteria(jimmyOne.getFirstname(), jimmyOne.getMiddlename(),
                jimmyOne.getLastname(), null, null);
        Page<Profile> actualProfiles = testService.getUsers(criteria, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Test
    void getUsersByNicknameNormalTest() {profileRepository.save(jimmyOne);
        profileRepository.save(jimmyTwo);
        nicknamedQuick = profileRepository.save(nicknamedQuick);
        profileRepository.save(steven);
        profileRepository.saveAll(profilesWithSameSurnameAsJimmy);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(nicknamedQuick);

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));
        ProfileSearchCriteria criteria = new ProfileSearchCriteria(null, null, null,
                nicknamedQuick.getNickname(), null);
        Page<Profile> actualProfiles = testService.getUsers(criteria, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Test
    void getUsersByEmailNormalTest() {
        String email = steven.getPrimary_email();
        saveWithEmails(jimmyOne);
        saveWithEmails(jimmyTwo);
        steven = saveWithEmails(steven);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(steven);

        List<Profile> someProfiles = profileRepository.findAll();
        List<Email> someEmails = emailRepository.findAll();

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));
        ProfileSearchCriteria criteria = new ProfileSearchCriteria(null, null, null,
                null, email);
        Page<Profile> actualProfiles = testService.getUsers(criteria, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Test
    void getUsersByEmailWithNoProfileWithThatEmailReturnsNothingTest() {
        saveWithEmails(jimmyOne);
        saveWithEmails(jimmyTwo);
        steven = saveWithEmails(steven);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(steven);

        List<Profile> someProfiles = profileRepository.findAll();
        List<Email> someEmails = emailRepository.findAll();

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));
        ProfileSearchCriteria criteria = new ProfileSearchCriteria(null, null, null,
                null, "not a real email");
        Page<Profile> actualProfiles = testService.getUsers(criteria, request);
        assertTrue(actualProfiles.isEmpty(), "Check no results are returned");
    }

    @Test
    void getUsersWithNoProfilesMatchingParamsReturnsNoProfilesTest() {
        jimmyOne = profileRepository.save(jimmyOne);
        maurice = profileRepository.save(maurice);
        steven = profileRepository.save(steven);

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));
        Page<Profile> actualProfiles;
        ProfileSearchCriteria criteria = new ProfileSearchCriteria("Jim", "Bim", "Dim",
                "Gim", null);
        actualProfiles = testService.getUsers(criteria, request);
        assertTrue(actualProfiles.isEmpty());
    }

    @Test
    void getUsersMatchingIsNotCaseSensitiveTest() {
        jimmyTwo = profileRepository.save(jimmyTwo);
        nicknamedQuick = profileRepository.save(nicknamedQuick);
        steven = profileRepository.save(steven);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(steven);

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));
        ProfileSearchCriteria criteria = new ProfileSearchCriteria("ste", null, null,
                null, null);
        Page<Profile> actualProfiles = testService.getUsers(criteria, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    /**
     * Test to ensure HTTP Ok response returned when successfully editing profile
     **/
    @Test
    void testAddLocationResponse() {
        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        Profile profile = createProfile();
        profileRepository.save(profile);
        ResponseEntity<String> response = profileService.updateProfileLocation(location, profile.getId());
        assertEquals(response, new ResponseEntity<>(HttpStatus.OK));
    }

    /**
     * Test to ensure data for profiles location matches the provided json data
     **/
    @Test
    void testAddLocationData() {
        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        Profile profile = createProfile();
        profileRepository.save(profile);
        ResponseEntity<String> response = profileService.updateProfileLocation(location, profile.getId());
        assertTrue(profile.getProfileLocation().equals(location));
    }

    /**
     * Test to ensure when changing the profiles location, the new location is now associated with the profile
     **/
    @Test
    void testChangeLocationData() {
        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        Profile profile = createProfile();
        profile.setLocation(location);
        profileRepository.save(profile);
        ProfileLocation newLocation = new ProfileLocation("Australia", "Sydney", "NSW");
        profileService.updateProfileLocation(newLocation, profile.getId());
        assertTrue(profile.getProfileLocation().equals(newLocation));
    }

    /**
     * Test to ensure when an invalid profile ID is given a 404 status will be returned
     **/
    @Test
    void testNonExistentProfileIdStatus(){
        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        ResponseEntity<String> response = profileService.updateProfileLocation(location,-1L);
        assertEquals(response, new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }

    /**
     * Test to ensure when an invalid profile ID is given the location is not added to the profile_location table
     **/
    @Test
    void testNonExistentProfileData(){
        ProfileLocation location = new ProfileLocation("New Zealand", "Christchurch", "Canterbury");
        ResponseEntity<String> response = profileService.updateProfileLocation(location, -1L);
        assertEquals(profileLocationRepository.count(), 0L);
    }

    /**
     * Example test profile to use in tests
     **/
    public Profile createProfile(){
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{}, new String[]{});
    }

    private Profile saveWithEmails(Profile profile) {
        Profile updated = profileRepository.save(profile);
        for (Email email: profile.retrieveEmails()) {
            emailRepository.save(email);
        }
        return  updated;
    }
}