package com.springvuegradle.service;

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
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(SpringExtension.class)
@DataJpaTest
public class ProfileServiceTest {

    @Autowired
    ProfileService profileService;

    @Autowired
    ProfileRepository profileRepository;

    @Autowired
    ProfileLocationRepository profileLocationRepository;

    @AfterEach
    void tearDown() {
        profileLocationRepository.deleteAll();
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

}
