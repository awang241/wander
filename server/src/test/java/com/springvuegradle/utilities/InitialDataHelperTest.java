package com.springvuegradle.utilities;

import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.ActivityTypeRepository;
import com.springvuegradle.repositories.EmailRepository;
import com.springvuegradle.repositories.PassportCountryRepository;
import com.springvuegradle.repositories.ProfileRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.List;

import static com.springvuegradle.utilities.FieldValidationHelper.verifyProfile;
import static com.springvuegradle.utilities.InitialDataHelper.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class InitialDataHelperTest {

    @Autowired
    ProfileRepository profileRepository;
    @Autowired
    EmailRepository emailRepository;
    @Autowired
    PassportCountryRepository passportCountryRepository;
    @Autowired
    ActivityTypeRepository activityTypeRepository;

    /**
     * Needs to be run before each test to reset the repositories
     */
    @BeforeEach
    private void setUp() {
        emailRepository.deleteAll();
        profileRepository.deleteAll();
        passportCountryRepository.deleteAll();
        activityTypeRepository.deleteAll();
    }

    /**
     * Test to check the default admin profile is updated correctly
     */
    @Test
    void updateDefaultAdminTest() {
        assertEquals(20, updateDefaultAdmin(profileRepository, emailRepository).length());
    }

    /**
     * Test to check the default admin profile already exists
     */
    @Test
    void updateDefaultAdminTestExistingAdmin() {
        updateDefaultAdmin(profileRepository, emailRepository).length();
        assertNull(updateDefaultAdmin(profileRepository, emailRepository));
    }

    /**
     * Test to check the example profiles created are valid profiles
     */
    @Test
    void testExampleProfilesValidity() {
        // using method of field validation helper to test sample profile validity
        addExampleProfiles(profileRepository, emailRepository);
        List<Profile> profiles = profileRepository.findAll();
        String result = verifyProfile(profiles.get(0), true, passportCountryRepository,
                activityTypeRepository, emailRepository);
        assertEquals("", result);
        result = verifyProfile(profiles.get(1), true, passportCountryRepository,
                activityTypeRepository, emailRepository);
        assertEquals("", result);
    }
}