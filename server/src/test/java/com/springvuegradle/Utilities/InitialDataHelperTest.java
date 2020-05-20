package com.springvuegradle.Utilities;

import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.PassportCountryRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import net.bytebuddy.matcher.ElementMatcher;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.List;

import static com.springvuegradle.Utilities.FieldValidationHelper.verifyProfile;
import static com.springvuegradle.Utilities.InitialDataHelper.*;
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

    @BeforeEach
    private void setUp() {
        emailRepository.deleteAll();
        profileRepository.deleteAll();
        passportCountryRepository.deleteAll();
        activityTypeRepository.deleteAll();
    }

    @Test
    void updateDefaultAdminTest() {
        assertEquals(20, updateDefaultAdmin(profileRepository, emailRepository).length());
    }

    @Test
    void updateDefaultAdminTestExistingAdmin() {
        updateDefaultAdmin(profileRepository, emailRepository).length();
        assertNull(updateDefaultAdmin(profileRepository, emailRepository));
    }

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