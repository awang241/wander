package com.springvuegradle.Utilities;

import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import net.bytebuddy.matcher.ElementMatcher;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static com.springvuegradle.Utilities.InitialDataHelper.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
class InitialDataHelperTest {

    @Autowired
    ProfileRepository profileRepository;
    @Autowired
    EmailRepository emailRepository;

    @BeforeEach
    private void setUp() {
        emailRepository.deleteAll();
        profileRepository.deleteAll();
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
}