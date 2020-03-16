package com.springvuegradle.Controller;

import com.springvuegradle.Model.PassportCountry;
import com.springvuegradle.Model.Profile;

import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.ProfileRepository;
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
 * Tests for the Profile_Controller class, these tests are run separate from the actual repository.
 * Author: Hamesh Ravji
 */
@ExtendWith(SpringExtension.class)
@DataJpaTest
class ProfileControllerTest {

    @Autowired
    private ProfileRepository repo;

    @Autowired
    private EmailRepository erepo;

    @Autowired
    private Profile_Controller profileController;

    /**
     * This tests to ensure profiles structured correctly can be added to the database.
     */
    @Test
    void createProfileTest() {
        Profile jimmy = createJimmy();
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy, true, repo, erepo);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        Profile maurice = createMaurice();
        ResponseEntity<String> response_entity_new = profileController.createProfile(maurice, true, repo, erepo);
        assertEquals(HttpStatus.CREATED, response_entity_new.getStatusCode());

        expected_in_repo = 2;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        Profile dummyJimmy = createDummyJimmy();
        ResponseEntity<String> response_entity_dummy = profileController.createProfile(dummyJimmy, true, repo, erepo);
        assertEquals(HttpStatus.FORBIDDEN, response_entity_dummy.getStatusCode());

        expected_in_repo = 2;
        assertEquals(expected_in_repo, profileController.getRepository().count());
        assertEquals(expected_in_repo, profileController.getRepository().count());
    }

    /**
     * This method tests that a profile filled correctly where the email address does not exist already in the database
     * is saved to the database.
     */
    @Test
    void getProfileSuccessTest() {
        Profile jimmy = createJimmy();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        ResponseEntity<String> response_entity = profileController.createProfile(jimmy, true, repo, erepo);
        assertEquals(HttpStatus.CREATED, response_entity.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        long expected_id = profileController.getRepository().findAll().get(0).getId();

        ResponseEntity<Profile> response_entity_new = profileController.getProfile(expected_id, null, true, repo);
        assertEquals(HttpStatus.OK, response_entity_new.getStatusCode());
        Profile db_jimmy = response_entity_new.getBody();
        assertTrue(jimmy.equals(db_jimmy));
    }

    @Test
    void getProfileDoesNotExistTest() {
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, profileController.getRepository().count());
        ResponseEntity<Profile> response_entity = profileController.getProfile((long) 1, null, true, repo);
        assertEquals(HttpStatus.NOT_FOUND, response_entity.getStatusCode());
    }

    /**
     * This method tests that the profile without the mandatory fields filled in is not saved to the database.
     */
    @Test
    void createProfileMandatoryFieldsTest() {
        Profile dummy_maurice = createDummyMaurice();

        int expected_in_repo = 0;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        ResponseEntity<String> response_entity = profileController.createProfile(dummy_maurice, true, repo, erepo);
        System.out.println(response_entity.getBody());
        assertEquals(HttpStatus.FORBIDDEN, response_entity.getStatusCode());
        String actual_error_message = response_entity.getBody();
        String expected_error_message = "The email field is blank.\n" +
                "The First Name field is blank.\n" +
                "The Last Name field is blank.\n" +
                "The Password is not long enough.\n" +
                "The fitness level isn't valid.\n" +
                "The Gender field must contain either 'male', 'female' or 'non-binary'.\n";
        assertEquals(expected_error_message, actual_error_message);
        assertEquals(expected_in_repo, profileController.getRepository().count());
    }

    /**
     * This test simulates a user trying to create an account with an email address which is already being used on
     * another account.
     */
    @Test
    void createProfileExistingEmailTest() {
        Profile jimmy = createJimmy();
        int expected_in_repo = 0;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        ResponseEntity<String> response_entity_jimmy = profileController.createProfile(jimmy, true, repo, erepo);
        assertEquals(HttpStatus.CREATED, response_entity_jimmy.getStatusCode());

        expected_in_repo = 1;
        assertEquals(expected_in_repo, profileController.getRepository().count());

        Profile dup_jimmy = createJimmy();

        ResponseEntity<String> response_entity_dup_jimmy = profileController.createProfile(dup_jimmy, true, repo, erepo);
        assertEquals(HttpStatus.FORBIDDEN, response_entity_dup_jimmy.getStatusCode());

        assertEquals(expected_in_repo, profileController.getRepository().count());

        String actual_error_message = response_entity_dup_jimmy.getBody();
        String expected_error_message = "An email address you have entered is already in use by another Profile.\n";
        assertEquals(expected_error_message, actual_error_message);
    }

    /**
     * Needs to be run before each test to ensure the repository starts empty.
     */
    @BeforeEach
    void setup() {
        repo.deleteAll();
        erepo.deleteAll();
    }

    /* Below are a set of ready-made Profile objects which can be used for various tests. */

    static Profile createJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"});
    }

    static Profile createDummyJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"Cowabunga"});
    }

    static Profile createMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{"New Zealand", "India"});
    }

    static Profile createDummyMaurice() {
        return new Profile(null, "", "", "Jack", "Jacky", "", new String[]{"additionaldoda@email.com"}, "hush",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "Male", 10, new String[]{"New Zealand", "India"});
    }
}

