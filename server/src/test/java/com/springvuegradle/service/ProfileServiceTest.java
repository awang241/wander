package com.springvuegradle.service;

import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileTestUtils;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.PassportCountryRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.hibernate.TransientObjectException;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

//TODO Update unit tests for new implementation
@ExtendWith(SpringExtension.class)
@DataJpaTest
class ProfileServiceTest {

    @Autowired
    private ProfileRepository profileRepository;
    @Autowired
    private EmailRepository emailRepository;
    @Autowired
    private ProfileService testService;

    private Profile jimmyOne, jimmyTwo, steven, maurice, nicknamedQuick;
    private List<Profile> profilesWithSameSurnameAsJimmy;

//    @BeforeAll
//    public static void setUpBeforeClass() {
//        jimmyOne = ProfileTestUtils.createProfileJimmy();
//        jimmyOne.setPassports(new HashSet<>());
//        jimmyTwo = ProfileTestUtils.createProfileJimmyAlternate();
//        nicknamedQuick = ProfileTestUtils.createProfileNicknameMatchesJimmySurname();
//        steven = ProfileTestUtils.createProfileWithMinimalFields();
//        maurice = ProfileTestUtils.createNormalProfileMaurice();
//        maurice.setPassports(new HashSet<>());
//        profilesWithSameSurnameAsJimmy = ProfileTestUtils.createProfilesWithSameSurnameAsJimmy();
//
//    }

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
        Page<Profile> result = testService.getUsers(null, null, null, null,
                null, request);
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
        Page<Profile> actualProfiles = testService.getUsers(jimmyOne.getFirstname(), null, null,
                null, null, request);

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
        Page<Profile> actualProfiles = testService.getUsers(null, maurice.getMiddlename(), null,
                null, null, request);

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

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));

        Page<Profile> actualProfiles = testService.getUsers(null, null, jimmyOne.getLastname(),
                null, null, request);
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

        Page<Profile> actualProfiles = testService.getUsers(jimmyOne.getFirstname(), jimmyOne.getMiddlename(), jimmyOne.getLastname(),
                null, null, request);
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

        Page<Profile> actualProfiles = testService.getUsers(null, null, null,
                nicknamedQuick.getNickname(), null, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Disabled
    @Test
    void getUsersByEmailNormalTest() {
        String email = steven.getPrimary_email();
        profileRepository.save(jimmyOne);
        profileRepository.save(jimmyTwo);
        profileRepository.save(nicknamedQuick);
        steven = profileRepository.save(steven);
        profileRepository.saveAll(profilesWithSameSurnameAsJimmy);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(steven);

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));

        Page<Profile> actualProfiles = testService.getUsers(null, null, null,
                null, email, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

    @Disabled
    @Test
    void getUsersWithNoProfilesMatchingParamsReturnsNoProfilesTest() {
        jimmyOne = profileRepository.save(jimmyOne);
        maurice = profileRepository.save(maurice);
        steven = profileRepository.save(steven);

        PageRequest request = PageRequest.of(0, Math.toIntExact(profileRepository.count()));
        Collection<Profile> profiles = profileRepository.findAll();
//        for (Profile profile: profiles) {
//            for (Email email: profile.retrieveEmails()) {
//                email.setProfile(profile);
//                emailRepository.save(email);
//            }
//        }
        Collection<Email> emails = emailRepository.findAll();
        Page<Profile> actualProfiles;
/*        try {
            actualProfiles = testService.getUsers("Jim", "Bim", "Dim", "Gim",
                    "Fim", request);
        } catch (TransientObjectException e) {
            System.out.print(e);
        }*/
        actualProfiles = testService.getUsers("Jim", "Bim", "Dim", "Gim",
            "Fim", request);
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

        Page<Profile> actualProfiles = testService.getUsers("ste", null, null,
                null, null, request);
        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(expectedProfiles.size(), actualProfiles.getTotalElements(), "Check page is of the right size.");
    }

}