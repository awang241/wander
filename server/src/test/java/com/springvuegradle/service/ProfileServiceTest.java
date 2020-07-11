package com.springvuegradle.service;

import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileTestUtils;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.PassportCountryRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.transaction.Transactional;

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
    private PassportCountryRepository passportCountryRepository;
    @Autowired
    private static ActivityTypeRepository activityTypeRepository;
    @Autowired
    private EmailRepository emailRepository;
    @Autowired
    private ProfileService testService;

    private static Profile jimmyOne, jimmyTwo, steven, maurice, nicknamedQuick;
    private static List<Profile> sameSurnameProfiles;

    @BeforeAll
    public static void setUpBeforeClass() {
        jimmyOne = ProfileTestUtils.createProfileJimmy();
        jimmyOne.setPassports(new HashSet<>());
        jimmyTwo = ProfileTestUtils.createProfileJimmyAlternate();
        nicknamedQuick = ProfileTestUtils.createProfileNicknameMatchesJimmySurname();
        steven = ProfileTestUtils.createProfileWithMinimalFields();
        maurice = ProfileTestUtils.createNormalProfileMaurice();
        maurice.setPassports(new HashSet<>());
        sameSurnameProfiles = ProfileTestUtils.createProfilesWithSameSurnameAsJimmy();

    }

    @BeforeEach
    public void setUp(){
        emailRepository.deleteAll();
        profileRepository.deleteAll();
    }

    @Disabled
    @Test
    void getUsersTestGetAll() {
        profileRepository.save(jimmyOne);
        profileRepository.save(jimmyTwo);
        profileRepository.save(nicknamedQuick);
        profileRepository.save(maurice);
        profileRepository.save(steven);
        profileRepository.saveAll(sameSurnameProfiles);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.add(jimmyTwo);
        expectedProfiles.add(nicknamedQuick);
        expectedProfiles.add(maurice);
        expectedProfiles.add(steven);
        expectedProfiles.addAll(sameSurnameProfiles);
    }

    @Disabled
    @Test
    void getUsersByNameTest() {
        profileRepository.save(jimmyOne);
        profileRepository.save(jimmyTwo);
        profileRepository.save(nicknamedQuick);
        profileRepository.save(maurice);
        profileRepository.save(steven);
        profileRepository.saveAll(sameSurnameProfiles);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.add(jimmyTwo);
        expectedProfiles.add(nicknamedQuick);
        expectedProfiles.addAll(sameSurnameProfiles);
        int pageSize = 3;
        int pageNum = 1;

        PageRequest request = PageRequest.of(pageNum, pageSize);/*
        Page<Profile> actualProfiles = testService.getUsers(jimmyOne.getLastname(), null, null,
                false, request);

        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(pageSize, actualProfiles.getTotalElements(), "Check page is of the right size.");
        */
    }

    @Disabled
    @Test
    void getUsersByNameIsBlankTest() {
        profileRepository.save(jimmyOne);
        profileRepository.save(jimmyTwo);
        profileRepository.save(nicknamedQuick);
        profileRepository.save(maurice);
        profileRepository.save(steven);
        profileRepository.saveAll(sameSurnameProfiles);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.add(jimmyTwo);
        expectedProfiles.add(nicknamedQuick);
        expectedProfiles.addAll(sameSurnameProfiles);
        int pageSize = 3;
        int pageNum = 1;

        List<Profile> p = profileRepository.findAll(Specification.where(null));
        profileRepository.findAll();

        /*
        PageRequest request = PageRequest.of(pageNum, pageSize);
        Page<Profile> actualProfiles = testService.getUsers(jimmyOne.getLastname(), null, null,
                false, request);

        assertTrue(expectedProfiles.containsAll(actualProfiles.getContent()), "Check page contains the correct profiles.");
        assertEquals(pageSize, actualProfiles.getTotalElements(), "Check page is of the right size.");
        */
    }

    @Disabled
    @Test
    void getUsersByNamePagingTest() {
        profileRepository.save(jimmyOne);
        profileRepository.save(maurice);
        profileRepository.save(steven);
        profileRepository.saveAll(sameSurnameProfiles);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.addAll(sameSurnameProfiles);

        int expectedSize = 3;
        /*
        List<Profile> actualProfiles = testService.getUsersByName(jimmyOne.getLastname(), expectedSize, 1);
        assertTrue(expectedProfiles.containsAll(actualProfiles), "Check page contains the correct profiles.");
        assertEquals(expectedSize, actualProfiles.size(), "Check page is of the right size.");
        */
    }

    @Disabled
    @Test
    void getUsersByFullNameTest() {
        profileRepository.save(jimmyOne);
        profileRepository.save(jimmyTwo);
        profileRepository.save(maurice);
        profileRepository.save(steven);

        Set<Profile> expectedProfiles = new HashSet<>();
        expectedProfiles.add(jimmyOne);
        expectedProfiles.add(jimmyTwo);

        Set<Profile> actualProfiles = new HashSet<Profile>(testService.getUsersByName(jimmyOne.getFullName(),
                Math.toIntExact(profileRepository.count()), 0).getContent());
        assertEquals(expectedProfiles, actualProfiles, "Check correct profiles have been returned.");
    }

    @Disabled
    @Test
    void getUsersByNameWhenNotInRepoReturnsEmptyListTest() {
        profileRepository.save(jimmyOne);
        profileRepository.save(maurice);
        profileRepository.save(steven);

        Set<Profile> actualProfiles = new HashSet<>(testService.getUsersByName("not a real name",
                Math.toIntExact(profileRepository.count()), 0).getContent());
        assertEquals(0, actualProfiles.size(), "Check empty list is returned.");
    }

    @Disabled
    @Test
    void getUsersByEmail() {
        fail("Not yet implemented");
    }
}