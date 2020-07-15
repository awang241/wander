package com.springvuegradle.Model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class ProfileTest {

    private Profile firstProfile;
    private Profile secondProfile;
    private Profile firstProfileAgain;
    private String[] passportCountries = {"USA", "UK"};
    private String[] extraEmails = {"throwayway1@gmail.com", "throwaway2@gmail.com"};
    private String[] activityTypes = {"Basketball", "Football"};

    @BeforeEach
    void setUp() {
        Calendar calendar1 = new GregorianCalendar(2000, 11, 5);
        Calendar calendar2 = new GregorianCalendar(2000, 11, 20);
        Calendar calendar3 = new GregorianCalendar(2000, 11, 5);


        firstProfile = new Profile(1L, "Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar1,
                "Male", 2, passportCountries, activityTypes);
        secondProfile = new Profile(2L, "Dave", "Tester", "The", "Davetest",
                "Dave@test.com", extraEmails, "987654321", "Here to run some tests!", calendar2,
                "Male", 2, passportCountries, activityTypes);
        firstProfileAgain = new Profile(3L, "Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar3,
                "Male", 2, passportCountries, activityTypes);


    }

    /**
     * To check that the two profiles are equal
     */
    @Test
    void twoProfilesAreEqualTest(){
        assertEquals(firstProfile, firstProfileAgain);
    }

    /**
     *  Test to check that the first name is updated correctly
     */
    @Test
    void updateProfileCheckFirstNameTest(){
        firstProfile.updateProfileExceptEmailsPassword(secondProfile);
        assertEquals(firstProfile.getFirstname(), secondProfile.getFirstname());
    }

    /**
     *  Test to check that the nick name is updated correctly
     */
    @Test
    void updateProfileCheckNickNameTest(){
        firstProfile.updateProfileExceptEmailsPassword(secondProfile);
        assertEquals(firstProfile.getNickname(), secondProfile.getNickname());
    }

    /**
     *  Test to check that the list of country names is the right size
     */
    @Test
    void listOfCountryNamesSizeTest(){
        assertEquals(2, firstProfile.getPassports().size());
    }

    /**
     *  Test to check that the list of country names are correct
     */

    @Test
    void getListOfCountryNamesTest(){
        assertEquals(firstProfile.getPassports().get(0).getClass(), String.class);
    }

    /**
     *  Test the format of date of birth
     */
    @Test
    void getDateOfBirthInCorrectStringFormatTest(){
        String expectedString = "2000-12-05";
        assertEquals(expectedString, firstProfile.getDateOfBirth());
    }

}