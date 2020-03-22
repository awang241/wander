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

    @BeforeEach
    void setUp() {
        Calendar calendar = new GregorianCalendar(2000, 11, 15);


        firstProfile = new Profile(1L, "Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar,
                "Male", 2, passportCountries);
        secondProfile = new Profile(2L, "Dave", "Tester", "The", "Davetest",
                "Dave@test.com", extraEmails, "987654321", "Here to run some tests!", calendar,
                "Male", 2, passportCountries);
        firstProfileAgain = new Profile(3L, "Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar,
                "Male", 2, passportCountries);


    }

    @Test
    void testTwoProfilesAreEqual(){
        assertEquals(firstProfile, firstProfileAgain);
    }

    @Test
    void testUpdateProfileWithNewInfo(){
        firstProfile.updateProfileExceptEmailsPassword(secondProfile);
        assertEquals(firstProfile.getFirstname(), secondProfile.getFirstname());
        assertEquals(firstProfile.getNickname(), secondProfile.getNickname());
    }

    @Test
    void testGetListOfCountryNames(){
        assertEquals(2, firstProfile.getPassports().size());
        assertEquals(firstProfile.getPassports().get(0).getClass(), String.class);
    }

    @Test
    void testGetDateOfBirthInCorrectStringFormat(){
        String expectedString = "2000-12-15";
        assertEquals(expectedString, firstProfile.getDateOfBirth());
    }

}