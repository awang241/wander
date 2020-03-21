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


        firstProfile = new Profile(Long.valueOf(1), "Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar,
                "Male", 2, passportCountries);
        secondProfile = new Profile(Long.valueOf(2), "Dave", "Tester", "The", "Davetest",
                "Dave@test.com", extraEmails, "987654321", "Here to run some tests!", calendar,
                "Male", 2, passportCountries);
        firstProfileAgain = new Profile(Long.valueOf(3), "Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar,
                "Male", 2, passportCountries);


    }

    @Test
    void testTwoProfilesAreEqual(){
        assertTrue(firstProfile.equals(firstProfileAgain));
    }

    @Test
    void testUpdateProfileWithNewInfo(){
        firstProfile.updateProfile(secondProfile);
        assertEquals(firstProfile.getFirstname(), secondProfile.getFirstname());
        assertEquals(firstProfile.getNickname(), secondProfile.getNickname());
        assertEquals(firstProfile.getPrimary_email(), secondProfile.getPrimary_email());
    }

    @Test
    void testGetListOfCountryNames(){
        assertEquals(2, firstProfile.getPassportCountryNames().size());
        assertEquals(firstProfile.getPassportCountryNames().get(0).getClass(), String.class);
    }

    @Test
    void testGetDateOfBirthInCorrectStringFormat(){
        String expectedString = "2000-12-15";
        assertEquals(expectedString, firstProfile.getDateOfBirth());
    }

}