package com.springvuegradle.Model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class ProfileTest {

    private Profile firstProfile;
    private Profile secondProfile;
    private Profile firstProfileAgain;
    private Set<PassportCountry> passportCountries;

    @BeforeEach
    void setUp() {
        Calendar calendar = new GregorianCalendar(2000, 11, 15);
        /*
        firstProfile = new Profile("Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", "987654321", "Here to run some tests!", calendar,
                "Male");
        secondProfile = new Profile("Dave", "Tester", "The", "Davetest",
                "Dave@test.com", "987654321", "Here to run some tests!", calendar,
                "Male");
        firstProfileAgain = new Profile("Steve", "Tester", "The", "Stevetest",
                "Steve@test.com", "987654321", "Here to run some tests!", calendar,
                "Male");

        PassportCountry firstCountry = new PassportCountry("USA");
        PassportCountry secondCountry = new PassportCountry("UK");
        passportCountries = Set.of(firstCountry, secondCountry);
        */
    }

    @Disabled
    @Test
    void testTwoProfilesAreEqual(){
        assertEquals(firstProfile, firstProfileAgain);
    }

    @Disabled
    @Test
    void testUpdateProfileWithNewInfo(){
        firstProfile.updateProfile(secondProfile);
        assertEquals(firstProfile.getFirstname(), secondProfile.getFirstname());
        assertEquals(firstProfile.getNickname(), secondProfile.getNickname());
        //assertEquals(firstProfile.getEmail(), secondProfile.getEmail());
    }


    @Disabled
    @Test
    void testGetListOfCountryNames(){
        firstProfile.setPassportCountries(passportCountries);
        assertEquals(2, firstProfile.getPassportCountryNames().size());
        assertEquals(firstProfile.getPassportCountryNames().get(0).getClass(), String.class);
    }

    @Disabled
    @Test
    void testGetDateOfBirthInCorrectStringFormat(){
        String expectedString = "2000-12-15";
        assertEquals(expectedString, firstProfile.getDateOfBirth());
    }

}