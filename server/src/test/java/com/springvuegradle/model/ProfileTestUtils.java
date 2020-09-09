package com.springvuegradle.model;

import com.springvuegradle.enums.NotificationType;

import java.util.*;

public class ProfileTestUtils {
    /* Below are a set of ready-made Profile objects which can be used for various tests. */

    public static Notification createNormalNotificationWithTypeBeing0() {
        return new Notification("Activity: bobbing in bobbington, has been created hooray", null, null, NotificationType.ActivityCreated);
    }

    public static Notification createNormalNotificationWithTypeBeing2() {
        return new Notification("Activity: bobbing in bobbington, has been edited, name is now gobbing in gobbington", null, null, NotificationType.ActivityEdited);
    }

    public static List<Notification> createListOfNormalNotifications() {
        List<Notification> notificationsList = new ArrayList<>();
        notificationsList.add(createNormalNotificationWithTypeBeing0());
        notificationsList.add(createNormalNotificationWithTypeBeing2());
        return notificationsList;
    }

    /**
     * @return a valid profile object.
     */
    public static Profile createProfileJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{});
    }

    /**
     * Creates a new profile with the same names as profile created by createNormalProfileJimmy().
     * @return a new profile.
     */
    public static Profile createProfileJimmyAlternate(){
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "notJimmyOne@mail.com", new String[]{}, "shush",
                "Not that guy", new GregorianCalendar(1990, Calendar.JUNE, 17), "non-Binary", 0,
                new String[]{}, new String[]{});
    }

    public static Profile createProfileNicknameMatchesJimmySurname(){
        return new Profile(null, "Tim", "Tam", null, "Quick", "biscuit@mail.com", new String[]{}, "sandwich",
                "chcoclate biscuit", new GregorianCalendar(1992, Calendar.FEBRUARY, 2), "non-Binary", 2,
                new String[]{}, new String[]{});
    }

    public static List<Profile> createProfilesWithSameSurnameAsJimmy(){
        List<Profile> profiles = new ArrayList<>();
        profiles.add(new Profile(null, "Tommy", "Quick", null, null,
                "quick@mail.com", new String[]{}, "password","Surname's fast",
                new GregorianCalendar(1989, Calendar.FEBRUARY, 13), "non-Binary", 0,
                new String[]{}, new String[]{}));

        profiles.add(new Profile(null, "Timmy", "Quick", null, null,
                "timmyquick@mail.com", new String[]{}, "password","Surname's the same",
                new GregorianCalendar(1980, Calendar.NOVEMBER, 23), "male", 0,
                new String[]{}, new String[]{}));

        profiles.add(new Profile(null, "Tammy", "Quick", null, null,
                "quack@mail.com", new String[]{}, "password","Surname's also the same",
                new GregorianCalendar(1992, Calendar.JUNE, 8), "female", 0,
                new String[]{}, new String[]{}));

        profiles.add(new Profile(null, "Tina", "Quick", null, null,
                "someemail@mail.com", new String[]{}, "password","Surname's still the same",
                new GregorianCalendar(1994, Calendar.FEBRUARY, 19), "female", 0,
                new String[]{}, new String[]{}));

        profiles.add(new Profile(null, "Bill", "Quick", null, null,
                "nail@mail.com", new String[]{}, "password","Suriname",
                new GregorianCalendar(1994, Calendar.FEBRUARY, 19), "male", 0,
                new String[]{}, new String[]{}));

        return profiles;
    }

    /**
     * @return a valid profile object with activityType types
     */
    public static Profile createProfileWithActivityTypes() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{"Football", "Tennis"});
    }

    /**
     * @return a profile object with invalid activityType types
     */
    public static Profile createProfileWithInvalidActivityTypes() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"New Zealand", "India"}, new String[]{"Not a real activityType", "Tennis"});
    }

    /**
     * @return a valid profile object with 3 activityTypes.
     */
    public static Profile createNormalActivityTypesProfile() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{"Football", "Hockey", "Basketball"});
    }

    /**
     * @return a valid profile object with updated activityTypes.
     */
    public static Profile createUpdatedActivityTypesProfile() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{}, new String[]{"Hiking"});
    }

    /**
     * @return a profile object with an invalid country name.
     */
    public static Profile createInvalidCountryProfileJimmy() {
        return new Profile(null, "Jimmy", "Quick", "Jones", "Jim-Jam", "jimjam@hotmail.com", new String[]{"additional@email.com"}, "hushhush",
                "The quick brown fox jumped over the lazy dog.", new GregorianCalendar(1999, Calendar.NOVEMBER,
                28), "male", 1, new String[]{"Cowabunga"}, new String[]{});
    }

    /**
     * @return a valid profile object.
     */
    public static Profile createNormalProfileMaurice() {
        return new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{"New Zealand", "China"}, new String[]{});
    }

    /**
     * @return an invalid profile object with fields containing empty strings.
     */
    public static Profile createInvalidFieldsProfileMaurice() {
        return new Profile(null, "", "", "Jack", "Jacky", "", new String[]{"additionaldoda@email.com"}, "hush",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "Male", 10, new String[]{"New Zealand", "India"}, new String[]{"random"});
    }

    /**
     * @return a profile with the minimal required fields to be successfully created
     */
    public static Profile createProfileWithMinimalFields() {
        return new Profile(null, "Steven", "Stevenson", "", "",
                "steven@steven.com", new String[]{}, "12345678", "", new GregorianCalendar(1992,
                Calendar.JUNE, 10), "male", 0, new String[]{}, new String[]{});
    }
}
