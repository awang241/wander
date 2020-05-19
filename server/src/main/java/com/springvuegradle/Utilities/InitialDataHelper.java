package com.springvuegradle.Utilities;

import com.springvuegradle.Controller.Profile_Controller;
import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.ProfileRepository;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Random;

public class InitialDataHelper {

    private InitialDataHelper(){};

    /**
     * Load initial activity types into the activity type database
     *
     * @param activityTypeRepository the way to access the activity type repository
     */
    public static void updateActivityTypeRepository(ActivityTypeRepository activityTypeRepository) {
        List<String> activityNames = List.of("Acrobatics", "Basketball", "Cardio", "Cycling", "Football", "Functional",
                "E-sports", "Golf", "Gymnastics", "HIIT", "Hiking", "Hockey", "Jogging", "Martial Arts", "Netball",
                "Rock Climbing", "Rugby", "Running", "Sprinting", "Swimming", "Tennis", "Tramping", "Weight Lifting",
                "Yoga");

        for (String activity: activityNames) {
            if (!activityTypeRepository.existsByActivityTypeName(activity)) {
                activityTypeRepository.save(new ActivityType(activity));
            }
        }
    }

    /**
     * Adds two example profiles to the database specified in the readme.
     *
     * @param repo  the profile repository
     * @param erepo the email repository
     */
    public static void addExampleProfiles(ProfileRepository repo, EmailRepository erepo) {
        Calendar calendar1 = new GregorianCalendar(2000, 11, 5);
        Calendar calendar2 = new GregorianCalendar(2000, 11, 20);
        String[] extraEmails = {"throwayway1@gmail.com", "throwaway2@gmail.com"};
        List<Profile> steves = repo.findByPrimaryEmail("Steve@test.com");
        List<Profile> daves = repo.findByPrimaryEmail("Dave@test.com");
        if (steves.size() == 0) {
            Profile regularProfile = new Profile(1L, "Steve", "Tester", "The", "Stevetest",
                    "Steve@test.com", extraEmails, Profile_Controller.hashPassword("987654321"), "Here to run some tests!", calendar1,
                    "Male", 2, new String[]{}, new String[]{});
            repo.save(regularProfile);
            Email regularEmail = regularProfile.retrievePrimaryEmail();
            regularEmail.setProfile(regularProfile);
            erepo.save(regularEmail);
        }

        if (daves.size() == 0) {
            Profile daveAdminProfile = new Profile(2L, "Dave", "Tester", "The", "Davetest",
                    "Dave@test.com", extraEmails, Profile_Controller.hashPassword("SecureAdminPassword"), "I'm a model Admin!", calendar2,
                    "Male", 2, new String[]{}, new String[]{});
            daveAdminProfile.setAuthLevel(1);
            repo.save(daveAdminProfile);
            Email daveEmail = daveAdminProfile.retrievePrimaryEmail();
            daveEmail.setProfile(daveAdminProfile);
            erepo.save(daveEmail);
        }
    }

    /**
     * Loads an initial default admin if one does not exist.
     *
     * @param repo  the profile repository
     * @param erepo the email repository
     * @return
     */
    public static String updateDefaultAdmin(ProfileRepository repo, EmailRepository erepo) {
        List<Profile> default_admins = repo.findByAuthLevel(0);

        if (default_admins.size() == 0) {
            int minAscii = 48;
            int maxAscii = 122;
            int passwordLength = 20;
            Random random = new Random();
            String password = random.ints(minAscii, maxAscii).filter(i -> (i <= 57 || i >= 65) && (i <= 90 || i >= 97))
                    .limit(passwordLength)
                    .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
                    .toString();
            Profile admin = new Profile(null, "Admin", "Profile", "", "",
                    "default@admin.com", new String[]{}, Profile_Controller.hashPassword(password), "", new GregorianCalendar(1992,
                    Calendar.JUNE, 10), "male", 0, new String[]{}, new String[]{});
            admin.setAuthLevel(0);
            repo.save(admin);
            Email adminEmail = admin.retrievePrimaryEmail();
            adminEmail.setProfile(admin);
            erepo.save(adminEmail);
            return password;
        }
        return null;
    }


    /**
     * If the database is empty, it will load in the default admin account as well as the default activity types.
     *
     * @param arepo the activity repository
     * @param repo the profile repository
     * @param erepo the email repository
     */
    public static String init(ActivityTypeRepository arepo, ProfileRepository repo, EmailRepository erepo) {
        updateActivityTypeRepository(arepo);
        String password = updateDefaultAdmin(repo, erepo);
        addExampleProfiles(repo, erepo);
        return password;
    }
}
