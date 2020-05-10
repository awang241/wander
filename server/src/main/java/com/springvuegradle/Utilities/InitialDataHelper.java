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

import static com.springvuegradle.Controller.Profile_Controller.hashPassword;

public class InitialDataHelper {

    /**
     * Load initial activity types into the activity type database
     *
     * @param activityTypeRepository the way to access the activity type repository
     */
    public static void updateActivityTypeRepository(ActivityTypeRepository activityTypeRepository) {
        if (activityTypeRepository.count() == 0) {
            activityTypeRepository.save(new ActivityType("Football"));
            activityTypeRepository.save(new ActivityType("Tennis"));
            activityTypeRepository.save(new ActivityType("Hockey"));
            activityTypeRepository.save(new ActivityType("Basketball"));
            activityTypeRepository.save(new ActivityType("Hiking"));
            activityTypeRepository.save(new ActivityType("Rock Climbing"));
            activityTypeRepository.save(new ActivityType("Tramping"));
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
        String[] passportCountries = {"USA", "UK"};
        String[] extraEmails = {"throwayway1@gmail.com", "throwaway2@gmail.com"};
        String[] activityTypes = {"Basketball", "Football"};
        List<Profile> steves = repo.findByPrimaryEmail("Steve@test.com");
        List<Profile> daves = repo.findByPrimaryEmail("Dave@test.com");
        if (steves.size() == 0) {
            Profile regularProfile = new Profile(1L, "Steve", "Tester", "The", "Stevetest",
                    "Steve@test.com", extraEmails, "987654321", "Here to run some tests!", calendar1,
                    "Male", 2, passportCountries, activityTypes);
            repo.save(regularProfile);
            Email regularEmail = regularProfile.retrievePrimaryEmail();
            regularEmail.setProfile(regularProfile);
            erepo.save(regularEmail);
        }

        if (daves.size() == 0) {
            Profile adminProfile = new Profile(2L, "Dave", "Tester", "The", "Davetest",
                    "Dave@test.com", extraEmails, "SecureAdminPassword", "I'm a model Admin!", calendar2,
                    "Male", 2, passportCountries, activityTypes);
            adminProfile.setAuthLevel(1);
            repo.save(adminProfile);
            Email adminEmail = adminProfile.retrievePrimaryEmail();
            adminEmail.setProfile(adminProfile);
            erepo.save(adminEmail);
        }
    }

    /**
     * Loads an initial default admin if one does not exist.
     *
     * @param repo  the profile repository
     * @param erepo the email repository
     */
    public static void updateDefaultAdmin(ProfileRepository repo, EmailRepository erepo) {
        List<Profile> default_admins = repo.findByAuthLevel(0);
        if (default_admins.size() == 0) {
            Profile admin = new Profile(null, "Admin", "Profile", "", "",
                    "default@admin.com", new String[]{}, Profile_Controller.hashPassword("admin"), "", new GregorianCalendar(1992,
                    Calendar.JUNE, 10), "male", 0, new String[]{}, new String[]{});
            admin.setAuthLevel(0);
            repo.save(admin);
            Email adminEmail = admin.retrievePrimaryEmail();
            adminEmail.setProfile(admin);
            erepo.save(adminEmail);
        }

    }


    /**
     * If the database is empty, it will load in the default admin account as well as the default activity types.
     *
     * @param arepo the activity repository
     * @param repo the profile repository
     * @param erepo the email repository
     */
    public static void init(ActivityTypeRepository arepo, ProfileRepository repo, EmailRepository erepo) {
        updateActivityTypeRepository(arepo);
        updateDefaultAdmin(repo, erepo);
        addExampleProfiles(repo, erepo);
    }
}
