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
            activityTypeRepository.save(new ActivityType("Acrobatics"));
            activityTypeRepository.save(new ActivityType("Basketball"));
            activityTypeRepository.save(new ActivityType("Cardio"));
            activityTypeRepository.save(new ActivityType("Cycling"));
            activityTypeRepository.save(new ActivityType("Football"));
            activityTypeRepository.save(new ActivityType("Functional"));
            activityTypeRepository.save(new ActivityType("E-sports"));
            activityTypeRepository.save(new ActivityType("Golf"));
            activityTypeRepository.save(new ActivityType("Gymnastics"));
            activityTypeRepository.save(new ActivityType("HIIT"));
            activityTypeRepository.save(new ActivityType("Hiking"));
            activityTypeRepository.save(new ActivityType("Hockey"));
            activityTypeRepository.save(new ActivityType("Jogging"));
            activityTypeRepository.save(new ActivityType("Martial Arts"));
            activityTypeRepository.save(new ActivityType("Netball"));
            activityTypeRepository.save(new ActivityType("Rock Climbing"));
            activityTypeRepository.save(new ActivityType("Rugby"));
            activityTypeRepository.save(new ActivityType("Running"));
            activityTypeRepository.save(new ActivityType("Sprinting"));
            activityTypeRepository.save(new ActivityType("Swimming"));
            activityTypeRepository.save(new ActivityType("Tennis"));
            activityTypeRepository.save(new ActivityType("Tramping"));
            activityTypeRepository.save(new ActivityType("Weight Lifting"));
            activityTypeRepository.save(new ActivityType("Yoga"));
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
    }
}
