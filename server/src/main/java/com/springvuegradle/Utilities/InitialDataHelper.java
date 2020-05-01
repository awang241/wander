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
        }
    }

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

    public static void init(ActivityTypeRepository arepo, ProfileRepository repo, EmailRepository erepo) {
        updateActivityTypeRepository(arepo);
        updateDefaultAdmin(repo, erepo);
    }
}
