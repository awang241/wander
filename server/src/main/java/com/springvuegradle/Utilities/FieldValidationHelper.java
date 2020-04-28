package com.springvuegradle.Utilities;

import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.PassportCountry;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ActivityTypeRepository;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.PassportCountryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashSet;

@Service
public class FieldValidationHelper {

    /**
     * Validates the activity object by making the checks:
     * - if duration based, start and end times not null
     * - if start time selected, end time also selected (vice versa)
     * - start time comes before end time
     *
     * @param activity the activity object we want validated
     * @return string containing a description of the error in validation (if error occurs)
     */
//    public static String validateActivity(Activity activity) {
//        String result = "";
//        if (!activity.getContinuous() && activity.getStartTime() == null && activity.getEndTime() == null) {
//            result += "If a duration based activity is chosen, it must have start and end times.\n";
//        }
//        if (activity.getStartTime() != null && activity.getEndTime() == null) {
//            result += "Start time selected, but end time is not selected.\n";
//        }
//        if (activity.getStartTime() == null && activity.getEndTime() != null) {
//            result += "End time selected, but start time is not selected.\n";
//        }
//        if (activity.getStartTime() != null && activity.getEndTime() != null &&
//                activity.getStartTime() > activity.getEndTime()) {
//            result += "The start time must come before the end time.\n";
//        }
//        return result;
//    }

    /**
     * Used in the create profile and update profile methods for verification. Returns a string to let the user know what
     * was entered incorrectly.
     *
     * @param newProfile object we want to complete verification on
     * @param edit_mode  indicates whether the method is being called from the update profile method to skip email authentication
     * @param pcRepo     the passport country repository
     * @param aRepo      the activity type repository
     * @param eRepo      the email repository
     * @return a string containing all the errors in the form, if any, else it will return an empty string
     */
    public static String verifyProfile(Profile newProfile, boolean edit_mode, PassportCountryRepository pcRepo,
                                       ActivityTypeRepository aRepo, EmailRepository eRepo) {
        String error = "";
        if (newProfile.retrievePrimaryEmail().getAddress() == "" ||
                newProfile.retrievePrimaryEmail().getAddress() == null) {
            error += "The email field is blank.\n";
        }
        if (newProfile.getFirstname() == "" ||
                newProfile.getFirstname() == null) {
            error += "The First Name field is blank.\n";
        }
        if (newProfile.getLastname() == "" ||
                newProfile.getLastname() == null) {
            error += "The Last Name field is blank.\n";
        }
        if (newProfile.getPassword().length() < 8) {
            error += "The Password is not long enough.\n";
        }
        if (newProfile.getFitness() > 4 || newProfile.getFitness() < 0) {
            error += "The fitness level isn't valid.\n";
        }
        if (newProfile.getDateOfBirth() == "" ||
                newProfile.getDateOfBirth() == null) {
            error += "The Date of Birth field is blank.\n";
        }
        if (!newProfile.getPassportObjects().isEmpty()) {
            for (PassportCountry passportCountry : newProfile.getPassportObjects()) {
                if (!pcRepo.existsByCountryName(passportCountry.getCountryName())) {
                    error += String.format("Country %s does not exist in the database.\n", passportCountry.getCountryName());
                }
            }
        }
        if (!newProfile.getActivityTypeObjects().isEmpty()) {
            for (ActivityType activityType : newProfile.getActivityTypeObjects()) {
                if (!aRepo.existsByActivityTypeName(activityType.getActivityTypeName())) {
                    error += String.format("ActivityType %s does not exist in the database.\n", activityType.getActivityTypeName());
                }
            }
        }

        if (!edit_mode) {
            error += verifyEmailsInProfile(newProfile, eRepo);
        }
        if (!((newProfile.getGender().equals("male")) ||
                (newProfile.getGender().equals("female")) ||
                (newProfile.getGender().equals("non-Binary")))) {
            error += "The Gender field must contain either 'male', 'female' or 'non-Binary'.\n";
        }
        return error;
    }

    /**
     * Method used to verify emails and check if the emails are already associated with a profile in the database.
     *
     * @param newProfile object we want to verify the emails for
     * @return string containing error messages, if any, else empty string
     */
    private static String verifyEmailsInProfile(Profile newProfile, EmailRepository eRepo) {
        String error = "";
        if (newProfile.retrieveEmails().size() >= 1) {
            boolean valid = true;
            ArrayList<String> emailStrings = new ArrayList<>();
            for (Email email : newProfile.retrieveEmails()) {
                if (eRepo.existsByAddress(email.getAddress())) {
                    valid = false;
                } else {
                    emailStrings.add(email.getAddress());
                }
            }
            if (emailStrings.size() != (new HashSet(emailStrings)).size()) {
                error += "There are duplicate email addresses used. User cannot enter same email multiple times in primary" +
                        " and/or additional.\n";
            } else if (!valid) {
                error += "An email address you have entered is already in use by another Profile.\n";
            }
        }
        return error;
    }
}
