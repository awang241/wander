package com.springvuegradle.service;

import com.springvuegradle.enums.AuthLevel;
import com.springvuegradle.enums.ProfileErrorMessage;
import com.springvuegradle.model.*;
import com.springvuegradle.repositories.ActivityMembershipRepository;
import com.springvuegradle.repositories.EmailRepository;
import com.springvuegradle.repositories.ProfileLocationRepository;
import com.springvuegradle.repositories.ProfileRepository;
import com.springvuegradle.repositories.spec.ProfileSpecifications;
import com.springvuegradle.utilities.FieldValidationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

/**
 * Service-layer class containing all business logic handling profiles.
 */
@Service
public class ProfileService {

    @Autowired
    private SecurityService securityService;

    @Autowired
    private ProfileLocationRepository profileLocationRepository;

    @Autowired
    private ProfileRepository profileRepository;

    @Autowired
    private EmailRepository emailRepository;

    @Autowired
    public ProfileService(ProfileRepository profileRepository) {
        this.profileRepository = profileRepository;
    }

    @Autowired
    private ProfileRepository repo;

    /**
     * Way to access Email Repository (Email table in db).
     */
    @Autowired
    private EmailRepository eRepo;

    @Autowired
    private ActivityMembershipRepository actMemRepo;

    /**
     * Updates the location associated with a users profile
     * @param newLocation the new location for the users profile
     * @param id the id of the profile whose location is being edited
     * @return a response status detailing if the operation was successful
     */
    public ResponseEntity<String> updateProfileLocation(ProfileLocation newLocation, Long id) {
        if(FieldValidationHelper.isNullOrEmpty(newLocation.getCity()) || FieldValidationHelper.isNullOrEmpty(newLocation.getCountry())){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        Optional<Profile> optionalProfile = profileRepository.findById(id);
        if(optionalProfile.isEmpty()){
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        Profile profile = optionalProfile.get();
        Optional<ProfileLocation> location = profileLocationRepository.findLocationByProfile(profile);
        if(location.isPresent()) {
            location.get().update(newLocation);
        } else {
            profile.setLocation(newLocation);
        }
        profileRepository.save(profile);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Deletes a location from the profile with the given ID if it exists
     * @param id the id of the profile whose location is being edited
     * @return a response status detailing if the operation was successful
     */
    public ResponseEntity<String> deleteProfileLocation(Long id) {
        Optional<Profile> optionalProfile = profileRepository.findById(id);
        if(optionalProfile.isEmpty()){
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        Profile profile = optionalProfile.get();
        ProfileLocation location = profile.getProfileLocation();
        if(location != null) {
            profile.setLocation(null);
            profileLocationRepository.deleteById(location.getId());
        }
        profileRepository.save(profile);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Returns the specified page from the list of all profiles that match the search criteria.
     *
     * @param criteria A ProfileSearchCriteria object containing the relevant criteria
     * @param request A page request containing the index and size of the page to be returned.
     * @return The specified page from the list of all profiles that match the search criteria.
     */
    public Page<Profile> getUsers(ProfileSearchCriteria criteria, Pageable request) {
        Specification<Profile> spec = ProfileSpecifications.notDefaultAdmin();

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(criteria.getFirstName()))) {
            spec = spec.and(ProfileSpecifications.firstNameContains(criteria.getFirstName()));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(criteria.getMiddleName()))) {
            spec = spec.and(ProfileSpecifications.middleNameContains(criteria.getMiddleName()));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(criteria.getLastName()))) {
            spec = spec.and(ProfileSpecifications.lastNameContains(criteria.getLastName()));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(criteria.getNickname()))) {
            spec = spec.and(ProfileSpecifications.nicknameContains(criteria.getNickname()));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(criteria.getEmailAddress()))) {
            spec = spec.and(ProfileSpecifications.emailContains(criteria.getEmailAddress()));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(criteria.getActivityTypes()))) {
            spec = spec.and(ProfileSpecifications.activityTypesContains(criteria.getActivityTypes(), criteria.getSearchMethod()));
        }

        return profileRepository.findAll(spec, request);
    }

    /**
     * Deletes a profile and related data from the repository given that it exists in the database.
     * Checks if the profile about to be deleted is the default admin and stops if it is the default admin.
     * @param id the id of the profile to be deleted
     * @return http response code and feedback message on the result of the delete operation
     */
    public ResponseEntity<String> deleteProfile(Long id) {
        Optional<Profile> result = repo.findById(id);
        if (Boolean.TRUE.equals(result.isPresent())) {
            Profile profileToDelete = result.get();
            deleteProfileLocation(id);
            if (profileToDelete.getAuthLevel() == 0) {
                return new ResponseEntity<>("Cannot delete default admin.", HttpStatus.FORBIDDEN);
            }

            for (Email email: profileToDelete.retrieveEmails()) {
                eRepo.delete(email);
            }
            for (ActivityMembership membership: profileToDelete.getActivities()) {
                actMemRepo.delete(membership);
            }
            profileToDelete.setActivityTypes(null);

            repo.delete(profileToDelete);
            return new ResponseEntity<>("The Profile does exist in the database.", HttpStatus.OK);
        } else {
            return new ResponseEntity<>("The profile does not exist in the database.", HttpStatus.NOT_FOUND);
        }
    }

    /**
     * Sets the auth level of the profile with the given ID to the given value.
     * @param userId The ID of the profile being changed.
     * @param newAuthLevel The new auth level.
     * Throws an exception if the auth level is an invalid one or if the id of the profile is not found in the profile repository
     */
    public void setUserAuthLevel(long userId, AuthLevel newAuthLevel) {
        if (newAuthLevel.getLevel() < AuthLevel.ADMIN.getLevel() ||
                newAuthLevel.getLevel() > AuthLevel.USER.getLevel()) {
            throw new IllegalArgumentException(ProfileErrorMessage.INVALID_AUTH_LEVEL.getMessage());
        }

        Optional<Profile> result = profileRepository.findById(userId);
        if (result.isEmpty()) {
            throw new IllegalArgumentException(ProfileErrorMessage.PROFILE_NOT_FOUND.getMessage());
        } else {
            Profile targetProfile = result.get();
            targetProfile.setAuthLevel(newAuthLevel.getLevel());
            profileRepository.save(targetProfile);
        }
    }

    /**
     * Checks if an email address exists in the database.
     * @param email The email in string that is being checked if it exists in the database
     * @return a boolean based on whether the email exists in the database or not.
     */
    public boolean checkEmailExistsInDB(String email) {
        Optional<Email> result = emailRepository.findByAddress(email);
        if (result.isEmpty()) {
           return false;
        }
        return true;
    }
}
