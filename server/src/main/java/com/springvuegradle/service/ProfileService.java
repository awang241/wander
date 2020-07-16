package com.springvuegradle.service;

import com.springvuegradle.Model.*;
import com.springvuegradle.Repositories.ProfileLocationRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.Repositories.spec.ProfileSpecifications;
import com.springvuegradle.Utilities.FieldValidationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


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
    public ProfileService(ProfileRepository profileRepository) {
        this.profileRepository = profileRepository;
    }


    /**
     * Updates the location associated with a users profile
     * @param newLocation the new location for the users profile
     * @param id the id of the profile whose location is being edited
     * @return a response status detailing if the operation was successful
     */
    public ResponseEntity<String> updateProfileLocation(ProfileLocation newLocation, Long id) {
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
     * @param token the token sent with the http request
     * @param id the id of the profile whose location is being edited
     * @return a response status detailing if the operation was successful
     */
    public ResponseEntity<String> deleteProfileLocation(String token, Long id) {
        if(!securityService.checkEditPermission(token, id)){
            return new ResponseEntity<>("Permission denied", HttpStatus.FORBIDDEN);
        }
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

}
