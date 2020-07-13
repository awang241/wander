package com.springvuegradle.service;

import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileSearchCriteria;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.Repositories.spec.ProfileSpecifications;
import com.springvuegradle.Utilities.FieldValidationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Class that handles business logic for profiles.
 */
@Service
public class ProfileService {

    private ProfileRepository profileRepository;

    @Autowired
    public ProfileService(ProfileRepository profileRepository) {
        this.profileRepository = profileRepository;
    }

    /**
     * Given a page index and size, calculates the appropriate page all profiles . The profiles that are in that page are then returned.
     *
     *

     */
    public Page<Profile> getUsersByName(String name, Integer pageSize, Integer pageNumber) {
        PageRequest p = PageRequest.of(pageNumber, pageSize);
        Specification<Profile> spec = ProfileSpecifications.nicknameContains(name).or(ProfileSpecifications.lastNameContains(name));
        return profileRepository.findAll(spec, p);
    }

    /**
     * Returns the specified page from the list of all profiles that match the search criteria.
     *
     * Criteria given
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

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(criteria.getEmail()))) {
            spec = spec.and(ProfileSpecifications.hasEmail(new Email(criteria.getEmail())));
        }
        /*
        if (!FieldValidationHelper.isNullOrEmpty(activityTypes)) {
            spec = spec.and(ProfileSpecifications.activityTypesContains(activityTypes, matchAll));
        }
         */
        return profileRepository.findAll(spec, request);
    }

    /**
     * Given a page index and size, calculates the appropriate page in the list of all profiles that have the
     * provided email. The profiles that are in that page are then returned.
     *
     * @param email The email to be matched to.
     * @param pageSize The maximum number of profiles that a page can have.
     * @param pageNumber The index of the page to be returned. Page indices start at 0.
     * @return The list of profiles that belong to the page.
     */
    public List<Profile> getUsersByEmail(String email, Integer pageSize, Integer pageNumber) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

}
