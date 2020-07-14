package com.springvuegradle.service;

import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileSearchCriteria;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.Repositories.spec.ProfileSpecifications;
import com.springvuegradle.Utilities.FieldValidationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Class that handles business logic for profiles.
 */
@Service
public class ProfileService {

    private ProfileRepository profileRepository;
    private EmailRepository emailRepository;

    @Autowired
    public ProfileService(ProfileRepository profileRepository, EmailRepository emailRepository) {
        this.profileRepository = profileRepository;
        this.emailRepository = emailRepository;
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
            Email searchEmail;
            Optional<Email> result = emailRepository.findByAddress(criteria.getEmail());
            if (result.isPresent()) {
                searchEmail = result.get();
                spec = spec.and(ProfileSpecifications.hasEmail(searchEmail));
            } else {
                spec = spec.and(Specification.not(spec));
            }
        }
        /*
        if (!FieldValidationHelper.isNullOrEmpty(activityTypes)) {
            spec = spec.and(ProfileSpecifications.activityTypesContains(activityTypes, matchAll));
        }
         */
        return profileRepository.findAll(spec, request);
    }

}
