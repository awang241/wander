package com.springvuegradle.service;

import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.Profile_;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.Repositories.spec.ProfileSpecifications;
import com.springvuegradle.Utilities.FieldValidationHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
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
     * Given a page index and size, calculates the appropriate page all profiles with the
     * provided name. The profiles that are in that page are then returned.
     *
     * The provided name can either be a single word or multiple space-separated words. In the case of a single word, it
     * will match profiles with that surname or nickname. Multiple words are interpreted as a full name, where the first
     * and last words are taken as the first and last names respectively, with all other words as the middle name(s).
     *
     * @param name The name to be matched to.
     * @param pageSize The maximum number of profiles that a page can have.
     * @param pageNumber The index of the page to be returned. Page indices start at 0.
     * @return The page of profiles requested.
     */
    public Page<Profile> getUsersByName(String name, Integer pageSize, Integer pageNumber) {
        PageRequest p = PageRequest.of(pageNumber, pageSize);
        Specification<Profile> spec = ProfileSpecifications.nicknameContains(name).or(ProfileSpecifications.lastNameContains(name));
        return profileRepository.findAll(spec, p);
    }

    public Page<Profile> getUsers(String firstName, String middleName, String lastName, String nickname, String email, PageRequest request) {
        Specification<Profile> spec = ProfileSpecifications.notDefaultAdmin();
        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(firstName))) {
            spec = spec.and(ProfileSpecifications.firstNameContains(firstName));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(middleName))) {
            spec = spec.and(ProfileSpecifications.middleNameContains(middleName));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(lastName))) {
            spec = spec.and(ProfileSpecifications.lastNameContains(lastName));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(nickname))) {
            spec = spec.and(ProfileSpecifications.nicknameContains(nickname));
        }

        if (Boolean.FALSE.equals(FieldValidationHelper.isNullOrEmpty(email))) {
            spec = spec.and(ProfileSpecifications.hasEmail(new Email(email)));
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
