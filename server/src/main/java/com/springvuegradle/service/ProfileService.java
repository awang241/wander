package com.springvuegradle.service;

import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Class that handles business logic for profiles.
 */
@Service
public class ProfileService {

    ProfileRepository profileRepository;

    @Autowired
    public ProfileService(ProfileRepository profileRepository) {
        this.profileRepository = profileRepository;
    }

    /**
     * Given a page index and size, calculates the appropriate page in the list of all profiles with the
     * provided name. The profiles that are in that page are then returned.
     *
     * The provided name can either be a single word or multiple space-separated words. In the case of a single word, it
     * will match profiles with that surname or nickname. Multiple words are interpreted as a full name, where the first
     * and last words are taken as the first and last names respectively, with all other words as the middle name(s).
     *
     * @param name The name to be matched to.
     * @param pageSize The maximum number of profiles that a page can have.
     * @param pageNumber The index of the page to be returned. Page indices start at 0.
     * @return The list of profiles that belong to the page.
     */
    public List<Profile> getUsersByName(String name, Integer pageSize, Integer pageNumber) {
        throw new UnsupportedOperationException("Not yet implemented");
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
