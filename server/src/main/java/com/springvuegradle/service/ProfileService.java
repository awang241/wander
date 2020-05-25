package com.springvuegradle.service;

import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileLocation;
import com.springvuegradle.Repositories.ProfileLocationRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import com.springvuegradle.Utilities.SecurityUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;


@Service
public class ProfileService {

    @Autowired
    private SecurityUtil securityUtil;

    @Autowired
    private ProfileLocationRepository profileLocationRepository;

    @Autowired
    private ProfileRepository profileRepository;

    /**
     * Updates the location associated with a users profile
     * @param newLocation the new location for the users profile
     * @param token the token sent with the http request
     * @param id the id of the profile whose location is being edited
     * @return a response status detailing if the operation was successful
     */
    public ResponseEntity<String> updateProfileLocation(ProfileLocation newLocation, String token, Long id) {
        if(!securityUtil.checkEditPermission(token, id)){
            return new ResponseEntity<>("Permission denied", HttpStatus.FORBIDDEN);
        }
        Profile profile = profileRepository.findById(id).get();
        Optional<ProfileLocation> location = profileLocationRepository.findLocationByProfile(profile);
        if(location.isPresent()) {
            location.get().update(newLocation);
        } else {
            profile.setLocation(newLocation);
        }
        profileRepository.save(profile);
        return new ResponseEntity<>(HttpStatus.OK);
    }
}
