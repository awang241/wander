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

@Service
public class ProfileService {

    @Autowired
    private SecurityUtil securityUtil;

    @Autowired
    private ProfileLocationRepository profileLocationRepository;

    @Autowired
    private ProfileRepository profileRepository;

    public ResponseEntity<String> updateProfileLocation(ProfileLocation newLocation, String token, Long id) {
        //if(!securityUtil.checkEditPermission(token, id)){
         //   return new ResponseEntity<>("Permission denied", HttpStatus.FORBIDDEN);
        //}
        Profile profile = profileRepository.getOne(id);
        newLocation.setProfile(profile);
        profile.setLocation(newLocation);
        profileRepository.save(profile);
        return new ResponseEntity<>(HttpStatus.OK);
    }
}
