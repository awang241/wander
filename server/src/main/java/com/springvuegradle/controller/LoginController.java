package com.springvuegradle.controller;

import com.springvuegradle.utilities.JwtUtil;
import com.springvuegradle.dto.requests.LoginRequest;
import com.springvuegradle.dto.responses.LoginResponse;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.EmailRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;

/**
 * Basic implementation of a login controller class.
 *
 * @author Alan Wang
 *
 */
@RestController
public class LoginController {

    private JwtUtil jwtUtil;
    private EmailRepository eRepo;

    @Autowired
    public LoginController(JwtUtil jwtUtil, EmailRepository eRepo) {
        this.jwtUtil = jwtUtil;
        this.eRepo = eRepo;
    }

    /**
     * Attempts to log in a user given a login request. If the credentials are correct, the user is logged
     * in and the session is recorded; otherwise, returns an error code.
     *
     * @param request the user's email and password mapped from the request body onto a LoginRequest object
     * @return a response containing either the user's profile ID and session ID upon a successful login, or an
     * appropriate error code and status otherwise.
     */
    @PostMapping("/login")
    @ResponseBody
    public ResponseEntity<LoginResponse> loginUser(@RequestBody LoginRequest request) {

        List<Profile> result = eRepo.findByPrimaryEmail(request.getEmail());
        if (result.size() > 1) {
            return new ResponseEntity(null, HttpStatus.INTERNAL_SERVER_ERROR);
        } else if (result.isEmpty()) {
            return new ResponseEntity(null, HttpStatus.UNAUTHORIZED);
        }
        Profile profile = result.get(0);
        String hashedPassword = Profile_Controller.hashPassword(request.getPassword());
        if (!result.get(0).getPassword().equals(hashedPassword)) {
            return new ResponseEntity(null, HttpStatus.UNAUTHORIZED);
        }

        String token = jwtUtil.generateToken(profile);

        return ResponseEntity.ok(new LoginResponse(token, profile.getId()));
    }
}
