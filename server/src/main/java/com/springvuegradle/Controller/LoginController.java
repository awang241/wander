package com.springvuegradle.Controller;

import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.dto.LoginRequest;
import com.springvuegradle.dto.LoginResponse;
import com.springvuegradle.dto.LogoutRequest;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Basic implementation of a login controller class.
 *
 * @author Alan Wang
 */
@RestController
public class LoginController {

    @Autowired
    private JwtUtil jwtUtil;

    @Autowired
    private ProfileRepository profileRepository;

    @Autowired
    private EmailRepository eRepo;
    private static Map<Long, Long> activeSessions = new HashMap<Long, Long>();
    private long sessionCounter;

    public LoginController() {
        sessionCounter = 0;
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
        } else if (result.size() == 0) {
            return new ResponseEntity(null, HttpStatus.UNAUTHORIZED);
        }
        Profile profile = result.get(0);
        String hashedPassword = Profile_Controller.hashPassword(request.getPassword());
        if (!result.get(0).getPassword().equals(hashedPassword)) {
            return new ResponseEntity(null, HttpStatus.UNAUTHORIZED);
        }

        String token = jwtUtil.generateToken(profile.getId());

        return ResponseEntity.ok(new LoginResponse(token, profile.getId()));
    }
}
