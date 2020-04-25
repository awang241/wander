package com.springvuegradle.Controller;

import com.springvuegradle.Services.MyUserDetailsService;
import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.dto.LoginRequest;
import com.springvuegradle.dto.LoginResponse;
import com.springvuegradle.dto.LogoutRequest;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

/**
 * Basic implementation of a login controller class.
 * @author Alan Wang
 *
 */
@RestController
public class LoginController {

    @Autowired
    private AuthenticationManager authenticationManager;

    @Autowired
    private MyUserDetailsService userDetailsService;

    @Autowired
    private ProfileRepository profileRepository;

    @Autowired
    private JwtUtil jwtTokenUtil;

    @Autowired
    private EmailRepository eRepo;
    private static Map<Long, Long> activeSessions = new HashMap<Long, Long>();
    private long sessionCounter;

    public LoginController() {
        sessionCounter = 0;
    }

    @GetMapping("/hello")
    public String hello(){
        return "Hello world";
    }

    @PostMapping("/authenticate")
    public ResponseEntity<?> createAuthenticationToken(@RequestBody LoginRequest loginRequest){
        try {
            authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(loginRequest.getEmail(), loginRequest.getPassword())
            );
        } catch (BadCredentialsException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        final UserDetails userDetails = userDetailsService.loadUserByUsername(loginRequest.getEmail());
        final String jwt = jwtTokenUtil.generateToken(userDetails);

        long id = 3;
        return ResponseEntity.ok(new LoginResponse(jwt, id));
    }

    /**
     * Attempts to log in a user given a login request. If the credentials are correct, the user is logged
     * in and the session is recorded; otherwise, returns an error code.
     * @param request the user's email and password mapped from the request body onto a LoginRequest object
     * @return a response containing either the user's profile ID and session ID upon a successful login, or an
     * appropriate error code and status otherwise.
     */
//    @PostMapping("/login")
//    @ResponseBody
//    public ResponseEntity<LoginResponse> loginUser(@RequestBody LoginRequest request) {
//        LoginResponse body = null;
//        HttpStatus status = null;
//        List<Profile> result = eRepo.findByPrimaryEmail(request.getEmail());
//        if (result.size() > 1) {
//            status = HttpStatus.INTERNAL_SERVER_ERROR;
//        } else if (result.size() == 0) {
//            status = HttpStatus.UNAUTHORIZED;
//        } else {
//            Profile profile = result.get(0);
//            String hashedPassword = Profile_Controller.hashPassword(request.getPassword());
//            if (!result.get(0).getPassword().equals(hashedPassword)) {
//                status = HttpStatus.UNAUTHORIZED;}
//            else if (activeSessions.containsKey(profile.getId())) {
//                status = HttpStatus.OK;
//                activeSessions.remove(result.get(0).getId());
//                body = new LoginResponse(++sessionCounter, result.get(0).getId());
//                activeSessions.put(profile.getId(), sessionCounter);
//            } else {
//                body = new LoginResponse(++sessionCounter, result.get(0).getId());
//                status = HttpStatus.OK;
//                activeSessions.put(profile.getId(), sessionCounter);
//            }
//        }
//        return new ResponseEntity<>(body, status);
//    }

    /**
     * Attempts to log out the user given a HTTP logout request. Only succeeds if the user's credentials are correct.
     * @param userId the user's profile ID from the request body.
     * @param field the Authorization field in the request header.
     * @return An HTTP response with the appropriate message and HTTP code depending on the logout success
     */
    @PostMapping("/logout")
    @ResponseBody
    public ResponseEntity<String> logoutUser(@RequestBody LogoutRequest userId, @RequestHeader("authorization") String field){
        String message = null;
        HttpStatus status = null;
        Long sessionID = Long.parseLong(field.split(" ")[0]);
        if (checkCredentials(userId.getUserId(), sessionID)){
            message = "Logout successful.";
            status = HttpStatus.OK;
            activeSessions.remove(userId.getUserId());
        } else {
            message = "Invalid session key pair.";
            status = HttpStatus.UNAUTHORIZED;
        }

        return new ResponseEntity<>(message, status);
    }

    /**
     *  Given a request's user ID and session ID, checks for a match with an existing session.
     * @param userID the user ID
     * @param sessionID the session ID to be validated
     * @return true if the session ID matches the user ID; false otherwise.
     */
    public boolean checkCredentials(long userID, long sessionID){
        if (activeSessions.containsKey(userID)) {
            return sessionID == activeSessions.get(userID);
        } else {
            return false;
        }
    }

    /**
     *  Given a request's user ID and session token, checks for a match with an existing session.
     * @param userID the user ID
     * @param sessionToken the session token pulled from a HTTP request header to be validated
     * @return true if the session ID matches the user ID; false otherwise.
     */
    public boolean checkCredentials(long userID, String sessionToken){
        long sessionID = retrieveSessionID(sessionToken);
        if (activeSessions.containsKey(userID)) {
            return sessionID == activeSessions.get(userID);
        } else {
            return false;
        }
    }

    /**
     * Strips extra information out of the authorization header to get just the session ID
     * @param rawAuthorizationField the authorization field string
     * @return Long of the session ID from the auth header
     */
    public static long retrieveSessionID(String rawAuthorizationField) {
        return Long.parseLong(rawAuthorizationField.substring(6).strip());
    }

    /**
     * returns the current session counter of the controller
     * @return the current session counter value
     */
    public long getSessionCounter() {
        return sessionCounter;
    }
}
