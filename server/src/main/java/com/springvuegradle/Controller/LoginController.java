package com.springvuegradle.Controller;

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

import java.util.*;

/**
 * Basic implementation of a login controller class.
 * @author Alan Wang
 *
 */
@RestController
public class LoginController {

    @Autowired
    private ProfileRepository repo;

    @Autowired
    private EmailRepository eRepo;

    /**
     * Key: Profile ID
     * Value: Session ID
     */
    private static Map<Long, Long> activeSessions = new HashMap<Long, Long>();

    /**
     * Key: Session ID
     * Value: Profile ID
     */
    private static Map<Long, Long> activeSessionsInverse = new HashMap<Long, Long>();

    private long sessionCounter;

    public LoginController() {
        sessionCounter = 0;
    }

    /**
     * Attempts to log in a user given a login request. If the credentials are correct, the user is logged
     * in and the session is recorded; otherwise, returns an error code.
     * @param request the user's email and password mapped from the request body onto a LoginRequest object
     * @return a response containing either the user's profile ID and session ID upon a successful login, or an
     * appropriate error code and status otherwise.
     */
    @PostMapping("/login")
    @ResponseBody
    public ResponseEntity<LoginResponse> loginUser(@RequestBody LoginRequest request) {
        LoginResponse body = null;
        HttpStatus status = null;
        List<Profile> result = eRepo.findByPrimaryEmail(request.getEmail());
        System.out.println(repo.count());
        if (result.size() > 1) {
            status = HttpStatus.INTERNAL_SERVER_ERROR;
        } else if (result.size() == 0) {
            status = HttpStatus.UNAUTHORIZED;
        } else {
            Profile profile = result.get(0);
            String hashedPassword = Profile_Controller.hashPassword(request.getPassword());
            if (!result.get(0).getPassword().equals(hashedPassword)) {
                status = HttpStatus.UNAUTHORIZED;}
            else if (activeSessions.containsKey(profile.getId())) {
                status = HttpStatus.OK;
                long tempSessionID = activeSessions.get(profile.getId());
                activeSessions.remove(result.get(0).getId());
                activeSessionsInverse.remove(tempSessionID);
                body = new LoginResponse(++sessionCounter, result.get(0).getId());
                activeSessions.put(profile.getId(), sessionCounter);
                activeSessionsInverse.put(sessionCounter, profile.getId());
            } else {
                body = new LoginResponse(++sessionCounter, result.get(0).getId());
                status = HttpStatus.OK;
                activeSessions.put(profile.getId(), sessionCounter);
                activeSessionsInverse.put(sessionCounter, profile.getId());
            }
        }
        return new ResponseEntity<>(body, status);
    }

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
        if (checkCredentials(userId.getUserId(), sessionID, repo)){
            message = "Logout successful.";
            status = HttpStatus.OK;
            activeSessions.remove(userId.getUserId());
            activeSessionsInverse.remove(sessionID);
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
     * @return true if the session ID matches the user ID or user with higher auth level accessing details of user
     * with lower auth level; false otherwise.
     */
    public boolean checkCredentials(long userID, long sessionID, ProfileRepository repo) {
        System.out.println("Printed from checkCredentials: " + repo.findById((long)7008));

        int authLevelFromSessionID = getAuthLevelFromSessionID(sessionID, repo);
        if (authLevelFromSessionID == -1) {
            return false;
        }

        int authLevelFromUserID = getAuthLevelFromId(userID, repo);
        if (authLevelFromUserID == -1) {
            return false;
        }

        return activeSessionsInverse.get(sessionID) == userID || authLevelFromSessionID > authLevelFromUserID;
    }

//    /**
//     *  Given a request's user ID and session token, checks for a match with an existing session.
//     * @param userID the user ID
//     * @param sessionToken the session token pulled from a HTTP request header to be validated
//     * @return true if the session ID matches the user ID; false otherwise.
//     */
//    public boolean checkCredentials(long userID, String sessionToken){
//        long sessionID = retrieveSessionID(sessionToken);
//        if (activeSessions.containsKey(userID)) {
//            return sessionID == activeSessions.get(userID);
//        } else {
//            return false;
//        }
//    }

    /**
     * Given a session ID, fetches the associated ID then gets the profile from the database. Then returns the
     * authorisation level associated with that profile.
     * @param sessionID the session ID
     * @return the authentication level associated with the profile associated with the given session id, else if the
     * session ID is invalid, return -1.
     */
    public int getAuthLevelFromSessionID(long sessionID, ProfileRepository repo) {
        if (! activeSessionsInverse.containsKey(sessionID)) {
            return -1;
        }
        Long profileID = activeSessionsInverse.get(sessionID);
        Profile profile = repo.findById((long)profileID).get();
        return profile.getAuthLevel();
    }

    /**
     * Given a user ID, fetches the associated profile from the database. Then returns the
     * authorisation level associated with that profile.
     * @param userID the user ID
     * @return the authentication level associated with the profile, else if the
     * user ID is invalid, return -1.
     */
    public int getAuthLevelFromId(long userID, ProfileRepository repo) {
        Optional<Profile> optionalProfile = repo.findById(userID);
        if (optionalProfile.isEmpty()) {
            return -1;
        }
        Profile profile = optionalProfile.get();
        return profile.getAuthLevel();
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
