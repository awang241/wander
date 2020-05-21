package com.springvuegradle.Utilities;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


/***
 * Class holding business related logic for security. Thought it would be better separation of
 * concerns to keep the workings of the JWT class separate from business logic.
 */
@Service
public class SecurityUtil {

    @Autowired
    private JwtUtil jwtUtil;

    /**
     * Checks if the user has permission to edit the profile. User will be given permission if
     * they are either an admin or it is their own profile.
     * @param token the token contained in the authorization header
     * @param id the ID of the profile we are editing
     * @return true if the user can edit the profile, false otherwise
     */
    public boolean checkEditPermission(String token, Long id) {
        if (jwtUtil.validateToken(token) && (jwtUtil.extractPermission(token) == 0 || jwtUtil.extractPermission(token) == 1 || (jwtUtil.extractId(token).equals(id)))) {
            return true;
        } else {
            return false;
        }
    }
}
