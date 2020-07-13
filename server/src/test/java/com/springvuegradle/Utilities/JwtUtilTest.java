package com.springvuegradle.Utilities;

import com.springvuegradle.Model.Profile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Calendar;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
public class JwtUtilTest {

    @Autowired
    private JwtUtil jwtUtil;

    /**
     * Test to check if a token is in correct jwt format
     */
    @Test
    void checkValidJwt(){
        String token = jwtUtil.generateToken(createNormalProfileMaurice());
        assertTrue(jwtUtil.validateToken(token));
    }

    /**
     * Test to check if a token is in incorrect jwt format
     */
    @Test
    void checkInvalidJwt(){
        String token = "thisisnotavalidjwt";
        assertFalse(jwtUtil.validateToken(token));
    }

    /**
     * Test to get id from a jwt token
     */
    @Test
    void getIdFromToken(){
        Profile profile = createNormalProfileMaurice();
        Long id = profile.getId();
        String token = jwtUtil.generateToken(profile);
        assertEquals(id, jwtUtil.extractId(token));
    }

    /**
     * Test to get permission level from a jwt token
     */
    @Test
    void getPermissionLevelFromToken(){
        Profile profile = createNormalProfileMaurice();
        int authLevel = profile.getAuthLevel();
        String token = jwtUtil.generateToken(profile);
        assertEquals(authLevel, jwtUtil.extractPermission(token));
    }

    /**
     * @return a valid profile object.
     */
    static Profile createNormalProfileMaurice() {
        Profile profile = new Profile(null, "Maurice", "Benson", "Jack", "Jacky", "jacky@google.com", new String[]{"additionaldoda@email.com"}, "jacky'sSecuredPwd",
                "Jacky loves to ride his bike on crazy mountains.", new GregorianCalendar(1985, Calendar.DECEMBER,
                20), "male", 1, new String[]{"New Zealand", "India"}, new String[]{});

        profile.setId(1L);
        return profile;
    }
}
