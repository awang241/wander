package com.springvuegradle.Utilities;

import com.springvuegradle.Model.Profile;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;


//Class to handle JWT specific functionality
//Code based on example from spring security with JWT tutorial
//https://github.com/koushikkothagal/spring-security-jwt/blob/master/src/main/java/io/javabrains/springsecurityjwt/util/JwtUtil.java
@Service
public class JwtUtil {

    public static final String PERMISSION_KEY = "authLevel";

    SecretKey SECRET_KEY = Keys.secretKeyFor(SignatureAlgorithm.HS256);

    public Long extractId(String token) {
        return Long.parseLong(extractClaim(token, Claims::getSubject));
    }

    public Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    public int extractPermission(String token) {
        String permission = String.valueOf(extractAllClaims(token).get(PERMISSION_KEY));
        return Integer.parseInt(permission);
    }

    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }
    private Claims extractAllClaims(String token) {
        return Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(token).getBody();
    }

    public Boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    /**
     * Takes the userdetails object from the user details service and creates a JWT from it
     * @param profile the profile associated with the user
     * @return a JWT generated from the users details
     */
    public String generateToken(Profile profile) {
        Map<String, Object> claims = new HashMap<>();
        claims.put(PERMISSION_KEY, profile.getAuthLevel());
        return createToken(claims, Long.toString(profile.getId()));
    }

    /**
     * Calls JWT API to build a JWT from the person
     * @param claims a list of claims on the token
     * @param userId the user that is being authenticated
     * @return a string showing the JWT
     */
    private String createToken(Map<String, Object> claims, String userId) {

        return Jwts.builder().setClaims(claims).setSubject(userId).setIssuedAt(new Date(System.currentTimeMillis()))
                .setExpiration(new Date(System.currentTimeMillis() + 1000 * 60 * 60 * 10))
                .signWith(SignatureAlgorithm.HS256, SECRET_KEY).compact();
    }

    /**
     * Gets the username and checks whether the username is the same as the username in the user details
     * @param token the token we are validating
     * @return Whether the token is valid
     */
    public Boolean validateToken(String token) {
        try{
            Jwts.parserBuilder().setSigningKey(SECRET_KEY).build().parseClaimsJws(token);
            return true;
        } catch (JwtException e){
            return false;
        }
    }
}