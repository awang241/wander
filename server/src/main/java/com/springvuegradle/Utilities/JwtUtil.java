package com.springvuegradle.Utilities;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;


//Class to handle JWT specific functionality
//Code based on example from spring security with JWT tutorial
//https://github.com/koushikkothagal/spring-security-jwt/blob/master/src/main/java/io/javabrains/springsecurityjwt/util/JwtUtil.java
@Service
public class JwtUtil {


    private String SECRET_KEY = "keyjklijaijsifjdksnfkdjakfjdkajhlhujdhsgjhfjghkjfhgjkhreuhrehwjkhjfkjhsjgkhfsjkhgkjfsdhgjfjkgshfdkhgfdjgkhs";

    public Long extractId(String token) {
        return Long.parseLong(extractClaim(token, Claims::getSubject));
    }

    public Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }
    private Claims extractAllClaims(String token) {
        return Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(token).getBody();
    }

    private Boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    /**
     * Takes the userdetails object from the user details service and creates a JWT from it
     * @param id the id associated with the user
     * @return a JWT generated from the users details
     */
    public String generateToken(Long id) {
        //Can pass in claims to be used in the JWT payload in this map
        Map<String, Object> claims = new HashMap<>();
        return createToken(claims, Long.toString(id));
    }

    /**
     * Calls JWT API to build a JWT from the person
     * @param claims a list of claims on the token
     * @param subject the user that is being authenticated
     * @return a string showing the JWT
     */
    private String createToken(Map<String, Object> claims, String subject) {

        return Jwts.builder().setClaims(claims).setSubject(subject).setIssuedAt(new Date(System.currentTimeMillis()))
                .setExpiration(new Date(System.currentTimeMillis() + 1000 * 60 * 60 * 10))
                .signWith(SignatureAlgorithm.HS256, SECRET_KEY).compact();
    }

    /**
     * Gets the username and checks whether the username is the same as the username in the user details
     * @param token
     * @param profileId The Id of the profile we are checking
     * @return Whether the token is valid
     */
    public Boolean validateToken(String token, Long profileId) {
        final Long tokenId = extractId(token);
        return (profileId.equals(tokenId) && !isTokenExpired(token));
    }
}