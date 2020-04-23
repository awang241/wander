package com.springvuegradle.Services;

import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;

@Service
public class MyUserDetailsService implements UserDetailsService {
    @Override
    //Expects this method to load user from wherever you've got your user saved.
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        //Default constructor has the username and the password and a collection of authorities
        //TODO change this to use our actual user
        return new User("username", "password", new ArrayList<>());
    }
}
