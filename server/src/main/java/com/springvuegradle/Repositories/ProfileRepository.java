package com.springvuegradle.Repositories;


import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface ProfileRepository extends JpaRepository<Profile, Long> {

    //@Query("SELECT p FROM Profile p JOIN USER_EMAIL u ON p.id = u.profile_id JOIN email e on e.id = u.email_id where e.email = :email")
    //List<Profile> findByEmails(@Param("email") String email);

}
