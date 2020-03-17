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

    @Query("SELECT p FROM Profile p JOIN Email e ON p.id = e.profile.id where p = e.profile AND e.address = :email AND e.isPrimary = true")
    List<Profile> findByPrimaryEmail(@Param("email") String email);

}
