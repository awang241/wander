package com.springvuegradle.Repositories;

import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.ProfileLocation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.Optional;

@RepositoryRestResource
public interface ProfileLocationRepository extends JpaRepository<ProfileLocation, Long> {

    @Query("SELECT p FROM ProfileLocation p WHERE p.profile = :profile")
    Optional<ProfileLocation> findLocationByProfile(@Param("profile") Profile profile);

}

