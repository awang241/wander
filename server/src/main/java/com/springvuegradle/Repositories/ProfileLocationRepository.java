package com.springvuegradle.Repositories;

import com.springvuegradle.Model.ProfileLocation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

@RepositoryRestResource
public interface ProfileLocationRepository extends JpaRepository<ProfileLocation, Long> {
}
