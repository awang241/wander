package com.springvuegradle.Repositories;


import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface ProfileRepository extends JpaRepository<Profile, Long> {

    List<Profile> findByEmail(String email);

}
