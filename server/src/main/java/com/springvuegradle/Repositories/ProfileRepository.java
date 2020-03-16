package com.springvuegradle.Repositories;


import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import javax.persistence.SqlResultSetMapping;
import java.util.List;

@RepositoryRestResource
public interface ProfileRepository extends JpaRepository<Profile, Long> {

    //@Query("SELECT p FROM Profile p JOIN p.Email e WHERE e.address=:address")
    //List<Profile> findByEmail(@Param("address") String email);

}
