package com.springvuegradle.Repositories;

import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface EmailRepository extends JpaRepository<Email, Long> {

    List<Email> findAllByAddress(String address);

    Boolean existsByAddress(String address);

    @Query("SELECT e.profile FROM Email e WHERE e.address = :email AND e.isPrimary = true")
    Profile findByPrimaryEmail(@Param("email") String email);

    @Query("SELECT e.profile FROM Email e WHERE e.address = :email")
    Profile findProfileByEmail(@Param("email") String email);
}
