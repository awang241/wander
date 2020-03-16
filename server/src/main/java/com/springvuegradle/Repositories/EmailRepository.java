package com.springvuegradle.Repositories;

import com.springvuegradle.Model.Email;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface EmailRepository extends JpaRepository<Email, Long> {

    List<Email> findAllByAddress(String address);

}
