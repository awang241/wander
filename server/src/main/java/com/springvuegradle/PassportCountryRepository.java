package com.springvuegradle;


import com.springvuegradle.Model.PassportCountry;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface PassportCountryRepository extends JpaRepository<PassportCountry, Long> {

    List<PassportCountry> findByCountryName(String name);

}
