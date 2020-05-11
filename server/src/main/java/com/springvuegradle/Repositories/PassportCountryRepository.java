package com.springvuegradle.Repositories;


import com.springvuegradle.Model.PassportCountry;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface PassportCountryRepository extends JpaRepository<PassportCountry, Long> {

    @Query("SELECT pc FROM PassportCountry pc WHERE pc.countryName = :name")
    List<PassportCountry> findByCountryName(@Param("name") String name);
    List<PassportCountry> findByNumericCode(String code);
    boolean existsByCountryName(String name);
    boolean existsByNumericCode(String code);
}
