package com.springvuegradle.Repositories;

import com.springvuegradle.Model.Activity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

@RepositoryRestResource
public interface ActivityRepository extends JpaRepository<Activity, Long> {

    @Query("SELECT max(id) from Activity")
    public Long getLastInsertedId();
}
