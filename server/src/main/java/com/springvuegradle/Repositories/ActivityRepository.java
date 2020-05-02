package com.springvuegradle.Repositories;

import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface ActivityRepository extends JpaRepository<Activity, Long> {

    @Query("SELECT a FROM Activity a WHERE a.activityName = :activityName")
    List<Activity> findByActivityNames(@Param("activityName") String activityName);

    @Query("SELECT max(id) from Activity")
    public Long getLastInsertedId();

    @Query("SELECT a.activityName FROM Activity a")
    List<String> findAllActivityNames();
}
