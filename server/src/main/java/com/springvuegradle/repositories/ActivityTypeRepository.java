package com.springvuegradle.repositories;

import com.springvuegradle.model.ActivityType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface ActivityTypeRepository extends JpaRepository<ActivityType, Long> {

    @Query("SELECT a FROM ActivityType a WHERE a.activityTypeName = :activityTypeName")
    List<ActivityType> findByActivityTypeName(@Param("activityTypeName") String activityTypeName);

    boolean existsByActivityTypeName(String name);

    ActivityType getByActivityTypeName(String name);

    @Query("SELECT a.activityTypeName FROM ActivityType a")
    List<String> findAllActivityTypeNames();
}
