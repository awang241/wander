package com.springvuegradle.repositories;

import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@RepositoryRestResource
public interface ActivityRepository extends JpaRepository<Activity, Long> {

    @Query("SELECT a FROM Activity a WHERE a.activityName = :activityName")
    List<Activity> findByActivityNames(@Param("activityName") String activityName);

    @Query("SELECT max(id) from Activity")
    public Long getLastInsertedId();

    @Query("SELECT a.activityName FROM Activity a")
    List<String> findAllActivityNames();

    @Query("SELECT a FROM Activity a WHERE a.privacyLevel = :privacyLevel")
    List<Activity> findAllByPrivacyLevel(@Param("privacyLevel") int privacyLevel);

    @Query("SELECT a FROM Activity a WHERE a.privacyLevel = :privacyLevel")
    Page<Activity> findAllByPrivacyLevelWithPagination(@Param("privacyLevel") int privacyLevel, Pageable pageable);

    @Query("SELECT distinct a FROM Activity a " +
            "Join ActivityMembership am on a = am.activity " +
            "WHERE (am.profile.id = :profileId AND a.privacyLevel = 1)" +
            "OR(a.privacyLevel = 2)" +
            "OR(am.profile.id = :profileId AND am.role = :creator)"
    )
    List<Activity> findActivitiesUserCanSee(@Param("profileId") Long profileId, @Param("creator") ActivityMembership.Role creator);

    boolean existsById(Long id);
}
