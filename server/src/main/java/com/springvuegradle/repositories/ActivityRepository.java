package com.springvuegradle.repositories;

import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface ActivityRepository extends JpaRepository<Activity, Long>, JpaSpecificationExecutor<Activity> {

    @Query("SELECT a FROM Activity a WHERE a.activityName = :activityName")
    List<Activity> findByActivityNames(@Param("activityName") String activityName);

    @Query("SELECT max(id) from Activity")
    Long getLastInsertedId();

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

    @Query("SELECT a FROM Activity a WHERE (a.longitude BETWEEN :x_left AND :x_right) AND (a.latitude BETWEEN :y_bottom AND :y_top)")
    List<Activity> findAllInRange(@Param("x_left") double x_left, @Param("x_right") double x_right,
                                  @Param("y_bottom") double y_bottom, @Param("y_top") double y_top);

    boolean existsById(Long id);
}
