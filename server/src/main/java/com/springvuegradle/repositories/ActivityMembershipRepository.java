package com.springvuegradle.repositories;

import com.springvuegradle.model.ActivityMembership;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;
import java.util.Optional;

@RepositoryRestResource
public interface ActivityMembershipRepository extends JpaRepository<ActivityMembership, Long> {
    List<ActivityMembership> findActivityMembershipsByActivity_IdAndRole(Long id, ActivityMembership.Role role);
    List<ActivityMembership> findActivityMembershipsByActivity_Id(Long id);
    Optional<ActivityMembership> findByActivity_IdAndProfile_Id(long activityId, long profileId);

    @Query("UPDATE ActivityMembership  am SET am.role = :role WHERE am.activity = :activityId AND am.profile = :profileId")
    void updateActivityRole(@Param("profileId") Long profileId, @Param("activityId") Long activityId, @Param("role") int role);
}
