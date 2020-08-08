package com.springvuegradle.repositories;

import com.springvuegradle.dto.responses.ActivityMemberProfileResponse;
import com.springvuegradle.model.ActivityMembership;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
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

    @Query("SELECT new com.springvuegradle.dto.responses.ActivityMemberProfileResponse(p.id, p.firstname, p.lastname, am.role) FROM Profile p JOIN ActivityMembership am on am.profile.id = p.id AND am.activity.id = :activityId")
    List<ActivityMemberProfileResponse> findActivityMembershipsByActivityId(long activityId);

    @Query("SELECT am FROM ActivityMembership am LEFT JOIN FETCH Activity a ON am.activity = a WHERE am.profile.id = :profileId")
    Page<ActivityMembership> findAllByProfileId(@Param("profileId") Long profileId, Pageable pageable);
}
