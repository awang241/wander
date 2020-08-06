package com.springvuegradle.repositories;

import com.springvuegradle.dto.responses.ActivityMemberProfileResponse;
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

    @Query("SELECT new com.springvuegradle.dto.responses.ActivityMemberProfileResponse(p.id, p.firstname, p.lastname, am.role) FROM Profile p JOIN ActivityMembership am on am.profile.id = p.id AND am.activity.id = :activityId")
    List<ActivityMemberProfileResponse> findActivityMembershipsByActivityId(long activityId);

    Optional<ActivityMembership> findByActivity_IdAndProfile_Id(long activityId, long profileId);
}
