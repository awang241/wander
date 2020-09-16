package com.springvuegradle.repositories;

import com.springvuegradle.dto.responses.ActivityMemberProfileResponse;
import com.springvuegradle.model.ActivityMembership;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@RepositoryRestResource
public interface ActivityMembershipRepository extends JpaRepository<ActivityMembership, Long> {


    List<ActivityMembership> findActivityMembershipsByActivity_IdAndRole(Long id, ActivityMembership.Role role);
    List<ActivityMembership> findActivityMembershipsByActivity_Id(Long id);
    List<ActivityMembership> findActivityMembershipsByRole(ActivityMembership.Role role);
    Optional<ActivityMembership> findByActivity_IdAndProfile_Id(long activityId, long profileId);

    /**
     * Returns a page of memberships that match the given activity and role. The size and index of the page returned
     * are determined by the provided Pageable object.
     * @param id        the id of the activity that the memberships belong to
     * @param role      the role of
     * @param pageable  the Pageable containing parameters of the pagination
     * @return a page of memberships matching the given criteria
     */
    @Query("SELECT am FROM ActivityMembership am WHERE am.activity.id = :id AND am.role = :role")
    Page<ActivityMembership> findByActivityAndRole(Long id, ActivityMembership.Role role, Pageable pageable);

    @Query("SELECT new com.springvuegradle.dto.responses.ActivityMemberProfileResponse(p.id, p.firstname, p.lastname, e.address, am.role) FROM Profile p JOIN ActivityMembership am on am.profile.id = p.id JOIN Email e ON p.id = e.profile.id WHERE am.activity.id = :activityId AND e.isPrimary=true")
    List<ActivityMemberProfileResponse> findActivityMembershipsByActivityId(long activityId);


    @Transactional
    @Modifying
    @Query("delete from ActivityMembership am where am.activity.id = :activityId and am.profile.id = :profileId")
    int deleteActivityMembershipByProfileIdAndActivityId(long profileId, long activityId);

    @Query("SELECT am FROM ActivityMembership am LEFT JOIN FETCH Activity a ON am.activity = a WHERE am.profile.id = :profileId")
    Page<ActivityMembership> findAllByProfileId(@Param("profileId") Long profileId, Pageable pageable);
    @Query("SELECT am FROM ActivityMembership am LEFT JOIN FETCH Activity a ON am.activity = a WHERE am.profile.id = :profileId")
    List<ActivityMembership> findAllByProfileId(@Param("profileId") Long profileId);

}
