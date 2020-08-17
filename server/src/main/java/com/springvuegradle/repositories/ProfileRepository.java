package com.springvuegradle.repositories;


import com.springvuegradle.model.ActivityMembership;
import com.springvuegradle.model.Profile;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface ProfileRepository extends JpaRepository<Profile, Long>, JpaSpecificationExecutor<Profile> {

    @Query("SELECT max(id) from Profile")
    Long getLastInsertedId();

    @Query("SELECT p FROM Profile p JOIN Email e ON p.id = e.profile.id where p = e.profile AND e.address = :email AND e.isPrimary = true")
    List<Profile> findByPrimaryEmail(@Param("email") String email);

    @Query("SELECT p FROM Profile p JOIN Email e ON p.id = e.profile.id where p = e.profile AND e.address = :email")
    List<Profile> findByEmail(@Param("email") String email);

    @Query("SELECT p FROM Profile p WHERE p.authLevel = :auth_level")
    List<Profile> findByAuthLevel(@Param("auth_level") Integer authLevel);

    @Query("SELECT p FROM Profile p WHERE p.id = :id")
    List<Profile> findAllById(@Param("id") Long id);

    @Query("SELECT p FROM Profile p WHERE p.authLevel > :auth_level")
    List<Profile> findAllBelowAuthlevel(@Param("auth_level") Integer authLevel);

    @Query("SELECT p FROM Profile p JOIN ActivityMembership am ON p = am.profile WHERE am.activity.id = :id AND am.role = :role")
    Page<Profile> findByActivityAndRole(@Param("id") long activityId, @Param("role") ActivityMembership.Role role, Pageable pageable);

    @Query(value = "SELECT p FROM Profile p WHERE p.firstname = :firstname and " +
            "(p.middlename = :middlename or :middlename is null) and p.lastname = :lastname" )
    List<Profile> findAllByName(@Param("firstname") String firstname, @Param("middlename") String middlename,
                                @Param("lastname") String lastname, Pageable pageable);

    List<Profile> findAllByLastname(@Param("lastname") String lastName, Pageable pageable);

    List<Profile> findAllByNickname(@Param("nickname") String nickname, Pageable pageable);
}
