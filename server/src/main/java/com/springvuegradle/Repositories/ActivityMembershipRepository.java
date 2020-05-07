package com.springvuegradle.Repositories;

import com.springvuegradle.Model.Activity;
import com.springvuegradle.Model.ActivityMembership;
import com.springvuegradle.Model.ActivityType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface ActivityMembershipRepository extends JpaRepository<ActivityMembership, Long> {

//    @Modifying
////    @Query("delete from ActivityMembership b where b.title=:title")
////    void deleteBooks(@Param("title") String title);
}
