package com.springvuegradle.repositories;

import com.springvuegradle.model.ActivityParticipation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ActivityParticipationRepository extends JpaRepository<ActivityParticipation, Long> {

    @Query("SELECT ap FROM ActivityParticipation ap WHERE ap.activity.id = :activityId")
    List<ActivityParticipation> findAllByActivityId(long activityId);
}
