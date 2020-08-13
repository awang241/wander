package com.springvuegradle.repositories;

import com.springvuegradle.model.ActivityParticipation;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ActivityParticipationRepository extends JpaRepository<ActivityParticipation, Long> {
}
