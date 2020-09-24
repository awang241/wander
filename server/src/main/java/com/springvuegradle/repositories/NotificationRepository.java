package com.springvuegradle.repositories;


import com.springvuegradle.model.Notification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.transaction.annotation.Transactional;

@RepositoryRestResource
public interface NotificationRepository extends JpaRepository<Notification, Long> {
    @Query("SELECT max(id) from Notification")
    public Long getLastInsertedId();

    @Transactional
    @Modifying
    @Query("update Notification n SET n.profile = null WHERE n.profile.id = :profileId")
    int detachProfileFromNotifications(long profileId);
}
