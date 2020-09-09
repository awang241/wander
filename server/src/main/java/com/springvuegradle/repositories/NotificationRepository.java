package com.springvuegradle.repositories;


import com.springvuegradle.model.Notification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

@RepositoryRestResource
public interface NotificationRepository extends JpaRepository<Notification, Long> {
    @Query("SELECT max(id) from Notification")
    public Long getLastInsertedId();

}
