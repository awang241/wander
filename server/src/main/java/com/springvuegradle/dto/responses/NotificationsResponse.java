package com.springvuegradle.dto.responses;

import com.springvuegradle.model.Notification;
import lombok.Data;

import java.util.List;

@Data
public class NotificationsResponse {

    String message = null;
    List<Notification> notifications = null;

    /**
     * Creates a NotificationsResponse object which contains a list of notifications
     * @param notifications the list of notifications
     */
    public NotificationsResponse(List<Notification> notifications) {
        this.message = null;
        this.notifications = notifications;
    }

    /**
     * Creates a NotificationsResponse object which contains an error message which is to be sent
     * to the front end
     * @param errorMessage details what error occurred
     */
    public NotificationsResponse(String errorMessage) {
        this.notifications = null;
        this.message = errorMessage;
    }
}
