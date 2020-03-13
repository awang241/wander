package com.springvuegradle.Model;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.HashSet;
import java.util.Set;

@Entity
public class UserEmail {

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    @Id
    @GeneratedValue
    private long id;

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    @Column
    @NotNull
    private String email;

    @ManyToOne
    @JoinColumn(name="profile_id", nullable=false)
    private Profile profile;

    public UserEmail(String email) {
        this.email = email;
    }

    @JsonCreator
    public UserEmail(@JsonProperty("email") String email, @JsonProperty("profileId") Profile profile){
        this.email = email;
        this.profile = profile;
    }
}
