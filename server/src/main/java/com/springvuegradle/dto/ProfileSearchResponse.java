package com.springvuegradle.dto;

import java.util.List;
import java.util.Objects;

public class ProfileSearchResponse {

    String message = null;
    List<ProfileSummary> results = null;

    public ProfileSearchResponse(List<ProfileSummary> results) {
        this.results = results;
        this.message = null;
    }

    public ProfileSearchResponse(String errorMessage) {
        this.results = null;
        this.message = errorMessage;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public List<ProfileSummary> getResults() {
        return results;
    }

    public void setResults(List<ProfileSummary> results) {
        this.results = results;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ProfileSearchResponse that = (ProfileSearchResponse) o;
        return Objects.equals(message, that.message) &&
                Objects.equals(results, that.results);
    }

    @Override
    public int hashCode() {
        return Objects.hash(message, results);
    }
}
