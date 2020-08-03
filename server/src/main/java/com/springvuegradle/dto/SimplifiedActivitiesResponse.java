package com.springvuegradle.dto;

import java.util.List;
import java.util.Objects;

public class SimplifiedActivitiesResponse {

    private String message;
    private List<SimplifiedActivity> results;

    public SimplifiedActivitiesResponse(List<SimplifiedActivity> results) {
        this.results = results;
        this.message = null;
    }

    public SimplifiedActivitiesResponse(String errorMessage) {
        this.results = null;
        this.message = errorMessage;
    }

    public String getMessage() {
        return this.message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public List<SimplifiedActivity> getResults() {
        return results;
    }

    public void setResults(List<SimplifiedActivity> results) {
        this.results = results;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SimplifiedActivitiesResponse that = (SimplifiedActivitiesResponse) o;
        return Objects.equals(message, that.message) &&
                Objects.equals(results, that.results);
    }

    @Override
    public int hashCode() {
        return Objects.hash(message, results);
    }

}
