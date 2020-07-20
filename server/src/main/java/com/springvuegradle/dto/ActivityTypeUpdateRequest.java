package com.springvuegradle.dto;

import java.util.ArrayList;

public class ActivityTypeUpdateRequest {
    private ArrayList<String> activities;

    public ActivityTypeUpdateRequest(ArrayList<String> activities){
        this.activities = activities;
    }

    public ArrayList<String> getActivityTypes(){
        return this.activities;
    }
}
