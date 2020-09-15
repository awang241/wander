package com.springvuegradle.utilities;

import com.springvuegradle.dto.ActivityParticipationRequest;
import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityParticipation;

import java.util.ArrayList;
import java.util.List;

public class ActivityTestUtils {

    /**
     * Creates a new activity.
     * @return a new activity.
     */
    public static Activity createNormalActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Kaikoura, NZ", 100, 100);
    }

    public static Activity createDifferentLocationActivity() {
        return new Activity("Kaikoura Coast Track race", "A big and nice race on a lovely peninsula",
                new String[]{"Hiking"}, false, "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300", "Dunedin, NZ", 50, 200);
    }

    public static ActivityParticipation createNormalParticipation() {
        return new ActivityParticipation("Played League as a noob", "Got rekt", "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300");
    }

    public static ActivityParticipation createEditedNormalParticipation() {
        return new ActivityParticipation("Played League as a pro", "Won the game", "2020-02-20T12:00:00+1300", "2020-02-20T16:00:00+1300");
    }

    public static ActivityParticipation createADifferentParticipation() {
        return new ActivityParticipation("Scored last minute winner", "Won Champions League :)", "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300");
    }

    public static ActivityParticipationRequest createNormalParticipationRequest() {
        return new ActivityParticipationRequest("Played League as a noob", "Got rekt", "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300");
    }

    public static ActivityParticipationRequest createADifferentParticipationRequest() {
        return new ActivityParticipationRequest("Scored last minute winner", "Won Champions League :)", "2020-02-20T08:00:00+1300", "2020-02-20T08:00:00+1300");
    }

    public static List<ActivityParticipation> createValidActivityParticipationsList() {
        ActivityParticipation normalActivityParticpation = createNormalParticipation();
        ActivityParticipation normalActivityParticpation1 = createADifferentParticipation();
        List<ActivityParticipation> activityParticipations = new ArrayList<>();
        activityParticipations.add(normalActivityParticpation);
        activityParticipations.add(normalActivityParticpation1);
        return activityParticipations;
    }
}
