package com.springvuegradle.utilities;

import com.springvuegradle.model.Profile;
import com.springvuegradle.model.ProfileLocation;

public class ProfileLocationTestUtils {

    public static ProfileLocation createValidProfileLocation() {
        return new ProfileLocation("Christchurch", "Canterbury", "New Zealand", 100, 100);
    }

    public static ProfileLocation createUpdatedProfileLocation() {
        return new ProfileLocation("Auckland", "Auckland", "New Zealand", 200, 200);
    }
}