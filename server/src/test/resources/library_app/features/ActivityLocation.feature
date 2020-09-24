Feature: Adding and editing an activities location

  @U39F1-Deleting/EditingActivities
  Scenario: I want to create an activity with a specific location
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    When I create a continuous activity with location "Wellington" and the latitude 42.4 and the longitude 17.3
    Then The activities location and latitude and longitude will be stored

  @U39F1-Deleting/EditingActivities
  Scenario: I want to edit the location of an activity I created as the owner
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And An activity with the title "Rick goes to space" exists
    When I choose to edit the activity by changing the location to "Sydney, Australia" and its latitude to 39.3 and longitude to 69.2
    Then The activity was edited