Feature: Getting notifications for activities

  @U16-Subscriptions
  Scenario: I want to delete an activity I created as the owner
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    When I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And An activity with the title "Rick goes to space" exists
    When I choose to delete the activity
    Then I get a notification saying that the activity has been deleted