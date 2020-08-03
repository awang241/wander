Feature: Creating modifying and deleting activities

  #@U8-activities
  #Scenario: Adding an activity successfully
  #  Given I register a user
  #  When I register an activity with name "Kaikoura Coast Track Race" and description "A big and nice race on a lovely peninsula"
  #  Then I check "Kaikoura Coast Track Race" exists in list of activities that have been registered

  @U28F1-Deleting/EditingActivities
  Scenario: I want to delete an activity I created as the owner
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    When I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And An activity with the title "Rick goes to space" exists
    When I choose to delete the activity
    Then The activity no longer exists

  @U28F1-Deleting/EditingActivities
  Scenario: I want to delete an activity someone else created
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And An activity with the title "Rick goes to space" exists
    And I register with email "morty@gmail.com" and password "morty'sSecuredPwd" and login
    When I choose to delete the activity
    Then The activity is not deleted

  @U28F1-Deleting/EditingActivities
  Scenario: I want to edit an activity someone else created
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And An activity with the title "Rick goes to space" exists
    And I register with email "morty@gmail.com" and password "morty'sSecuredPwd" and login
    When I choose to edit the activity by changing the title to "Morty goes to space"
    Then The activity is not edited

  @U28F1-Deleting/EditingActivities
  Scenario: I want to edit an activity I created as the owner
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And An activity with the title "Rick goes to space" exists
    When I choose to edit the activity by changing the title to "Summer goes to space"
    Then The activity was edited

  @U28F5-Editing-Participation
  Scenario: I want to change my role in an activity
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "FOLLOWER" of this activity
    When I choose to change my role to "follower"
    Then I am now a follower of the activity