Feature: Creating modifying and deleting activities

  #@U8-activities
  #Scenario: Adding an activity successfully
  #  Given I register a user
  #  When I register an activity with name "Kaikoura Coast Track Race" and description "A big and nice race on a lovely peninsula"
  #  Then I check "Kaikoura Coast Track Race" exists in list of activities that have been registered

  @U28-DeletingActivity
  Scenario: I want to delete an activity I created as the owner
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    When I create a continuous activity with the title "Rick goes to space" and the location "Space"
    Then An activity with the title "Rick goes to space" exists
    When I choose to delete the activity
    Then The activity no longer exists

  @U28-DeletingActivity
  Scenario: I want to delete an activity someone else created
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    When I create a continuous activity with the title "Rick goes to space" and the location "Space"
    Then An activity with the title "Rick goes to space" exists
    Then I register with email "morty@gmail.com" and password "morty'sSecuredPwd" and login
    When I choose to delete the activity
    Then The activity is not deleted