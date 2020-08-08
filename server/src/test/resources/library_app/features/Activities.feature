Feature: Creating modifying and deleting activities

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

  @U28F2-Add/Delete/ChangeActivityRoles
  Scenario: I want to add someone else as a follower of my activity.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create a continuous activity with the title "Airbending training session" and the location "Somewhere"
    And An activity with the title "Airbending training session" exists
    And I create another account with email "katana@waterbender.com" and password "katana'sSecuredPwd"
    When I choose to add the account with the email "katana@waterbender.com" to the activity as a "organiser"
    Then The activity has an organiser

  @U28F2-Add/Delete/ChangeActivityRoles
  Scenario: I want to add myself as a follower of an activity created by someone else.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create a continuous activity with the title "Airbending training session" and the location "Somewhere"
    And An activity with the title "Airbending training session" exists
    And I create another account with email "katana@waterbender.com" and password "katana'sSecuredPwd"
    And I login with the email "katana@waterbender.com" and password "katana'sSecuredPwd"
    When I choose to add the account with the email "katana@waterbender.com" to the activity as a "follower"
    Then The activity has a follower

  @U17F1-ChangePrivacyOfActivity
  Scenario: As the creator of an activity, I want to change the privacy of my activity.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create a continuous activity with the title "Airbending training session" and the location "Somewhere"
    When I change the visibility of my activity to "public" as the creator with email "aang@airbender.com"
    Then The activity is public

  @U17F1-GetActivitiesWithPrivacyLevel
  Scenario: As a logged in user, I want to get all activities with public privacy level.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create a continuous activity with the title "Airbending training session" and the location "Somewhere"
    When I change the visibility of my activity to "public" as the creator with email "aang@airbender.com"
    Then There is one activity with privacy "public"

  @U17F5-GetActivitiesToDisplayOnActivitiesPage
  Scenario: As a logged in user, I want view all the activities I am a creator or organiser of in a single display.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create the following activities, making them public
      | Hockey Game        |
      | Bowling            |
    And I create another account with email "katana@waterbender.com" and password "katana'sSecuredPwd"
    And I login with the email "katana@waterbender.com" and password "katana'sSecuredPwd"
    And I create the following activities, making them public and the account with email "aang@airbender.com" an organiser of each
      | Fun Run            |
      | Triathlon          |
    When I login with the email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I go to view the activities that I am a creator or organiser of.
    Then Four activities are returned.


  @U28F5-Editing-Participation
  Scenario: I want to change my role in an activity from follower to participant
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "FOLLOWER" of this activity
    When I choose to change my role to "PARTICIPANT"
    Then I am now a "PARTICIPANT" of the activity

  @U28F5-Editing-Participation
  Scenario: I am unable to change my own role in an activity from participant to creator
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "PARTICIPANT" of this activity
    When I choose to change my role to "CREATOR"
    Then I am now a "PARTICIPANT" of the activity

  @U28F5-Editing-Participation
  Scenario: I am unable to change my own role in an activity to organizer
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "FOLLOWER" of this activity
    When I choose to change my role to "ORAGNISER"
    Then I am now a "FOLLOWER" of the activity

  @U28F5-Editing-Participation
  Scenario: I want to change my role in an activity from follower to participant
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "FOLLOWER" of this activity
    When I choose to change my role to "PARTICIPANT"
    Then I am now a "PARTICIPANT" of the activity

  @U28F5-Editing-Participation
  Scenario: I am unable to change my own role in an activity from participant to creator
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "PARTICIPANT" of this activity
    When I choose to change my role to "CREATOR"
    Then I am now a "PARTICIPANT" of the activity

  @U28F5-Editing-Participation
  Scenario: I am unable to change my own role in an activity to organizer
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "FOLLOWER" of this activity
    When I choose to change my role to "ORAGNISER"
    Then I am now a "FOLLOWER" of the activity