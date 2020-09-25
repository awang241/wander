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

  @U17F3-ShareActivitesWithOthers
  Scenario: As the creator of an activity, I want to share the activity with other users and make them followers.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create a continuous activity with the title "Airbending training session" and the location "Somewhere"
    And I create another account with email "katana@waterbender.com" and password "katana'sSecuredPwd"
    When I share the activity with email "katana@waterbender.com", and give them the role "follower".
    Then The activity now has one creator and one follower.

  @U17F3-ShareActivitesWithOthers
  Scenario: As the creator of an activity, I want to change the privacy level of my activity to friends.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create a continuous activity with the title "Airbending training session" and the location "Somewhere"
    When I change the privacy level to friends.
    Then The activity privacy level is now 1.

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

  @U17F5-GetActivitiesToDisplayOnActivitiesPage
  Scenario: As a logged in user, I want to discover new public activities that I can follow / participate in.
    Given I registered account with email "aang@airbender.com" and password "aang'sSecuredPwd"
    And I create the following activities, making them public
      | Hockey Game        |
      | Bowling            |
      | Fun Run            |
      | Triathlon          |
    And I create another account with email "katana@waterbender.com" and password "katana'sSecuredPwd"
    When I login with the email "katana@waterbender.com" and password "katana'sSecuredPwd"
    And I go to discover new public activities.
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
  Scenario: I am unable to change my own role in an activity to organiser
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
  Scenario: I am unable to change my own role in an activity to organiser
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And I create a continuous activity with the title "Rick goes to space" and the location "Space"
    And I am a "FOLLOWER" of this activity
    When I choose to change my role to "ORAGNISER"
    Then I am now a "FOLLOWER" of the activity

  @U17F2-Getting-Users-With-Roles
  Scenario: I want to find users with a role in an activity
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And an activity exists in the database with 2 participants, 3 followers and 1 organisers
    When I want to see who is following my activity
    Then The ID first name last name and role of All people with roles in the activity is returned

  @U17F4-Clearing-Memberships-Of-An-Activity
  Scenario: I am removing all the followers from my activity
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And an activity exists in the database with 2 participants, 3 followers and 1 organisers
    And I am the owner of the activity
    When I remove all "FOLLOWER"s from the activity
    Then The amount of "FOLLOWER"s of the activity is 0

  @U17F4-Clearing-Memberships-Of-An-Activity
  Scenario: I am removing all the participants from my activity
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And an activity exists in the database with 2 participants, 3 followers and 1 organisers
    And I am the owner of the activity
    When I remove all "PARTICIPANT"s from the activity
    Then The amount of "PARTICIPANT"s of the activity is 0

  @U17F4-Clearing-Memberships-Of-An-Activity
  Scenario: I am removing all the organisers from my activity
    Given I registered account with email "rick@gmail.com" and password "rick'sSecuredPwd"
    And an activity exists in the database with 2 participants, 3 followers and 1 organisers
    And I am the owner of the activity
    When I remove all "ORGANISER"s from the activity
    Then The amount of "ORGANISER"s of the activity is 0

  @U16-DeleteProfileWithNotifications
  Scenario: Deleting a profile that has associated activities, memberships, and notifications.
    Given I registered account with email "bart@simpson.com" and password "3atmy5h0rt5"
    And an activity exists in the database with 2 participants, 3 followers and 1 organisers
    When I delete the profile with the email "bart@simpson.com"
    Then The profile is deleted
    And The activity is deleted
    And The membership is deleted
    And No notifications are shared with the deleted user


