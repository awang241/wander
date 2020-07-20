Feature: Editing a user profile

  @U4-Password
  Scenario: Changing the password on the user profile
    Given I have registered account with email "lisa@simpson.com" and password "lisa'sSecuredPwd"
    When I change the password from "lisa'sSecuredPwd" to "likestoplaysaxaphone"
    Then An account with email "lisa@simpson.com" exists
    And I can login with the email "lisa@simpson.com" and the password "likestoplaysaxaphone"