Feature: Registering user profile and logging in

  @U1-authentication
  Scenario: Registering an account successfully
    Given No account with email "jacky@google.com" exists
    When I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then An account with email "jacky@google.com" exists

  @U1-authentication
  Scenario: Registering an account with an already used email
    Given I have registered an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I receive a "TAKEN_EMAIL" email error message

  @U1-authentication
  Scenario: Logging in with a registered account
    Given I have registered an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I am successfully logged in

  @U1-authentication
  Scenario: Logging in with an unregistered account
    Given No account with email "jacky@google.com" exists
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I receive a login error message

  @U1-authentication
  Scenario: Passwords are not stored in plain text
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I query the database for the password of the user account with email "jacky@google.com"
    Then the password field is not equal to "jacky'sSecuredPwd"
    And the password field is equal to the hash of "jacky'sSecuredPwd"