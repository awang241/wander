Feature: Registering user profile and logging in

  @U1-authentication
  Scenario: Registering an account successfully
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I get count of accounts that have been registered
    Then exactly 1 account should be returned

  @U1-authentication
  Scenario: Registering an account with an already used email
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I receive an error saying that email address is already in use

  @U1-authentication
  Scenario: Logging in with a registered account
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I am successfully logged in

#  @U1-authentication
#  Scenario: Passwords are not stored in plain text
#    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
#    When I query the database for the password of the user account with email "jacky@google.com"
#    Then the password field is not equal to "jacky'sSecuredPwd"
#    And the password field is equal to the hash of "jacky'sSecuredPwd"