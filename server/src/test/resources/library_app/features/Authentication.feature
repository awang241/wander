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

  @U10-AC1
  Scenario: Accessing profiles while logged in
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    And I register an account with email "jom@bom.com" and password "jombomcom"
    And I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I load the profile with the email "jom@bom.com"
    Then I receive a "GET" response with the details of the profile with the email "jom@bom.com"

  @U10-AC1
  Scenario: Accessing profiles while not logged in
    Given I register an account with email "jom@bom.com" and password "jombomcom"
    When I load the profile with the email "jom@bom.com"
    Then I receive a "GET" response with the response code 401

  @U10-AC3
  Scenario: Editing profiles as the account owner
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    And The account with the email "jacky@google.com" does not have the nickname "Tim"
    And I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I change the nickname of the profile with email "jacky@google.com" to "Tim"
    Then The account with the email "jacky@google.com" has the nickname "Tim"

  @U10-AC3
  Scenario: Editing profiles as an admin
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    And The account with the email "jacky@google.com" does not have the nickname "Tim"
    And The sample admin exists
    And I log in as the sample admin
    When I change the nickname of the profile with email "jacky@google.com" to "Tim"
    Then The account with the email "jacky@google.com" has the nickname "Tim"

  @U10-AC3
  Scenario: Editing profiles as a normal account
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    And The account with the email "jacky@google.com" does not have the nickname "Tim"
    And I register an account with email "jom@bom.com" and password "jombomcom"
    And I log in using email "jom@bom.com" and password "jombomcom"
    When I change the nickname of the profile with email "jacky@google.com" to "Tim"
    Then The account with the email "jacky@google.com" does not have the nickname "Tim"

  @U10-AC3
  Scenario: Editing profiles while not logged in
    Given I register an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    And The account with the email "jacky@google.com" does not have the nickname "Tim"
    When I change the nickname of the profile with email "jacky@google.com" to "Tim"
    Then The account with the email "jacky@google.com" does not have the nickname "Tim"

