Feature: Search for a user by name or email address

  @U11-SearchUserByNameOrEmail
  Scenario: Searching a user by surname only
    Given I have registered an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I am successfully logged in
    When I search by the surname "Tester"
    Then the search result will return the following users
      |first name | surname |
      | Steve    | Tester  |
      | Dave     | Tester  |

  @U11-SearchUserByNameOrEmail
  Scenario: Searching a user by full name only
    Given I have registered an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I am successfully logged in
    When I search by the full name "Steve Tester"
    Then the search result will return the following user
      |first name | surname |
      | Steve     | Tester  |

  @U11-SearchUserByNameOrEmail
  Scenario: Searching a user by email address
    Given I have registered an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I am successfully logged in
    When I search by the email address "steve@google.com"
    Then the search result will return the user with the email address "steve@google.com"

  @U11-SearchUserByNameOrEmail
  Scenario: Searching a user by surname and email address
    Given I have registered an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I am successfully logged in
    When I search by surname "Tester" and the email address "steve@google.com"
    Then the search result will return the profile with surname "Tester"
    And the email address "steve@google.com"

  @U11-SearchUserByNameOrEmail
  Scenario: Searching a user by full name and email address
    Given I have registered an account with email "jacky@google.com" and password "jacky'sSecuredPwd"
    When I log in using email "jacky@google.com" and password "jacky'sSecuredPwd"
    Then I am successfully logged in
    When I search by surname "Steve Tester" and the email address "steve@google.com"
    Then the search result will return the profile with first name "Steve"
    And the surname "Tester"
    And the email address "steve@google.com"


