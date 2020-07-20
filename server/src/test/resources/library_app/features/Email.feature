Feature: Email addresses

  @U3-EmailAddresses
  Scenario: Adding an additional email address to profile
    Given I have registered an account with email "mando@starwars.com" and the password "mando'sSecuredPwd"
    When The following additional emails are added
      | mando@disney.com  |
    Then An account with email "mando@starwars.com" exists with the following additional emails
      | mando@disney.com  |

  @U3-EmailAddresses
  Scenario: Adding multiple additional email addresses to profile
    Given I have registered an account with email "yoda@starwars.com" and the password "yoda'sSecuredPwd"
    When The following additional emails are added
      | yoda@disney.com   |
      | yoda@gmail.com    |
      | yoda@outlook.com  |
    Then An account with email "yoda@starwars.com" exists with the following additional emails
      | yoda@disney.com   |
      | yoda@gmail.com    |
      | yoda@outlook.com  |

  @U3-EmailAddresses
  Scenario: Only changing primary email address of profile
    Given I have registered an account with email "rick@gmail.com" and the password "rick'sSecuredPwd"
    When I change the primary email "rick@gmail.com" to the new primary email "rick@yahoo.com"
    Then An account with the email "rick@yahoo.com" exists

  @U3-EmailAddresses
  Scenario: Adding an email to profile then setting it as primary such that the old primary email is now an additional email
    Given I have registered an account with email "morty@gmail.com" and the password "morty'sSecuredPwd"
    When I set the new primary email to "morty@yahoo.com" and set the old primary "morty@gmail.com" to an additional email
    Then An account with the email "morty@yahoo.com" exists with the additional email address "morty@gmail.com"