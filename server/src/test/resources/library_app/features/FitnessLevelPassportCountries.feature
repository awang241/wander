Feature: Fitness level and passport countries

  @U2-FitnessLevelPassportCountries
  Scenario: Adding a fitness level on registration
    When I register an account with email "jacky246@google.com" and password "jacky'sSecuredPwd" and fitness level 4
    Then An account with email "jacky246@google.com" exists with fitness level 4

  @U2-FitnessLevelPassportCountries
  Scenario: Adding a single passport country on registration
    When I register an account with email "jacky123@google.com" and password "jacky'sSecuredPwd" and fitness level 4 and the following passport countries are added
      | name              |
      | New Zealand       |
    Then An account with email "jacky123@google.com" exists with fitness level 4 and the following passport countries
      | name              |
      | New Zealand       |

  @U2-FitnessLevelPassportCountries
  Scenario: Adding multiple passport countries on registration
    When I register an account with email "jacky321@google.com" and password "jacky'sSecuredPwd" and fitness level 4 and the following passport countries are added
      | name              |
      | New Zealand       |
      | India             |
      | China             |
    Then An account with email "jacky321@google.com" exists with fitness level 4 and the following passport countries
      | name              |
      | New Zealand       |
      | India             |
      | China             |