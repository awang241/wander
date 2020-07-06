Feature: Creating modifying and deleting activities

  @U8-activities
  Scenario: Adding an activity successfully
    Given I register a user
    When I register an activity with name "Kaikoura Coast Track Race" and description "A big and nice race on a lovely peninsula"
    Then I check "Kaikoura Coast Track Race" exists in list of activities that have been registered 
