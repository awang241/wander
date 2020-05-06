Feature: Creating modifying and deleting activities

  @U8-activities
  Scenario: Adding an activity successfully
    Given I register a continuous activity with name "Kaikoura coastrack race"
    When I get count of activities that have been registered
    Then exactly 1 activity should be returned
