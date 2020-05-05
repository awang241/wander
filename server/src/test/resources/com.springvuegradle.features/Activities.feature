Feature: Creating modifying and deleting activities

#  @U8-activities
#  Scenario: Adding an activity successfully
#    Given I register an activity with name "Kaikoura coastrack race"
#    And description "Epic cool race"
#    And activity types "['Hiking']"
#    And type continuous is "true"
#    And location "Kaikoura"
#    When I get count of activities that have been registered
#    Then exactly 1 activity should be returned

  @U8-activities
  Scenario: Adding an activity successfully
    Given I register a continuous activity with name "Kaikoura coastrack race"
    When I get count of activities that have been registered
    Then exactly 1 activity should be returned
