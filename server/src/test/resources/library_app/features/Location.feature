Feature: CRUD for a profiles location

  @U9-location
  Scenario: Adding a location without a state
    Given A profile exists with no location
    When I select the country "New Zealand" and city "Wellington"
    Then The profile has the country "New Zealand" and city "Wellington"

  @U9-location
  Scenario: Adding a location with a state
    Given A profile exists with no location
    When I select the country "New Zealand" and city "Christchurch" and the state "Canterbury"
    Then The profile has the country "New Zealand" city "Christchurch" and state "Canterbury"

  @U9-location
  Scenario: Adding a location without city fails
    Given A profile exists with no location
    When I select the country "Australia" and the State "New South Wales"
    Then The profile location is not added to the database

  @U9-location
  Scenario: Changing the city and country of an existing location
    Given A profile exists with city "Wellington" and country "New Zealand"
    When I select "Chile" as country "Santiago" city
    Then The profile should now have "Chile" As country and "Santiago" as city

  @U9-location
  Scenario: Deleting a users location
    Given A profile exists with city "Wellington" and country "New Zealand"
    When I choose to delete the location from this profile
    Then the profile now has no location

  @U9-location
  Scenario: Adding a location with an address, latitude, and longitude values
    Given A profile exists with no location
    When I select the address "20 Kirkwood Ave, Upper Riccarton, Christchurch", longitude "1.1" and latitude "-1.1"
    Then the profile has the address "20 Kirkwood Ave, Upper Riccarton, Christchurch", longitude "1.1" and latitude "-1.1"