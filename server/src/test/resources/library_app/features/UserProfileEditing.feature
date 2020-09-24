Feature: Editing a user profile

  @U4-UserProfileEditing
  Scenario: Changing the bio on the user profile
    Given I have registered an account with email "homer@simpson.com" and the password "homer'sSecuredPwd" and a bio stating "I work at a power-plant."
    When I change the bio to "I have a crayon stuck up my nose."
    Then An account with email "homer@simpson.com" exists with the new bio "I have a crayon stuck up my nose."

  @U4-UserProfileEditing
  Scenario: Changing the gender on the user profile
    Given I have registered an account with email "marge@simpson.com" and the password "marge'sSecuredPwd" and the gender "female"
    When I change the gender to "non-Binary"
    Then An account with email "marge@simpson.com" exists with the gender "non-Binary"

  @U4-UserProfileEditing
  Scenario: Setting the nickname for a user to an empty string (non-mandatory field)
    Given I have registered an account with email "bart@simpson.com" and the password "3atmy5h0rt5" and nickname "Homer"
    When I remove the nickname by setting it to an empty string "" for the account with the primary email "bart@simpson.com"
    Then An account with the email "bart@simpson.com" exists with an empty string "" as the nickname

