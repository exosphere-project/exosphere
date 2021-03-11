Feature: Text presence

    @setup
    Scenario: Adding a Jetstream cloud account
        Given a browser
        When I go to Exosphere
        Then I should see "Choose a login method" within 60 seconds
        When I click the "Add Jetstream Account" button
        Then I should see "Add a Jetstream Cloud Account" within 15 seconds
        When I enter TACC credentials
        And I click the "IU Cloud" radio button
        And I click the "Log In" button
        Then I should see "Choose Projects for" within 15 seconds
        And I should see "TG-CCR190024"
        When I click the "TG-CCR190024" checkbox
        And I click the "Choose" button
        Then I wait for 2 seconds
        Then I should see "iu.jetstream-cloud.org - TG-CCR190024" within 5 seconds
        And I should see an element with xpath "//h2[contains(string(),'Servers')]" within 20 seconds
        Then I save the "exosphere-save" item in browser local storage

    @launch
    Scenario: Launch an instance
        Given a browser
        When I go to Exosphere
        Then I should see "Choose a login method" within 60 seconds
        When I load the "exosphere-save" item in browser local storage
        Then I should see "iu.jetstream-cloud.org - TG-CCR190024" within 15 seconds
        And I should see an element with xpath "//h2[contains(string(),'Instances')]" within 20 seconds
        And I should not see an element with xpath "//div[contains(string(),'bdd_test_server')]"
        When I click the "Create" button
        And I click the "Instance" button
        Then the browser's URL should contain "/projects/f477d7139ced4da384dab42001a7ea3c/images"
        And I should see "Images loading..."
        And I should see an element with xpath "//h2[contains(string(),'Choose an image')]" within 120 seconds
        When I fill input labeled "Filter on image name:" with "JS-API-Featured-Ubuntu20-Latest"
        And I click the "Choose" button
        Then I should see an element with xpath "//h2[contains(string(),'Create Instance')]" within 5 seconds
        # Wait a few seconds to allow all API requests to complete
        Then I wait for 5 seconds
        When I fill input labeled "Name" with "bdd_test_server"
        And I click the last "Create" button
        Then I should see an element with xpath "//div[contains(string(),'bdd_test_server')]" within 5 seconds
        And the browser's URL should contain "/projects/f477d7139ced4da384dab42001a7ea3c/servers"
        When I press the last element with xpath "//div[contains(string(),'bdd_test_server')]"
        Then I should see an element with xpath "//h2[contains(string(),'Instance Details')]" within 2 seconds
        And I should see an element with xpath "//div[contains(string(),'Building')]" within 5 seconds
        When I click the "See detail" button
        Then I should see "Detailed status"
        And I should see "OpenStack status"
        And I should see "Power state"
        And I should see "Server Dashboard and Terminal readiness"
        # Now we wait for the instance to become ready...
        Then I should see an element with xpath "//div[contains(string(),'Partially Active')]" within 500 seconds
        Then I should see an element with xpath "//div[contains(string(),'Ready')]" within 300 seconds


    @delete
    Scenario: Delete instance
        Given a browser
        When I go to Exosphere
        Then I should see "Choose a login method" within 60 seconds
        When I load the "exosphere-save" item in browser local storage
        Then I should see "iu.jetstream-cloud.org - TG-CCR190024" within 15 seconds
        And I should see an element with xpath "//h2[contains(string(),'Instances')]" within 20 seconds
        And I should see an element with xpath "//div[contains(string(),'bdd_test_server')]"
        Then I should see an element with xpath "//div[contains(string(),'bdd_test_server')]" within 30 seconds
        When I press the last element with xpath "//div[contains(string(),'bdd_test_server')]"
        Then I should see an element with xpath "//h2[contains(string(),'Instance Details')]" within 2 seconds
        When I click the "See detail" button
        Then I should see "Detailed status"
        And I should see "OpenStack status"
        And I should see "Power state"
        And I should see "Server Dashboard and Terminal readiness"
        When I click the "Delete" button
        Then I should see "Are you sure you want to delete?" within 5 seconds
        When I click the "Yes" button
        Then I should see "Deleting..." within 5 seconds
        And I should not see an element with xpath "//div[contains(string(),'bdd_test_server')]" within 30 seconds
        And I should not see "Deleting..."
