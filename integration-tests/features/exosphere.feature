Feature: Text presence

    @add-allocation
    Scenario: Adding a Jetstream1 account
        Given a browser
        When I go to Exosphere
        Then I should see "Add project" within 60 seconds
        When I click the "Add project" card
        Then I should see "Add Jetstream1 Account" within 60 seconds
        When I click the "Add Jetstream1 Account" button
        Then I should see "Add a Jetstream1 Account" within 15 seconds
        When I enter TACC credentials
        And I click the "IU Cloud" radio button
        And I click the "Log In" button
        Then I should see "TG-INI210003" within 15 seconds
        When I click the "TG-INI210003" checkbox
        And I click the "Choose" button
        Then I wait for 2 seconds
        Then I should see "TG-INI210003" within 5 seconds
        When I click the "TG-INI210003" card
        Then I should see "Jetstream1 IU RegionOne - TG-INI210003" within 5 seconds
        And I should see an element with xpath "//h3[contains(string(),'Instances')]" within 20 seconds

    @launch
    Scenario: Launch an instance
        Given a browser
        When I go to Exosphere
        Then I should see "Add project" within 60 seconds
        When I click the "Add project" card
        Then I should see "Choose a Login Method" within 60 seconds
        When I add a Jetstream1 Account for allocation "TG-INI210003"
        Then I should see "TG-INI210003" within 5 seconds
        When I click the "TG-INI210003" card
        Then I should see "Jetstream1 IU RegionOne - TG-INI210003" within 15 seconds
        And I should see an element with xpath "//h3[contains(string(),'Instances')]" within 20 seconds
        Given a unique instance name starting with "ubuntu"
        And I should not see the unique instance name within 30 seconds
        When I click the "Create" button
        And I click the "Instance" button
        Then the browser's URL should contain "/projects/285529556e524028aae29f9c8b0f8017/regions/RegionOne/instancesource"
        And I should see "Loading images..."
        And I should see an element with xpath "//h2[contains(string(),'Choose an Instance Source')]" within 120 seconds
        When I click the "20.04 (latest)" button
        Then I should see an element with xpath "//h2[contains(string(),'Create Instance')]" within 5 seconds
        # Wait a few seconds to allow all API requests to complete
        Then I wait for 5 seconds
        When I fill input labeled "Name" with the unique instance name
        And I press the "Show" option in the "Advanced Options" radio button group
        Then I should see "Install operating system updates?" within 2 seconds
        And I press the "No" option in the "Install operating system updates?" radio button group
        And I should see "Warning: Skipping operating system updates is a security risk" within 2 seconds
        And I click the last "Create" button
        Then I should see an element with xpath "//h3[contains(string(),'Instances')]" within 5 seconds
        When I press the last element with xpath "//h3[contains(string(),'Instances')]"
        Then I should see the unique instance name within 5 seconds
        And the browser's URL should contain "/projects/285529556e524028aae29f9c8b0f8017/regions/RegionOne/servers"
        When I press on the unique instance name
        Then I should see an element with xpath "//h2[contains(string(),'Instance')]" within 2 seconds
        And I should see an element with xpath "//div[contains(string(),'Building')]" within 10 seconds
        # Now we wait for the instance to become ready...
        Then I should see an element with xpath "//div[contains(string(),'Running Setup')]" within 500 seconds
        Then I should see an element with xpath "//div[contains(string(),'Ready')]" within 600 seconds


    @delete
    Scenario: Delete instance
        Given a browser
        When I go to Exosphere
        Then I should see "Add project" within 60 seconds
        When I click the "Add project" card
        Then I should see "Choose a Login Method" within 60 seconds
        When I add a Jetstream1 Account for allocation "TG-INI210003"
        Then I should see "TG-INI210003" within 5 seconds
        When I click the "TG-INI210003" card
        Then I should see "Jetstream1 IU RegionOne - TG-INI210003" within 15 seconds
        And I should see an element with xpath "//h3[contains(string(),'Instances')]" within 20 seconds
        When I press the last element with xpath "//h3[contains(string(),'Instances')]"
        Given a unique instance name starting with "ubuntu"
        Then I should see the unique instance name within 30 seconds
        When I press on the unique instance name
        Then I should see an element with xpath "//h2[contains(string(),'Instance')]" within 2 seconds
        When I click the "Actions" button
        When I click the "Delete" button
        Then I should see "Are you sure you want to delete?" within 5 seconds
        When I click the "Yes" button
        And I should not see the unique instance name within 30 seconds

