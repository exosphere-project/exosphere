module Tests.OpenStack.OpenRc exposing (processOpenRcSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import OpenStack.OpenRc
import OpenStack.Types exposing (OpenstackLogin)
import Page.LoginOpenstack
import Test exposing (Test, describe, test)


openrcV3withComments : String
openrcV3withComments =
    """
#!/usr/bin/env bash
# To use an OpenStack cloud you need to authenticate against the Identity
# service named keystone, which returns a **Token** and **Service Catalog**.
# The catalog contains the endpoints for all services the user/tenant has
# access to - such as Compute, Image Service, Identity, Object Storage, Block
# Storage, and Networking (code-named nova, glance, keystone, swift,
# cinder, and neutron).
#
# *NOTE*: Using the 3 *Identity API* does not necessarily mean any other
# OpenStack API is version 3. For example, your cloud provider may implement
# Image API v1.1, Block Storage API v2, and Compute API v2.0. OS_AUTH_URL is
# only for the Identity API served through keystone.
export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
# With the addition of Keystone we have standardized on the term **project**
# as the entity that owns the resources.
export OS_PROJECT_ID=1d00d4b1de1d00d4b1de1d00d4b1de
export OS_PROJECT_NAME="cloud-riders"
export OS_USER_DOMAIN_NAME="Default"
if [ -z "$OS_USER_DOMAIN_NAME" ]; then unset OS_USER_DOMAIN_NAME; fi
export OS_PROJECT_DOMAIN_ID="default"
if [ -z "$OS_PROJECT_DOMAIN_ID" ]; then unset OS_PROJECT_DOMAIN_ID; fi
# unset v2.0 items in case set
unset OS_TENANT_ID
unset OS_TENANT_NAME
# In addition to the owning entity (tenant), OpenStack stores the entity
# performing the action as the **user**.
export OS_USERNAME="enfysnest"
# With Keystone you pass the keystone password.
echo "Please enter your OpenStack Password for project $OS_PROJECT_NAME as user $OS_USERNAME: "
read -sr OS_PASSWORD_INPUT
export OS_PASSWORD=$OS_PASSWORD_INPUT
# If your configuration has multiple regions, we set that information here.
# OS_REGION_NAME is optional and only valid in certain environments.
export OS_REGION_NAME="CellOne"
# Don't leave a blank variable, unset it if it was empty
if [ -z "$OS_REGION_NAME" ]; then unset OS_REGION_NAME; fi
export OS_INTERFACE=public
export OS_IDENTITY_API_VERSION=3
"""


openrcV3 : String
openrcV3 =
    """
#!/usr/bin/env bash

export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_PROJECT_ID=1d00d4b1de1d00d4b1de1d00d4b1de
export OS_PROJECT_NAME="cloud-riders"
export OS_USER_DOMAIN_NAME="Default"
if [ -z "$OS_USER_DOMAIN_NAME" ]; then unset OS_USER_DOMAIN_NAME; fi
export OS_PROJECT_DOMAIN_ID="default"
if [ -z "$OS_PROJECT_DOMAIN_ID" ]; then unset OS_PROJECT_DOMAIN_ID; fi
unset OS_TENANT_ID
unset OS_TENANT_NAME
export OS_USERNAME="enfysnest"
echo "Please enter your OpenStack Password for project $OS_PROJECT_NAME as user $OS_USERNAME: "
read -sr OS_PASSWORD_INPUT
export OS_PASSWORD=$OS_PASSWORD_INPUT
export OS_REGION_NAME="CellOne"
if [ -z "$OS_REGION_NAME" ]; then unset OS_REGION_NAME; fi
export OS_INTERFACE=public
export OS_IDENTITY_API_VERSION=3
"""


openrcPreV3 : String
openrcPreV3 =
    """
export OS_PROJECT_NAME="cloud-riders"
export OS_USERNAME="enfysnest"
export OS_IDENTITY_API_VERSION=3
export OS_USER_DOMAIN_NAME="default"
export OS_TENANT_NAME="enfysnest"
export OS_AUTH_URL="https://cell.alliance.rebel:35357/v3"
export OS_PROJECT_DOMAIN_NAME="default"
export OS_REGION_NAME="CellOne"
export OS_PASSWORD=$OS_PASSWORD_INPUT
    """


openrcNoExportKeyword : String
openrcNoExportKeyword =
    """
OS_REGION_NAME=redacted
OS_PROJECT_DOMAIN_ID=deadbeef
OS_INTERFACE=public
OS_AUTH_URL=https://mycloud.whatever:5000/v3/
OS_USERNAME=redactedusername
OS_PROJECT_ID=deadbeef
OS_USER_DOMAIN_NAME=Default
OS_PROJECT_NAME=redactedprojectname
OS_PASSWORD=redactedpassword
OS_IDENTITY_API_VERSION=3
    """


processOpenRcSuite : Test
processOpenRcSuite =
    describe "end result of processing imported openrc files"
        [ test "ensure an empty file is unmatched" <|
            \() ->
                ""
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal Page.LoginOpenstack.defaultCreds
        , test "that $OS_PASSWORD_INPUT is *not* processed" <|
            \() ->
                """
                export OS_PASSWORD=$OS_PASSWORD_INPUT
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .password
                    |> Expect.equal ""
        , test "that double quotes are not included in a processed match" <|
            \() ->
                """
                export OS_AUTH_URL="https://cell.alliance.rebel:5000/v3"
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that single quotes are accepted but not included in a processed match" <|
            \() ->
                """
                export OS_AUTH_URL='https://cell.alliance.rebel:5000/v3'
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that quotes are optional" <|
            \() ->
                """
                export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that mismatched quotes fail to parse" <|
            \() ->
                """
                export OS_AUTH_URL='https://cell.alliance.rebel:5000/v3"
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal ""
        , test "ensure pre-'API Version 3' can be processed " <|
            \() ->
                openrcPreV3
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://cell.alliance.rebel:35357/v3"
                            "default"
                            "enfysnest"
                            ""
                        )
        , test "ensure an 'API Version 3' open with comments works" <|
            \() ->
                openrcV3withComments
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://cell.alliance.rebel:5000/v3"
                            "Default"
                            "enfysnest"
                            ""
                        )
        , test "ensure an 'API Version 3' open _without_ comments works" <|
            \() ->
                openrcV3
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://cell.alliance.rebel:5000/v3"
                            "Default"
                            "enfysnest"
                            ""
                        )
        , test "ensure that export keyword is optional" <|
            \() ->
                openrcNoExportKeyword
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://mycloud.whatever:5000/v3/"
                            "Default"
                            "redactedusername"
                            "redactedpassword"
                        )
        ]
