module Tests.Page.LoginOpenstack exposing (cloudSelectionSuite, readCredentialFileSuite)

import Expect
import OpenStack.CloudsYaml
import OpenStack.Types exposing (ApplicationCredential)
import Page.LoginOpenstack exposing (CredentialFileError(..), CredentialFileOutcome(..))
import Set
import Test exposing (Test, describe, test)


cloud : String -> OpenStack.CloudsYaml.CloudEntry
cloud name =
    { name = name
    , authUrl = "https://cell.alliance.rebel:5000/v3"
    , appCredential = ApplicationCredential (name ++ "-id") (name ++ "-secret")
    , regionName = Nothing
    }


modelWithClouds : List String -> List String -> Page.LoginOpenstack.Model
modelWithClouds names selected =
    let
        model =
            Page.LoginOpenstack.init Nothing
    in
    { model
        | clouds = List.map cloud names
        , selectedClouds = Set.fromList selected
    }


cloudSelectionSuite : Test
cloudSelectionSuite =
    describe "picking which clouds to log in to"
        [ test "selects nothing when nothing is ticked" <|
            \() ->
                modelWithClouds [ "one", "two" ] []
                    |> Page.LoginOpenstack.selectedClouds
                    |> Expect.equal []
        , test "selects only the ticked clouds" <|
            \() ->
                modelWithClouds [ "one", "two", "three" ] [ "two" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map .name
                    |> Expect.equal [ "two" ]
        , test "keeps the order of the file rather than the order of the selection" <|
            \() ->
                modelWithClouds [ "one", "two", "three" ] [ "three", "one" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map .name
                    |> Expect.equal [ "one", "three" ]
        , test "carries the application credential of each selected cloud" <|
            \() ->
                modelWithClouds [ "one", "two" ] [ "one", "two" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map (.appCredential >> .uuid)
                    |> Expect.equal [ "one-id", "two-id" ]
        , test "ignores a selected name that is not in the file" <|
            \() ->
                modelWithClouds [ "one" ] [ "one", "ghost" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map .name
                    |> Expect.equal [ "one" ]
        ]


openrcWithAppCredential : String
openrcWithAppCredential =
    """#!/usr/bin/env bash

export OS_AUTH_TYPE=v3applicationcredential
export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_REGION_NAME="IU"
export OS_PROJECT_NAME="cloud-riders"
export OS_APPLICATION_CREDENTIAL_ID=abcd-efgh
export OS_APPLICATION_CREDENTIAL_SECRET=supersecret
"""


openrcWithAppCredentialAuthButNoCredential : String
openrcWithAppCredentialAuthButNoCredential =
    """#!/usr/bin/env bash

export OS_AUTH_TYPE=v3applicationcredential
export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_APPLICATION_CREDENTIAL_ID=abcd-efgh
"""


openrcWithPassword : String
openrcWithPassword =
    """#!/usr/bin/env bash

export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_USER_DOMAIN_NAME="Default"
export OS_USERNAME="enfysnest"
export OS_PASSWORD="hunter2"
"""


singleCloudYaml : String
singleCloudYaml =
    """clouds:
  cloud_riders_IU:
    region_name: IU
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
"""


twoCloudsYaml : String
twoCloudsYaml =
    singleCloudYaml
        ++ """  rogue_squadron_TACC:
    region_name: TACC
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: ijkl-mnop
      application_credential_secret: alsosecret
"""


passwordCloudsYaml : String
passwordCloudsYaml =
    """clouds:
  openstack:
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      username: enfysnest
      password: hunter2
"""


read : String -> CredentialFileOutcome
read =
    Page.LoginOpenstack.readCredentialFile Page.LoginOpenstack.defaultCreds


readCredentialFileSuite : Test
readCredentialFileSuite =
    describe "reading a credential file"
        [ test "an OpenRC file with an application credential is ready to log in with" <|
            \() ->
                read openrcWithAppCredential
                    |> Expect.equal
                        (LogInWith
                            { name = "cloud-riders"
                            , authUrl = "https://cell.alliance.rebel:5000/v3"
                            , appCredential = ApplicationCredential "abcd-efgh" "supersecret"
                            , regionName = Nothing
                            }
                        )
        , test "an OpenRC file without a project name falls back to the auth URL host" <|
            \() ->
                """export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_APPLICATION_CREDENTIAL_ID=abcd-efgh
export OS_APPLICATION_CREDENTIAL_SECRET=supersecret
"""
                    |> read
                    |> Expect.equal
                        (LogInWith
                            { name = "cell.alliance.rebel"
                            , authUrl = "https://cell.alliance.rebel:5000/v3"
                            , appCredential = ApplicationCredential "abcd-efgh" "supersecret"
                            , regionName = Nothing
                            }
                        )
        , test "an OpenRC file with a password fills in the password form" <|
            \() ->
                read openrcWithPassword
                    |> Expect.equal
                        (FillInPasswordForm
                            { authUrl = "https://cell.alliance.rebel:5000/v3"
                            , userDomain = "Default"
                            , username = "enfysnest"
                            , password = "hunter2"
                            }
                        )
        , test "an OpenRC file set up for application credentials but missing the secret is refused" <|
            \() ->
                read openrcWithAppCredentialAuthButNoCredential
                    |> Expect.equal (CredentialFileProblem IncompleteAppCredential)
        , test "a clouds.yaml with one cloud is ready to log in with, region included" <|
            \() ->
                read singleCloudYaml
                    |> Expect.equal
                        (LogInWith
                            { name = "cloud_riders_IU"
                            , authUrl = "https://cell.alliance.rebel:5000/v3"
                            , appCredential = ApplicationCredential "abcd-efgh" "supersecret"
                            , regionName = Just "IU"
                            }
                        )
        , test "a clouds.yaml with several clouds offers a choice" <|
            \() ->
                read twoCloudsYaml
                    |> Expect.equal
                        (ChooseAmongClouds
                            [ { name = "cloud_riders_IU"
                              , authUrl = "https://cell.alliance.rebel:5000/v3"
                              , appCredential = ApplicationCredential "abcd-efgh" "supersecret"
                              , regionName = Just "IU"
                              }
                            , { name = "rogue_squadron_TACC"
                              , authUrl = "https://cell.alliance.rebel:5000/v3"
                              , appCredential = ApplicationCredential "ijkl-mnop" "alsosecret"
                              , regionName = Just "TACC"
                              }
                            ]
                        )
        , test "a password style clouds.yaml is refused" <|
            \() ->
                read passwordCloudsYaml
                    |> Expect.equal (CredentialFileProblem NoApplicationCredentials)
        , test "prose is refused" <|
            \() ->
                read "please find my credentials attached"
                    |> Expect.equal (CredentialFileProblem UnrecognizedFile)
        , test "an empty file is refused" <|
            \() ->
                read ""
                    |> Expect.equal (CredentialFileProblem UnrecognizedFile)
        ]
