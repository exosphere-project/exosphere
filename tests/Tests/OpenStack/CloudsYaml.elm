module Tests.OpenStack.CloudsYaml exposing (cloudsYamlSuite)

import Expect
import OpenStack.CloudsYaml
import OpenStack.Types exposing (ApplicationCredential)
import Test exposing (Test, describe, test)


{-| The shape `Helpers.Credentials.getCloudsYaml` writes for a single project.
-}
exosphereSingleProject : String
exosphereSingleProject =
    """clouds:
  cloud_riders_IU:
    region_name: IU
    interface: public
    identity_api_version: 3
    auth_type: v3applicationcredential
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
"""


{-| The same shape with several projects, which is what `getCloudsYaml` writes when handed a
list of more than one project.
-}
exosphereMultipleProjects : String
exosphereMultipleProjects =
    """clouds:
  cloud_riders_IU:
    region_name: IU
    interface: public
    identity_api_version: 3
    auth_type: v3applicationcredential
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
  rogue_squadron_TACC:
    region_name: TACC
    interface: public
    identity_api_version: 3
    auth_type: v3applicationcredential
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: ijkl-mnop
      application_credential_secret: alsosecret
"""


{-| The shape Horizon offers for download after you create an application credential.
-}
horizonCloudsYaml : String
horizonCloudsYaml =
    """# This is a clouds.yaml file, which can be used by OpenStack tools as a source
# of configuration on how to connect to a cloud.
clouds:
  openstack:
    auth:
      auth_url: https://cell.alliance.rebel:5000
      application_credential_id: "abcd-efgh"
      application_credential_secret: "supersecret"
    region_name: "CellOne"
    interface: "public"
    identity_api_version: 3
    auth_type: "v3applicationcredential"
"""


horizonWithoutRegion : String
horizonWithoutRegion =
    """clouds:
  openstack:
    auth:
      auth_url: https://cell.alliance.rebel:5000
      application_credential_id: "abcd-efgh"
      application_credential_secret: "supersecret"
    interface: "public"
    identity_api_version: 3
    auth_type: "v3applicationcredential"
"""


passwordCloudsYaml : String
passwordCloudsYaml =
    """clouds:
  openstack:
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      username: enfysnest
      password: hunter2
      project_name: cloud-riders
      user_domain_name: Default
    region_name: CellOne
    identity_api_version: 3
"""


{-| The same auth URL, application credential and region filed under two names.
-}
duplicateLogins : String
duplicateLogins =
    """clouds:
  alpha:
    region_name: IU
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
  zulu:
    region_name: IU
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
"""


sameCredentialTwoRegions : String
sameCredentialTwoRegions =
    """clouds:
  alpha:
    region_name: IU
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
  zulu:
    region_name: TACC
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: supersecret
"""


openrcNotCloudsYaml : String
openrcNotCloudsYaml =
    """#!/usr/bin/env bash

export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
export OS_APPLICATION_CREDENTIAL_ID=abcd-efgh
export OS_APPLICATION_CREDENTIAL_SECRET=supersecret
"""


withWindowsLineEndings : String -> String
withWindowsLineEndings =
    String.replace "\n" "\u{000D}\n"


expectedFirstEntry : OpenStack.CloudsYaml.CloudEntry
expectedFirstEntry =
    { name = "cloud_riders_IU"
    , authUrl = "https://cell.alliance.rebel:5000/v3"
    , appCredential = ApplicationCredential "abcd-efgh" "supersecret"
    , regionName = Just "IU"
    }


cloudsYamlSuite : Test
cloudsYamlSuite =
    describe "clouds.yaml parsing"
        [ describe "looksLikeCloudsYaml"
            [ test "recognizes a top level clouds key" <|
                \() ->
                    OpenStack.CloudsYaml.looksLikeCloudsYaml horizonCloudsYaml
                        |> Expect.equal True
            , test "recognizes a top level clouds key with a trailing comment" <|
                \() ->
                    OpenStack.CloudsYaml.looksLikeCloudsYaml "clouds: # all of them\n  openstack:\n"
                        |> Expect.equal True
            , test "recognizes a top level clouds key with CRLF line endings" <|
                \() ->
                    OpenStack.CloudsYaml.looksLikeCloudsYaml (withWindowsLineEndings horizonCloudsYaml)
                        |> Expect.equal True
            , test "ignores an indented clouds key" <|
                \() ->
                    OpenStack.CloudsYaml.looksLikeCloudsYaml "something:\n  clouds:\n    openstack: {}\n"
                        |> Expect.equal False
            , test "ignores a commented out clouds key" <|
                \() ->
                    OpenStack.CloudsYaml.looksLikeCloudsYaml "# clouds:\n"
                        |> Expect.equal False
            , test "ignores a clouds key that carries an inline value" <|
                \() ->
                    OpenStack.CloudsYaml.looksLikeCloudsYaml "clouds: nope\n"
                        |> Expect.equal False
            , test "ignores an OpenRC file" <|
                \() ->
                    OpenStack.CloudsYaml.looksLikeCloudsYaml openrcNotCloudsYaml
                        |> Expect.equal False
            ]
        , describe "parse"
            [ test "reads the single project file Exosphere writes" <|
                \() ->
                    OpenStack.CloudsYaml.parse exosphereSingleProject
                        |> Expect.equal (Ok [ expectedFirstEntry ])
            , test "reads every project from the multiple project file Exosphere writes" <|
                \() ->
                    OpenStack.CloudsYaml.parse exosphereMultipleProjects
                        |> Expect.equal
                            (Ok
                                [ expectedFirstEntry
                                , { name = "rogue_squadron_TACC"
                                  , authUrl = "https://cell.alliance.rebel:5000/v3"
                                  , appCredential = ApplicationCredential "ijkl-mnop" "alsosecret"
                                  , regionName = Just "TACC"
                                  }
                                ]
                            )
            , test "reads the file Horizon offers" <|
                \() ->
                    OpenStack.CloudsYaml.parse horizonCloudsYaml
                        |> Expect.equal
                            (Ok
                                [ { name = "openstack"
                                  , authUrl = "https://cell.alliance.rebel:5000"
                                  , appCredential = ApplicationCredential "abcd-efgh" "supersecret"
                                  , regionName = Just "CellOne"
                                  }
                                ]
                            )
            , test "treats a missing region as absent rather than as a failure" <|
                \() ->
                    OpenStack.CloudsYaml.parse horizonWithoutRegion
                        |> Result.map (List.map .regionName)
                        |> Expect.equal (Ok [ Nothing ])
            , test "reads a file with CRLF line endings" <|
                \() ->
                    OpenStack.CloudsYaml.parse (withWindowsLineEndings exosphereSingleProject)
                        |> Expect.equal (Ok [ expectedFirstEntry ])
            , test "reads a multiple project file with CRLF line endings" <|
                \() ->
                    OpenStack.CloudsYaml.parse (withWindowsLineEndings exosphereMultipleProjects)
                        |> Result.map (List.map .name)
                        |> Expect.equal (Ok [ "cloud_riders_IU", "rogue_squadron_TACC" ])
            , test "skips clouds that carry no application credential" <|
                \() ->
                    OpenStack.CloudsYaml.parse
                        (exosphereSingleProject ++ "  password_only:\n    auth:\n      auth_url: https://elsewhere:5000/v3\n      username: enfysnest\n")
                        |> Result.map (List.map .name)
                        |> Expect.equal (Ok [ "cloud_riders_IU" ])
            , test "reports a password only file as carrying no application credentials" <|
                \() ->
                    OpenStack.CloudsYaml.parse passwordCloudsYaml
                        |> Expect.equal (Err OpenStack.CloudsYaml.NoAppCredentials)
            , test "rejects an application credential with a blank secret" <|
                \() ->
                    OpenStack.CloudsYaml.parse
                        """clouds:
  openstack:
    auth:
      auth_url: https://cell.alliance.rebel:5000/v3
      application_credential_id: abcd-efgh
      application_credential_secret: ""
"""
                        |> Expect.equal (Err OpenStack.CloudsYaml.NoAppCredentials)
            , test "collapses two entries that are the same login under different names" <|
                \() ->
                    OpenStack.CloudsYaml.parse duplicateLogins
                        |> Result.map (List.map .name)
                        |> Expect.equal (Ok [ "alpha" ])
            , test "keeps entries that share a credential but name different regions" <|
                \() ->
                    OpenStack.CloudsYaml.parse sameCredentialTwoRegions
                        |> Result.map (List.map .regionName)
                        |> Expect.equal (Ok [ Just "IU", Just "TACC" ])
            , test "rejects an OpenRC file" <|
                \() ->
                    OpenStack.CloudsYaml.parse openrcNotCloudsYaml
                        |> Expect.equal (Err OpenStack.CloudsYaml.NotCloudsYaml)
            , test "rejects prose" <|
                \() ->
                    OpenStack.CloudsYaml.parse "the quick brown fox jumps over the lazy dog"
                        |> Expect.equal (Err OpenStack.CloudsYaml.NotCloudsYaml)
            , test "rejects an empty string" <|
                \() ->
                    OpenStack.CloudsYaml.parse ""
                        |> Expect.equal (Err OpenStack.CloudsYaml.NotCloudsYaml)
            , test "rejects YAML that has no clouds key" <|
                \() ->
                    OpenStack.CloudsYaml.parse "servers:\n  one:\n    name: bespin\n"
                        |> Expect.equal (Err OpenStack.CloudsYaml.NotCloudsYaml)
            , test "rejects a clouds key whose value is not a mapping" <|
                \() ->
                    OpenStack.CloudsYaml.parse "clouds: everywhere\n"
                        |> Expect.equal (Err OpenStack.CloudsYaml.NotCloudsYaml)
            ]
        ]
