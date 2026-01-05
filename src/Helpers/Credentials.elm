module Helpers.Credentials exposing (getCloudsYaml, getOpenRcPs1, getOpenRcSh, getOpenRcVariables, projectCloudName)

import Helpers.String exposing (formatStringTemplate)
import Regex
import Types.Project exposing (Project)
import Yaml.Encode as YE


getOpenRcVariables : Project -> List ( String, String )
getOpenRcVariables project =
    let
        regionId : String
        regionId =
            case project.region of
                Nothing ->
                    "RegionOne"

                Just region ->
                    region.id

        -- TODO: If no app credential, then use username and ask for password
        ( appCredentialUuid, appCredentialSecret ) =
            case project.secret of
                Types.Project.ApplicationCredential appCredential ->
                    ( appCredential.uuid, appCredential.secret )

                Types.Project.NoProjectSecret ->
                    ( "", "" )
    in
    [ ( "{os-auth-url}", project.endpoints.keystone )
    , ( "{os-region}", regionId )
    , ( "{os-ac-id}", appCredentialUuid )
    , ( "{os-ac-secret}", appCredentialSecret )
    ]


getOpenRcSh : Project -> String
getOpenRcSh =
    let
        template =
            """#!/usr/bin/env bash

export OS_AUTH_TYPE=v3applicationcredential
export OS_AUTH_URL={os-auth-url}
export OS_IDENTITY_API_VERSION=3
export OS_REGION_NAME="{os-region}"
export OS_INTERFACE=public
export OS_APPLICATION_CREDENTIAL_ID="{os-ac-id}"
export OS_APPLICATION_CREDENTIAL_SECRET="{os-ac-secret}"
"""
    in
    getOpenRcVariables >> formatStringTemplate template


getOpenRcPs1 : Project -> String
getOpenRcPs1 =
    let
        template =
            """
$env:OS_AUTH_TYPE="v3applicationcredential"
$env:OS_AUTH_URL="{os-auth-url}"
$env:OS_IDENTITY_API_VERSION="3"
$env:OS_INTERFACE="public"
$env:OS_REGION_NAME="{os-region}"
$env:OS_APPLICATION_CREDENTIAL_ID="{os-ac-id}"
$env:OS_APPLICATION_CREDENTIAL_SECRET="{os-ac-secret}"
"""
    in
    getOpenRcVariables >> formatStringTemplate template


projectCloudName : Project -> String
projectCloudName project =
    let
        reNonAlphanumeric =
            Regex.fromString "\\W+"
                |> Maybe.withDefault Regex.never
    in
    (case project.region of
        Just region ->
            project.auth.project.name ++ "_" ++ region.id

        Nothing ->
            project.auth.project.name
    )
        |> Regex.replace reNonAlphanumeric (always "_")


getCloudYamlRecord : Project -> List ( String, YE.Encoder )
getCloudYamlRecord project =
    let
        regionId : String
        regionId =
            case project.region of
                Nothing ->
                    "RegionOne"

                Just region ->
                    region.id

        ( appCredentialUuid, appCredentialSecret ) =
            case project.secret of
                Types.Project.ApplicationCredential appCredential ->
                    ( appCredential.uuid, appCredential.secret )

                Types.Project.NoProjectSecret ->
                    ( "", "" )
    in
    [ ( projectCloudName project
      , YE.record
            [ ( "region_name", YE.string regionId )
            , ( "interface", YE.string "public" )
            , ( "identity_api_version", YE.int 3 )
            , ( "auth_type", YE.string "v3applicationcredential" )
            , ( "auth"
              , YE.record
                    [ ( "auth_url", YE.string project.endpoints.keystone )

                    -- , ( "project_name", YE.string project.auth.project.name )
                    -- , ( "project_domain_name", YE.string project.auth.projectDomain.name )
                    -- , ( "project_domain_id", YE.string project.auth.projectDomain.uuid )
                    , ( "application_credential_id", YE.string appCredentialUuid )
                    , ( "application_credential_secret", YE.string appCredentialSecret )
                    ]
              )
            ]
      )
    ]


getCloudsYaml : List Project -> String
getCloudsYaml projects =
    YE.record
        [ ( -- @nonlocalized
            "clouds"
          , YE.record <| List.concatMap getCloudYamlRecord projects
          )
        ]
        |> YE.toString 2
