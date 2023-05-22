module LocalStorage.LocalStorage exposing
    ( decodeStoredState
    , generateStoredState
    , hydrateModelFromStoredState
    )

import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage.Types exposing (StoredProject, StoredProject2, StoredProject3, StoredProject4, StoredState)
import OpenStack.Types as OSTypes
import Style.Types as ST
import Time
import Types.Project
import Types.SharedModel as Types
import UUID
import View.Helpers exposing (toExoPalette)


generateStoredState : Types.SharedModel -> Encode.Value
generateStoredState model =
    let
        strippedProjects =
            List.map generateStoredProject model.projects
    in
    encodeStoredState strippedProjects model.clientUuid model.style.styleMode model.viewContext.experimentalFeaturesEnabled


generateStoredProject : Types.Project.Project -> StoredProject
generateStoredProject project =
    { secret = project.secret
    , auth = project.auth
    , region = project.region
    , endpoints = project.endpoints
    , description = project.description
    }


hydrateModelFromStoredState : (UUID.UUID -> Types.SharedModel) -> UUID.UUID -> StoredState -> Types.SharedModel
hydrateModelFromStoredState emptyModel newClientUuid storedState =
    let
        model =
            emptyModel clientUuid

        projects =
            List.map
                hydrateProjectFromStoredProject
                storedState.projects

        clientUuid =
            -- If client UUID exists in stored state then use that, else set a new one
            case storedState.clientUuid of
                Just uuid ->
                    uuid

                Nothing ->
                    newClientUuid

        styleMode =
            storedState.styleMode
                |> Maybe.withDefault { theme = ST.System, systemPreference = Nothing }
                |> (\r -> { r | systemPreference = model.style.styleMode.systemPreference })

        oldStyle =
            model.style

        newStyle =
            { oldStyle | styleMode = styleMode }

        experimentalFeaturesEnabled =
            storedState.experimentalFeaturesEnabled
                |> Maybe.withDefault False

        viewContext =
            model.viewContext
    in
    { model
        | projects = projects
        , style = newStyle
        , viewContext =
            { viewContext
                | experimentalFeaturesEnabled = experimentalFeaturesEnabled
                , palette = toExoPalette newStyle
            }
    }


hydrateProjectFromStoredProject : StoredProject -> Types.Project.Project
hydrateProjectFromStoredProject storedProject =
    { secret = storedProject.secret
    , auth = storedProject.auth
    , region = storedProject.region
    , endpoints = storedProject.endpoints
    , description = storedProject.description
    , images = RDPP.empty
    , servers = RDPP.empty
    , serverImages = []
    , flavors = []
    , keypairs = RDPP.empty
    , volumes = RDPP.empty
    , volumeSnapshots = RDPP.empty
    , networks = RDPP.empty
    , shares = RDPP.empty
    , autoAllocatedNetworkUuid = RDPP.empty
    , dnsRecordSets = RDPP.empty
    , floatingIps = RDPP.empty
    , ports = RDPP.empty
    , securityGroups = RDPP.empty
    , computeQuota = RDPP.empty
    , volumeQuota = RDPP.empty
    , networkQuota = RDPP.empty
    , jetstream2Allocations = RDPP.empty
    }



-- Encoders


encodeStoredState : List StoredProject -> UUID.UUID -> ST.StyleMode -> Bool -> Encode.Value
encodeStoredState projects clientUuid styleMode experimentalFeaturesEnabled =
    let
        secretEncode : Types.Project.ProjectSecret -> Encode.Value
        secretEncode secret =
            case secret of
                Types.Project.NoProjectSecret ->
                    Encode.object
                        [ ( "secretType", Encode.string "noProjectSecret" ) ]

                Types.Project.ApplicationCredential appCred ->
                    Encode.object
                        [ ( "secretType", Encode.string "applicationCredential" )
                        , ( "appCredentialId", Encode.string appCred.uuid )
                        , ( "appCredentialSecret", Encode.string appCred.secret )
                        ]

        storedProjectEncode : StoredProject -> Encode.Value
        storedProjectEncode storedProject =
            Encode.object
                [ ( "secret", secretEncode storedProject.secret )
                , ( "auth", encodeAuthToken storedProject.auth )
                , ( "region", encodeRegion storedProject.region )
                , ( "endpoints", encodeExoEndpoints storedProject.endpoints )
                , ( "description"
                  , storedProject.description
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]
    in
    Encode.object
        [ ( "8"
          , Encode.object
                [ ( "projects", Encode.list storedProjectEncode projects )
                , ( "clientUuid", Encode.string (UUID.toString clientUuid) )
                , ( "styleMode", encodeStyleMode styleMode )
                , ( "experimentalFeaturesEnabled", Encode.bool experimentalFeaturesEnabled )
                ]
          )
        ]


encodeAuthToken : OSTypes.ScopedAuthToken -> Encode.Value
encodeAuthToken authToken =
    Encode.object
        [ ( "catalog", encodeCatalog authToken.catalog )
        , ( "project"
          , Encode.object
                [ ( "name", Encode.string authToken.project.name )
                , ( "uuid", Encode.string authToken.project.uuid )
                ]
          )
        , ( "projectDomain"
          , Encode.object
                [ ( "name", Encode.string authToken.projectDomain.name )
                , ( "uuid", Encode.string authToken.projectDomain.uuid )
                ]
          )
        , ( "user"
          , Encode.object
                [ ( "name", Encode.string authToken.user.name )
                , ( "uuid", Encode.string authToken.user.uuid )
                ]
          )
        , ( "userDomain"
          , Encode.object
                [ ( "name", Encode.string authToken.userDomain.name )
                , ( "uuid", Encode.string authToken.userDomain.uuid )
                ]
          )
        , ( "expiresAt", Encode.int (Time.posixToMillis authToken.expiresAt) )
        , ( "tokenValue", Encode.string authToken.tokenValue )
        ]


encodeRegion : Maybe OSTypes.Region -> Encode.Value
encodeRegion maybeRegion =
    case maybeRegion of
        Nothing ->
            Encode.null

        Just region ->
            Encode.object
                [ ( "id", Encode.string region.id )
                , ( "description", Encode.string region.description )
                ]


encodeCatalog : OSTypes.ServiceCatalog -> Encode.Value
encodeCatalog serviceCatalog =
    Encode.list encodeService serviceCatalog


encodeService : OSTypes.Service -> Encode.Value
encodeService service =
    Encode.object
        [ ( "name", Encode.string service.name )
        , ( "type_", Encode.string service.type_ )
        , ( "endpoints", Encode.list encodeCatalogEndpoint service.endpoints )
        ]


encodeCatalogEndpoint : OSTypes.Endpoint -> Encode.Value
encodeCatalogEndpoint endpoint =
    Encode.object
        [ ( "interface", encodeCatalogEndpointInterface endpoint.interface )
        , ( "url", Encode.string endpoint.url )
        , ( "region_id", Encode.string endpoint.regionId )
        ]


encodeCatalogEndpointInterface : OSTypes.EndpointInterface -> Encode.Value
encodeCatalogEndpointInterface endpointInterface =
    let
        interfaceString =
            case endpointInterface of
                OSTypes.Public ->
                    "public"

                OSTypes.Admin ->
                    "admin"

                OSTypes.Internal ->
                    "internal"
    in
    Encode.string interfaceString


encodeExoEndpoints : Types.Project.Endpoints -> Encode.Value
encodeExoEndpoints endpoints =
    Encode.object
        [ ( "cinder", Encode.string endpoints.cinder )
        , ( "glance", Encode.string endpoints.glance )
        , ( "keystone", Encode.string endpoints.keystone )
        , ( "manila"
          , endpoints.manila
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "nova", Encode.string endpoints.nova )
        , ( "neutron", Encode.string endpoints.neutron )
        , ( "jetstream2Accounting"
          , endpoints.jetstream2Accounting
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "designate", endpoints.designate |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        ]


encodeStyleMode : ST.StyleMode -> Encode.Value
encodeStyleMode { theme } =
    Encode.string <|
        case theme of
            ST.Override ST.Dark ->
                "darkMode"

            ST.Override ST.Light ->
                "lightMode"

            ST.System ->
                "system"



-- Decoders


decodeStoredState : Decode.Decoder StoredState
decodeStoredState =
    let
        projects =
            Decode.oneOf
                [ Decode.at [ "0", "providers" ] (Decode.list storedProjectDecode1)
                , Decode.at [ "1", "projects" ] (Decode.list storedProjectDecode1)

                -- Added ApplicationCredential
                , Decode.at [ "2", "projects" ] (Decode.list storedProjectDecode2)

                -- Added Endpoints
                , Decode.at [ "3", "projects" ] (Decode.list storedProjectDecode3)

                -- Added client UUID
                , Decode.at [ "4", "projects" ] (Decode.list storedProjectDecode3)

                -- Added StyleMode
                , Decode.at [ "5", "projects" ] (Decode.list storedProjectDecode3)

                -- Added ExperimentalFeaturesEnabled
                , Decode.at [ "6", "projects" ] (Decode.list storedProjectDecode3)

                -- Added project description field
                , Decode.at [ "7", "projects" ] (Decode.list storedProjectDecode4)

                -- Added region
                , Decode.at [ "8", "projects" ] (Decode.list storedProjectDecode)
                ]

        clientUuid =
            -- This is tricky; optional field that will either be Just a UUID.UUID, or Nothing (either because we don't
            -- have a clientUuid key in the JSON, or because converting the decoded string to UUID failed).
            Decode.maybe
                (Decode.oneOf
                    [ Decode.at [ "4", "clientUuid" ] Decode.string
                    , Decode.at [ "5", "clientUuid" ] Decode.string
                    , Decode.at [ "6", "clientUuid" ] Decode.string
                    , Decode.at [ "7", "clientUuid" ] Decode.string
                    , Decode.at [ "8", "clientUuid" ] Decode.string
                    ]
                    |> Decode.map UUID.fromString
                    |> Decode.andThen
                        (\result ->
                            case result of
                                Ok uuid ->
                                    Decode.succeed uuid

                                Err _ ->
                                    Decode.fail ""
                        )
                )

        styleMode =
            Decode.maybe
                (Decode.oneOf
                    [ Decode.at [ "5", "styleMode" ] Decode.string
                    , Decode.at [ "6", "styleMode" ] Decode.string
                    , Decode.at [ "7", "styleMode" ] Decode.string
                    , Decode.at [ "8", "styleMode" ] Decode.string
                    ]
                    |> Decode.andThen decodeStyleMode
                )

        experimentalFeaturesEnabled =
            Decode.maybe
                (Decode.oneOf
                    [ Decode.at [ "6", "experimentalFeaturesEnabled" ] Decode.bool
                    , Decode.at [ "7", "experimentalFeaturesEnabled" ] Decode.bool
                    , Decode.at [ "8", "experimentalFeaturesEnabled" ] Decode.bool
                    ]
                )
    in
    Decode.map4 StoredState projects clientUuid styleMode experimentalFeaturesEnabled


storedProjectDecode1 : Decode.Decoder StoredProject
storedProjectDecode1 =
    Decode.fail "Stored projects with a hard-coded password are no longer supported."


storedProject2ToStoredProject : StoredProject2 -> Decode.Decoder StoredProject
storedProject2ToStoredProject sp =
    case Helpers.serviceCatalogToEndpoints sp.auth.catalog Nothing of
        Ok endpoints ->
            Decode.succeed <|
                StoredProject
                    sp.secret
                    sp.auth
                    Nothing
                    endpoints
                    Nothing

        Err e ->
            Decode.fail ("Could not decode endpoints from service catalog because: " ++ e)


storedProjectDecode2 : Decode.Decoder StoredProject
storedProjectDecode2 =
    Decode.map2 StoredProject2
        (Decode.field "secret" decodeProjectSecret)
        (Decode.field "auth" decodeStoredAuthTokenDetails)
        |> Decode.andThen storedProject2ToStoredProject


storedProject3ToStoredProject : StoredProject3 -> Decode.Decoder StoredProject
storedProject3ToStoredProject sp =
    Decode.succeed <|
        StoredProject
            sp.secret
            sp.auth
            Nothing
            sp.endpoints
            Nothing


storedProjectDecode3 : Decode.Decoder StoredProject
storedProjectDecode3 =
    Decode.map3 StoredProject3
        (Decode.field "secret" decodeProjectSecret)
        (Decode.field "auth" decodeStoredAuthTokenDetails)
        (Decode.field "endpoints" decodeEndpoints)
        |> Decode.andThen storedProject3ToStoredProject


decodeProjectSecret : Decode.Decoder Types.Project.ProjectSecret
decodeProjectSecret =
    let
        -- https://thoughtbot.com/blog/5-common-json-decoders#5---conditional-decoding-based-on-a-field
        projectSecretFromType : String -> Decode.Decoder Types.Project.ProjectSecret
        projectSecretFromType typeStr =
            case typeStr of
                "noProjectSecret" ->
                    Decode.succeed Types.Project.NoProjectSecret

                "applicationCredential" ->
                    Decode.map2
                        OSTypes.ApplicationCredential
                        (Decode.field "appCredentialId" Decode.string)
                        (Decode.field "appCredentialSecret" Decode.string)
                        |> Decode.map Types.Project.ApplicationCredential

                _ ->
                    Decode.fail <| "Invalid user type \"" ++ typeStr ++ "\". Must be either password or applicationCredential."
    in
    Decode.field "secretType" Decode.string |> Decode.andThen projectSecretFromType


storedProjectDecode4 : Decode.Decoder StoredProject
storedProjectDecode4 =
    Decode.map4 StoredProject4
        (Decode.field "secret" decodeProjectSecret)
        (Decode.field "auth" decodeStoredAuthTokenDetails)
        (Decode.field "endpoints" decodeEndpoints)
        (Decode.field "description" (Decode.nullable Decode.string))
        |> Decode.andThen storedProject4ToStoredProject


storedProject4ToStoredProject : StoredProject4 -> Decode.Decoder StoredProject
storedProject4ToStoredProject sp =
    Decode.succeed <|
        StoredProject
            sp.secret
            sp.auth
            Nothing
            sp.endpoints
            sp.description


storedProjectDecode : Decode.Decoder StoredProject
storedProjectDecode =
    Decode.map5 StoredProject
        (Decode.field "secret" decodeProjectSecret)
        (Decode.field "auth" decodeStoredAuthTokenDetails)
        (Decode.field "region" decodeRegion)
        (Decode.field "endpoints" decodeEndpoints)
        (Decode.field "description" (Decode.nullable Decode.string))


decodeStoredAuthTokenDetails : Decode.Decoder OSTypes.ScopedAuthToken
decodeStoredAuthTokenDetails =
    Decode.map7 OSTypes.ScopedAuthToken
        (Decode.field "catalog" (Decode.list openstackStoredServiceDecoder))
        (Decode.field "project" decodeNameAndId)
        (Decode.field "projectDomain" decodeNameAndId)
        (Decode.field "user" decodeNameAndId)
        (Decode.field "userDomain" decodeNameAndId)
        (Decode.field "expiresAt" Decode.int
            |> Decode.map Time.millisToPosix
        )
        (Decode.field "tokenValue" Decode.string)


decodeRegion : Decode.Decoder (Maybe OSTypes.Region)
decodeRegion =
    Decode.nullable <|
        Decode.map2 OSTypes.Region
            (Decode.field "id" Decode.string)
            (Decode.field "description" Decode.string)


decodeEndpoints : Decode.Decoder Types.Project.Endpoints
decodeEndpoints =
    Decode.map8 Types.Project.Endpoints
        (Decode.field "cinder" Decode.string)
        (Decode.field "glance" Decode.string)
        (Decode.field "keystone" Decode.string)
        (Decode.oneOf
            -- This decodes projects which do not have Manila support.
            [ Decode.field "manila" Decode.string |> Decode.nullable
            , Decode.succeed Nothing
            ]
        )
        (Decode.field "nova" Decode.string)
        (Decode.field "neutron" Decode.string)
        (Decode.oneOf
            -- This decodes earlier stored projects which do not have the jetstream2Accounting field in encoded endpoints
            [ Decode.field "jetstream2Accounting" Decode.string |> Decode.nullable
            , Decode.succeed Nothing
            ]
        )
        (Decode.oneOf
            -- This decodes earlier stored projects which do not have the designate field in encoded endpoints
            [ Decode.field "designate" Decode.string |> Decode.nullable
            , Decode.succeed Nothing
            ]
        )


decodeNameAndId : Decode.Decoder OSTypes.NameAndUuid
decodeNameAndId =
    Decode.map2 OSTypes.NameAndUuid
        (Decode.field "name" Decode.string)
        (Decode.field "uuid" Decode.string)


openstackStoredServiceDecoder : Decode.Decoder OSTypes.Service
openstackStoredServiceDecoder =
    Decode.map3 OSTypes.Service
        (Decode.field "name" Decode.string)
        (Decode.field "type_" Decode.string)
        (Decode.field "endpoints" (Decode.list openstackStoredEndpointDecoder))


openstackStoredEndpointDecoder : Decode.Decoder OSTypes.Endpoint
openstackStoredEndpointDecoder =
    Decode.map3 OSTypes.Endpoint
        (Decode.field "interface" Decode.string
            |> Decode.andThen openstackStoredEndpointInterfaceDecoder
        )
        (Decode.field "url" Decode.string)
        -- Older stored projects had no region ID for endpoints, so this defaults to a placeholder value.
        -- cmart does not expect this to have any logic implications because we store endpoint URLs separately.
        (Decode.oneOf
            [ Decode.field "region_id" Decode.string
            , Decode.succeed "unknown-region"
            ]
        )


openstackStoredEndpointInterfaceDecoder : String -> Decode.Decoder OSTypes.EndpointInterface
openstackStoredEndpointInterfaceDecoder interface =
    case interface of
        "public" ->
            Decode.succeed OSTypes.Public

        "admin" ->
            Decode.succeed OSTypes.Admin

        "internal" ->
            Decode.succeed OSTypes.Internal

        _ ->
            Decode.fail "unrecognized interface type"


decodeStyleMode : String -> Decode.Decoder ST.StyleMode
decodeStyleMode styleModeStr =
    case styleModeStr of
        "darkMode" ->
            Decode.succeed <|
                { theme = ST.Override ST.Dark
                , systemPreference = Nothing
                }

        "lightMode" ->
            Decode.succeed <|
                { theme = ST.Override ST.Light
                , systemPreference = Nothing
                }

        "system" ->
            Decode.succeed <|
                { theme = ST.System
                , systemPreference = Nothing
                }

        _ ->
            Decode.fail "unrecognized style mode"
