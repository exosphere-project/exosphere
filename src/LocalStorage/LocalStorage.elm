module LocalStorage.LocalStorage exposing
    ( decodeStoredState
    , generateStoredState
    , hydrateModelFromStoredState
    )

import Dict
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Url as UrlHelpers
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage.Types exposing (StoredProject, StoredProject1, StoredProject2, StoredState)
import OpenStack.Types as OSTypes
import RemoteData
import Style.Types
import Time
import Types.Types as Types
import UUID


generateStoredState : Types.Model -> Encode.Value
generateStoredState model =
    let
        strippedProjects =
            List.map generateStoredProject model.projects
    in
    encodeStoredState strippedProjects model.clientUuid model.style.styleMode


generateStoredProject : Types.Project -> StoredProject
generateStoredProject project =
    { secret = project.secret
    , auth = project.auth
    , endpoints = project.endpoints
    }


hydrateModelFromStoredState : (UUID.UUID -> Types.Model) -> UUID.UUID -> StoredState -> Types.Model
hydrateModelFromStoredState emptyModel newClientUuid storedState =
    let
        model =
            emptyModel clientUuid

        projects =
            List.map (hydrateProjectFromStoredProject model.cloudsWithUserAppProxy) storedState.projects

        clientUuid =
            -- If client UUID exists in stored state then use that, else set a new one
            case storedState.clientUuid of
                Just uuid ->
                    uuid

                Nothing ->
                    newClientUuid

        styleMode =
            storedState.styleMode |> Maybe.withDefault Style.Types.LightMode

        oldStyle =
            model.style

        newStyle =
            { oldStyle | styleMode = styleMode }
    in
    { model
        | projects = projects
        , style = newStyle
    }


hydrateProjectFromStoredProject : Types.CloudsWithUserAppProxy -> StoredProject -> Types.Project
hydrateProjectFromStoredProject cloudsWithTlsReverseProxy storedProject =
    { secret = storedProject.secret
    , auth = storedProject.auth
    , endpoints = storedProject.endpoints
    , images = []
    , servers = RDPP.empty
    , flavors = []
    , keypairs = []
    , volumes = RemoteData.NotAsked
    , networks = RDPP.empty
    , floatingIps = []
    , ports = RDPP.empty
    , securityGroups = []
    , computeQuota = RemoteData.NotAsked
    , volumeQuota = RemoteData.NotAsked
    , pendingCredentialedRequests = []
    , userAppProxyHostname =
        storedProject.endpoints.keystone
            |> UrlHelpers.hostnameFromUrl
            |> (\h -> Dict.get h cloudsWithTlsReverseProxy)
    }



-- Encoders


encodeStoredState : List StoredProject -> UUID.UUID -> Style.Types.StyleMode -> Encode.Value
encodeStoredState projects clientUuid styleMode =
    let
        secretEncode : Types.ProjectSecret -> Encode.Value
        secretEncode secret =
            case secret of
                Types.NoProjectSecret ->
                    Encode.object
                        [ ( "secretType", Encode.string "noProjectSecret" ) ]

                Types.OpenstackPassword _ ->
                    -- No longer storing user passwords persistently
                    Encode.object
                        [ ( "secretType", Encode.string "noProjectSecret" ) ]

                Types.ApplicationCredential appCred ->
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
                , ( "endpoints", encodeExoEndpoints storedProject.endpoints )
                ]
    in
    Encode.object
        [ ( "5"
          , Encode.object
                [ ( "projects", Encode.list storedProjectEncode projects )
                , ( "clientUuid", Encode.string (UUID.toString clientUuid) )
                , ( "styleMode", encodeStyleMode styleMode )
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


encodeExoEndpoints : Types.Endpoints -> Encode.Value
encodeExoEndpoints endpoints =
    Encode.object
        [ ( "cinder", Encode.string endpoints.cinder )
        , ( "glance", Encode.string endpoints.glance )
        , ( "keystone", Encode.string endpoints.keystone )
        , ( "nova", Encode.string endpoints.nova )
        , ( "neutron", Encode.string endpoints.neutron )
        ]


encodeStyleMode : Style.Types.StyleMode -> Encode.Value
encodeStyleMode styleMode =
    Encode.string <|
        case styleMode of
            Style.Types.DarkMode ->
                "darkMode"

            Style.Types.LightMode ->
                "lightMode"



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
                , Decode.at [ "3", "projects" ] (Decode.list storedProjectDecode)

                -- Added client UUID
                , Decode.at [ "4", "projects" ] (Decode.list storedProjectDecode)

                -- Added StyleMode
                , Decode.at [ "5", "projects" ] (Decode.list storedProjectDecode)
                ]

        clientUuid =
            -- This is tricky; optional field that will either be Just a UUID.UUID, or Nothing (either because we don't
            -- have a clientUuid key in the JSON, or because converting the decoded string to UUID failed).
            Decode.maybe
                (Decode.oneOf
                    [ Decode.at [ "4", "clientUuid" ] Decode.string
                    , Decode.at [ "5", "clientUuid" ] Decode.string
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
                (Decode.at [ "5", "styleMode" ] Decode.string
                    |> Decode.andThen decodeStyleMode
                )
    in
    Decode.map3 StoredState projects clientUuid styleMode


strToNameAndUuid : String -> OSTypes.NameAndUuid
strToNameAndUuid s =
    if Helpers.stringIsUuidOrDefault s then
        OSTypes.NameAndUuid "" s

    else
        OSTypes.NameAndUuid s ""


storedProject1ToStoredProject : StoredProject1 -> Decode.Decoder StoredProject
storedProject1ToStoredProject sp =
    let
        authToken =
            OSTypes.ScopedAuthToken
                sp.auth.catalog
                sp.auth.project
                sp.projDomain
                sp.auth.user
                sp.userDomain
                sp.auth.expiresAt
                sp.auth.tokenValue
    in
    case Helpers.serviceCatalogToEndpoints sp.auth.catalog of
        Ok endpoints ->
            Decode.succeed <|
                StoredProject
                    (Types.OpenstackPassword sp.password)
                    authToken
                    endpoints

        Err e ->
            Decode.fail ("Could not decode endpoints from service catalog because: " ++ e)


storedProjectDecode1 : Decode.Decoder StoredProject
storedProjectDecode1 =
    Decode.map4 StoredProject1
        (Decode.at [ "creds", "password" ] Decode.string)
        (Decode.field "auth" decodeStoredAuthTokenDetails1)
        (Decode.map strToNameAndUuid <|
            Decode.at [ "creds", "projectDomain" ] Decode.string
        )
        (Decode.map strToNameAndUuid <|
            Decode.at [ "creds", "userDomain" ] Decode.string
        )
        |> Decode.andThen storedProject1ToStoredProject


storedProject2ToStoredProject : StoredProject2 -> Decode.Decoder StoredProject
storedProject2ToStoredProject sp =
    case Helpers.serviceCatalogToEndpoints sp.auth.catalog of
        Ok endpoints ->
            Decode.succeed <|
                StoredProject
                    sp.secret
                    sp.auth
                    endpoints

        Err e ->
            Decode.fail ("Could not decode endpoints from service catalog because: " ++ e)


storedProjectDecode2 : Decode.Decoder StoredProject
storedProjectDecode2 =
    Decode.map2 StoredProject2
        (Decode.field "secret" decodeProjectSecret)
        (Decode.field "auth" decodeStoredAuthTokenDetails)
        |> Decode.andThen storedProject2ToStoredProject


decodeProjectSecret : Decode.Decoder Types.ProjectSecret
decodeProjectSecret =
    let
        -- https://thoughtbot.com/blog/5-common-json-decoders#5---conditional-decoding-based-on-a-field
        projectSecretFromType : String -> Decode.Decoder Types.ProjectSecret
        projectSecretFromType typeStr =
            case typeStr of
                "noProjectSecret" ->
                    Decode.succeed Types.NoProjectSecret

                "password" ->
                    Decode.field "password" Decode.string |> Decode.map Types.OpenstackPassword

                "applicationCredential" ->
                    Decode.map2
                        OSTypes.ApplicationCredential
                        (Decode.field "appCredentialId" Decode.string)
                        (Decode.field "appCredentialSecret" Decode.string)
                        |> Decode.map Types.ApplicationCredential

                _ ->
                    Decode.fail <| "Invalid user type \"" ++ typeStr ++ "\". Must be either password or applicationCredential."
    in
    Decode.field "secretType" Decode.string |> Decode.andThen projectSecretFromType


storedProjectDecode : Decode.Decoder StoredProject
storedProjectDecode =
    Decode.map3 StoredProject
        (Decode.field "secret" decodeProjectSecret)
        (Decode.field "auth" decodeStoredAuthTokenDetails)
        (Decode.field "endpoints" decodeEndpoints)


decodeStoredAuthTokenDetails1 : Decode.Decoder OSTypes.ScopedAuthToken
decodeStoredAuthTokenDetails1 =
    Decode.map7 OSTypes.ScopedAuthToken
        (Decode.field "catalog" (Decode.list openstackStoredServiceDecoder))
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.field "projectName" Decode.string)
            (Decode.field "projectUuid" Decode.string)
        )
        -- Can't determine project domain name/uuid here so we populate empty
        (Decode.succeed <| OSTypes.NameAndUuid "" "")
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.field "username" Decode.string)
            (Decode.field "userUuid" Decode.string)
        )
        -- Can't determine user domain name/uuid here so we populate empty
        (Decode.succeed <| OSTypes.NameAndUuid "" "")
        (Decode.field "expiresAt" Decode.int
            |> Decode.map Time.millisToPosix
        )
        (Decode.field "tokenValue" Decode.string)


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


decodeEndpoints : Decode.Decoder Types.Endpoints
decodeEndpoints =
    Decode.map5 Types.Endpoints
        (Decode.field "cinder" Decode.string)
        (Decode.field "glance" Decode.string)
        (Decode.field "keystone" Decode.string)
        (Decode.field "nova" Decode.string)
        (Decode.field "neutron" Decode.string)


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
    Decode.map2 OSTypes.Endpoint
        (Decode.field "interface" Decode.string
            |> Decode.andThen openstackStoredEndpointInterfaceDecoder
        )
        (Decode.field "url" Decode.string)


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


decodeStyleMode : String -> Decode.Decoder Style.Types.StyleMode
decodeStyleMode styleModeStr =
    case styleModeStr of
        "darkMode" ->
            Decode.succeed Style.Types.DarkMode

        "lightMode" ->
            Decode.succeed Style.Types.LightMode

        _ ->
            Decode.fail "unrecognized style mode"
