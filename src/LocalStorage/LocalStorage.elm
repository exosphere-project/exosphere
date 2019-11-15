module LocalStorage.LocalStorage exposing
    ( decodeStoredState
    , generateStoredState
    , hydrateModelFromStoredState
    )

import Helpers.Helpers as Helpers
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage.Types exposing (StoredProject, StoredState)
import OpenStack.Types as OSTypes
import RemoteData
import Time
import Types.Types as Types


generateStoredState : Types.Model -> Encode.Value
generateStoredState model =
    let
        strippedProjects =
            List.map generateStoredProject model.projects
    in
    encodeStoredState { projects = strippedProjects }


generateStoredProject : Types.Project -> StoredProject
generateStoredProject project =
    { creds = project.creds
    , auth = project.auth
    }


hydrateModelFromStoredState : Types.Model -> StoredState -> Types.Model
hydrateModelFromStoredState model storedState =
    let
        projects =
            List.map hydrateProjectFromStoredProject storedState.projects

        viewState =
            case projects of
                [] ->
                    Types.NonProjectView Types.LoginPicker

                firstProject :: _ ->
                    Types.ProjectView (Helpers.getProjectId firstProject) Types.ListProjectServers
    in
    { model | projects = projects, viewState = viewState }


hydrateProjectFromStoredProject : StoredProject -> Types.Project
hydrateProjectFromStoredProject storedProject =
    { creds = storedProject.creds
    , auth = storedProject.auth
    , endpoints = Helpers.serviceCatalogToEndpoints storedProject.auth.catalog
    , images = []
    , servers = RemoteData.NotAsked
    , flavors = []
    , keypairs = []
    , volumes = RemoteData.NotAsked
    , networks = []
    , floatingIps = []
    , ports = []
    , securityGroups = []
    , pendingCredentialedRequests = []
    }



-- Encoders


encodeStoredState : StoredState -> Encode.Value
encodeStoredState storedState =
    let
        storedProjectEncode : StoredProject -> Encode.Value
        storedProjectEncode storedProject =
            Encode.object
                [ ( "creds", encodeCreds storedProject.creds )
                , ( "auth", encodeAuthToken storedProject.auth )
                ]
    in
    Encode.object
        [ ( "2"
          , Encode.object [ ( "projects", Encode.list storedProjectEncode storedState.projects ) ]
          )
        ]


encodeCreds : Types.OpenstackCreds -> Encode.Value
encodeCreds creds =
    Encode.object
        [ ( "authUrl", Encode.string creds.authUrl )
        , ( "projectDomain", Encode.string creds.projectDomain )
        , ( "projectName", Encode.string creds.projectName )
        , ( "userDomain", Encode.string creds.userDomain )
        , ( "username", Encode.string creds.username )
        , ( "password", Encode.string creds.password )
        ]


encodeAuthToken : OSTypes.AuthToken -> Encode.Value
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
        , ( "endpoints", Encode.list encodeEndpoint service.endpoints )
        ]


encodeEndpoint : OSTypes.Endpoint -> Encode.Value
encodeEndpoint endpoint =
    Encode.object
        [ ( "interface", encodeEndpointInterface endpoint.interface )
        , ( "url", Encode.string endpoint.url )
        ]


encodeEndpointInterface : OSTypes.EndpointInterface -> Encode.Value
encodeEndpointInterface endpointInterface =
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



-- Decoders


decodeStoredState : Decode.Decoder StoredState
decodeStoredState =
    Decode.map
        StoredState
        (Decode.oneOf
            -- Todo turn this into an actual migration
            [ Decode.at [ "0", "providers" ] (Decode.list storedProjectDecode1)
            , Decode.at [ "1", "projects" ] (Decode.list storedProjectDecode1)
            , Decode.at [ "2", "projects" ] (Decode.list storedProjectDecode)
            ]
        )


storedProjectDecode1 : Decode.Decoder StoredProject
storedProjectDecode1 =
    Decode.map2 StoredProject
        (Decode.field "creds" credsDecode)
        (Decode.field "auth" decodeStoredAuthTokenDetails1)


storedProjectDecode : Decode.Decoder StoredProject
storedProjectDecode =
    Decode.map2 StoredProject
        (Decode.field "creds" credsDecode)
        (Decode.field "auth" decodeStoredAuthTokenDetails)


credsDecode : Decode.Decoder Types.OpenstackCreds
credsDecode =
    Decode.map6 Types.OpenstackCreds
        (Decode.field "authUrl" Decode.string)
        (Decode.field "projectDomain" Decode.string)
        (Decode.field "projectName" Decode.string)
        (Decode.field "userDomain" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "password" Decode.string)


decodeStoredAuthTokenDetails1 : Decode.Decoder OSTypes.AuthToken
decodeStoredAuthTokenDetails1 =
    Decode.map7 OSTypes.AuthToken
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


decodeStoredAuthTokenDetails : Decode.Decoder OSTypes.AuthToken
decodeStoredAuthTokenDetails =
    Decode.map7 OSTypes.AuthToken
        (Decode.field "catalog" (Decode.list openstackStoredServiceDecoder))
        (Decode.field "project" decodeNameAndId)
        (Decode.field "projectDomain" decodeNameAndId)
        (Decode.field "user" decodeNameAndId)
        (Decode.field "userDomain" decodeNameAndId)
        (Decode.field "expiresAt" Decode.int
            |> Decode.map Time.millisToPosix
        )
        (Decode.field "tokenValue" Decode.string)


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
