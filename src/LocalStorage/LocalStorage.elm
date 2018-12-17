module LocalStorage.LocalStorage exposing
    ( decodeStoredState
    , generateStoredState
    , hydrateModelFromStoredState
    )

import Helpers.Helpers as Helpers
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage.Types exposing (..)
import RemoteData
import Time
import OpenStack.Types as OSTypes
import Types.Types as Types


generateStoredState : Types.Model -> Encode.Value
generateStoredState model =
    let
        strippedProviders =
            List.map generateStoredProvider model.providers
    in
    encodeStoredState { providers = strippedProviders }


generateStoredProvider : Types.Provider -> StoredProvider
generateStoredProvider provider =
    { name = provider.name
    , creds = provider.creds
    , auth = provider.auth
    }


hydrateModelFromStoredState : Types.Model -> StoredState -> Types.Model
hydrateModelFromStoredState model storedState =
    let
        providers =
            List.map hydrateProviderFromStoredProvider storedState.providers

        viewState =
            case providers of
                [] ->
                    Types.NonProviderView Types.Login

                firstProvider :: _ ->
                    Types.ProviderView firstProvider.name Types.ListProviderServers
    in
    { model | providers = providers, viewState = viewState }


hydrateProviderFromStoredProvider : StoredProvider -> Types.Provider
hydrateProviderFromStoredProvider storedProvider =
    { name = storedProvider.name
    , creds = storedProvider.creds
    , auth = storedProvider.auth
    , endpoints = Helpers.serviceCatalogToEndpoints storedProvider.auth.catalog
    , images = []
    , servers = RemoteData.NotAsked
    , flavors = []
    , keypairs = []
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
        storedProviderEncode : StoredProvider -> Encode.Value
        storedProviderEncode storedProvider =
            Encode.object
                [ ( "name", Encode.string storedProvider.name )
                , ( "creds", encodeCreds storedProvider.creds )
                , ( "auth", encodeAuthToken storedProvider.auth )
                ]
    in
    Encode.object
        [ ( "0"
          , Encode.object [ ( "providers", Encode.list storedProviderEncode storedState.providers ) ]
          )
        ]


encodeCreds : Types.Creds -> Encode.Value
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
        , ( "projectUuid", Encode.string authToken.projectUuid )
        , ( "projectName", Encode.string authToken.projectName )
        , ( "userUuid", Encode.string authToken.userUuid )
        , ( "username", Encode.string authToken.userName )
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
    Decode.map StoredState (Decode.at [ "0", "providers" ] (Decode.list storedProviderDecode))


storedProviderDecode : Decode.Decoder StoredProvider
storedProviderDecode =
    Decode.map3 StoredProvider
        (Decode.field "name" Decode.string)
        (Decode.field "creds" credsDecode)
        (Decode.field "auth" decodeStoredAuthTokenDetails)


credsDecode : Decode.Decoder Types.Creds
credsDecode =
    Decode.map6 Types.Creds
        (Decode.field "authUrl" Decode.string)
        (Decode.field "projectDomain" Decode.string)
        (Decode.field "projectName" Decode.string)
        (Decode.field "userDomain" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "password" Decode.string)


decodeStoredAuthTokenDetails : Decode.Decoder OSTypes.AuthToken
decodeStoredAuthTokenDetails =
    Decode.map7 OSTypes.AuthToken
        (Decode.field "catalog" (Decode.list openstackStoredServiceDecoder))
        (Decode.field "projectUuid" Decode.string)
        (Decode.field "projectName" Decode.string)
        (Decode.field "userUuid" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "expiresAt" Decode.int
            |> Decode.map Time.millisToPosix
        )
        (Decode.field "tokenValue" Decode.string)


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
