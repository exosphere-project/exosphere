module LocalStorage exposing
    ( decodeStoredState
    , generateStoredState
    , hydrateModelFromStoredState
    )

import Helpers.Helpers as Helpers
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData
import Types.OpenstackTypes as OSTypes
import Types.Types as Types


decodeStoredAuthTokenDetails : Decode.Decoder OSTypes.AuthToken
decodeStoredAuthTokenDetails =
    let
        iso8601StringToPosixDecodeError str =
            case Helpers.iso8601StringToPosix str of
                Ok posix ->
                    Decode.succeed posix

                Err error ->
                    Decode.fail error
    in
    Decode.map7 OSTypes.AuthToken
        (Decode.field "catalog" (Decode.list openstackStoredServiceDecoder))
        (Decode.field "projectUuid" Decode.string)
        (Decode.field "projectName" Decode.string)
        (Decode.field "userUuid" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "expiresAt" Decode.string
            |> Decode.andThen iso8601StringToPosixDecodeError
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


generateStoredProvider : Types.Provider -> Types.StoredProvider
generateStoredProvider provider =
    { name = provider.name
    , creds = provider.creds
    , auth = provider.auth
    }


decodeStoredState : Decode.Decoder Types.StoredState
decodeStoredState =
    Decode.map Types.StoredState (Decode.field "providers" (Decode.list storedProviderDecode))


storedProviderDecode : Decode.Decoder Types.StoredProvider
storedProviderDecode =
    Decode.map3 Types.StoredProvider
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


encodeEndpoint : OSTypes.Endpoint -> Encode.Value
encodeEndpoint endpoint =
    Encode.object
        [ ( "interface", encodeEndpointInterface endpoint.interface )
        , ( "url", Encode.string endpoint.url )
        ]


encodeService : OSTypes.Service -> Encode.Value
encodeService service =
    Encode.object
        [ ( "name", Encode.string service.name )
        , ( "type_", Encode.string service.type_ )
        , ( "endpoints", Encode.list encodeEndpoint service.endpoints )
        ]


encodeCatalog : OSTypes.ServiceCatalog -> Encode.Value
encodeCatalog serviceCatalog =
    Encode.list encodeService serviceCatalog


encodeAuthToken : OSTypes.AuthToken -> Encode.Value
encodeAuthToken authToken =
    Encode.object
        [ ( "catalog", encodeCatalog authToken.catalog )
        , ( "projectUuid", Encode.string authToken.projectUuid )
        , ( "projectName", Encode.string authToken.projectName )
        , ( "userUuid", Encode.string authToken.userUuid )
        , ( "username", Encode.string authToken.userName )
        , ( "expiresAt", Encode.string (Helpers.posixToIso8601String authToken.expiresAt) )
        , ( "tokenValue", Encode.string authToken.tokenValue )
        ]


encodeStoredState : Types.StoredState -> Encode.Value
encodeStoredState storedState =
    let
        storedProviderEncode : Types.StoredProvider -> Encode.Value
        storedProviderEncode storedProvider =
            Encode.object
                [ ( "name", Encode.string storedProvider.name )
                , ( "creds", encodeCreds storedProvider.creds )
                , ( "auth", encodeAuthToken storedProvider.auth )
                ]
    in
    Encode.object
        [ ( "providers", Encode.list storedProviderEncode storedState.providers )
        ]


generateStoredState : Types.Model -> Encode.Value
generateStoredState model =
    let
        strippedProviders =
            List.map generateStoredProvider model.providers
    in
    encodeStoredState { providers = strippedProviders }


hydrateProviderFromStoredProvider : Types.StoredProvider -> Types.Provider
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


hydrateModelFromStoredState : Types.Model -> Types.StoredState -> Types.Model
hydrateModelFromStoredState model storedState =
    let
        modelWithProviders =
            { model | providers = List.map hydrateProviderFromStoredProvider storedState.providers }

        viewState =
            case modelWithProviders.providers of
                [] ->
                    Types.NonProviderView Types.Login

                firstProvider :: _ ->
                    Types.ProviderView firstProvider.name Types.ListProviderServers

        modelWithViewState =
            { modelWithProviders | viewState = viewState }
    in
    modelWithViewState
