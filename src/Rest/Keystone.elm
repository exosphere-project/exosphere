module Rest.Keystone exposing
    ( decodeScopedAuthToken
    , decodeUnscopedAuthToken
    , requestAppCredential
    , requestScopedAuthToken
    , requestUnscopedAuthToken
    , requestUnscopedProjects
    , requestUnscopedRegions
    )

import Dict
import Helpers.GetterSetters as GetterSetters
import Helpers.Url
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , idOrName
        , iso8601StringToPosixDecodeError
        , keystoneUrlWithVersion
        , openstackCredentialedRequest
        , proxyifyRequest
        , resultToMsgErrorBody
        )
import Time
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.HelperTypes as HelperTypes exposing (HttpRequestMethod(..), UnscopedProvider, UnscopedProviderProject, UnscopedProviderRegion)
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import UUID
import Url



{- HTTP Requests -}


requestUnscopedAuthToken : Maybe HelperTypes.Url -> OSTypes.OpenstackLogin -> Cmd SharedMsg
requestUnscopedAuthToken maybeProxyUrl creds =
    let
        requestBody =
            Encode.object
                [ ( "auth"
                  , Encode.object
                        [ ( "identity"
                          , Encode.object
                                [ ( "methods", Encode.list Encode.string [ "password" ] )
                                , ( "password"
                                  , Encode.object
                                        [ ( "user"
                                          , Encode.object
                                                [ ( "name", Encode.string creds.username )
                                                , ( "domain"
                                                  , Encode.object
                                                        [ ( idOrName creds.userDomain, Encode.string creds.userDomain )
                                                        ]
                                                  )
                                                , ( "password", Encode.string creds.password )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                "log into OpenStack"
                ErrorCrit
                (Just "Make sure your login credentials including password are correct!")
    in
    requestAuthTokenHelper
        requestBody
        creds.authUrl
        maybeProxyUrl
        (resultToMsgErrorBody errorContext (ReceiveUnscopedAuthToken creds.authUrl))


requestScopedAuthToken : Maybe HelperTypes.Url -> OSTypes.CredentialsForAuthToken -> Cmd SharedMsg
requestScopedAuthToken maybeProxyUrl input =
    let
        requestBody =
            case input of
                OSTypes.AppCreds _ _ appCred ->
                    Encode.object
                        [ ( "auth"
                          , Encode.object
                                [ ( "identity"
                                  , Encode.object
                                        [ ( "methods", Encode.list Encode.string [ "application_credential" ] )
                                        , ( "application_credential"
                                          , Encode.object
                                                [ ( "id", Encode.string appCred.uuid )
                                                , ( "secret", Encode.string appCred.secret )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]

                OSTypes.TokenCreds _ token projectId ->
                    Encode.object
                        [ ( "auth"
                          , Encode.object
                                [ ( "identity"
                                  , Encode.object
                                        [ ( "methods", Encode.list Encode.string [ "token" ] )
                                        , ( "token"
                                          , Encode.object
                                                [ ( "id"
                                                  , Encode.string token.tokenValue
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "scope"
                                  , Encode.object
                                        [ ( "project"
                                          , Encode.object
                                                [ ( "id", Encode.string projectId )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]

        inputUrl =
            case input of
                OSTypes.TokenCreds url _ _ ->
                    url

                OSTypes.AppCreds url _ _ ->
                    url

        errorContext =
            let
                projectLabel =
                    case input of
                        OSTypes.AppCreds _ projectName _ ->
                            projectName

                        OSTypes.TokenCreds _ _ projectId ->
                            projectId
            in
            ErrorContext
                ("log into OpenStack project \"" ++ projectLabel ++ "\"")
                ErrorCrit
                (Just "Check with your cloud administrator to ensure you have access to this project.")
    in
    requestAuthTokenHelper
        requestBody
        inputUrl
        maybeProxyUrl
        (resultToMsgErrorBody errorContext (ReceiveProjectScopedToken inputUrl))


requestAuthTokenHelper : Encode.Value -> HelperTypes.Url -> Maybe HelperTypes.Url -> (Result HttpErrorWithBody ( Http.Metadata, String ) -> SharedMsg) -> Cmd SharedMsg
requestAuthTokenHelper requestBody authUrl maybeProxyUrl resultMsg =
    let
        correctedUrl =
            let
                maybeUrl =
                    Url.fromString authUrl
            in
            case maybeUrl of
                -- Cannot parse URL, so uh, don't make changes to it. We should never be here
                Nothing ->
                    authUrl

                Just url_ ->
                    { url_ | path = "/v3/auth/tokens" } |> Url.toString

        ( finalUrl, headers ) =
            case maybeProxyUrl of
                Nothing ->
                    ( correctedUrl, [] )

                Just proxyUrl ->
                    proxyifyRequest proxyUrl correctedUrl
    in
    {- https://stackoverflow.com/questions/44368340/get-request-headers-from-http-request -}
    Http.request
        { method = "POST"
        , headers = headers
        , url = finalUrl
        , body = Http.jsonBody requestBody

        {- Todo handle no response? -}
        , expect =
            Http.expectStringResponse
                resultMsg
                (\response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Err <| HttpErrorWithBody (Http.BadUrl url_) ""

                        Http.Timeout_ ->
                            Err <| HttpErrorWithBody Http.Timeout ""

                        Http.NetworkError_ ->
                            Err <| HttpErrorWithBody Http.NetworkError ""

                        Http.BadStatus_ metadata body ->
                            Err <| HttpErrorWithBody (Http.BadStatus metadata.statusCode) body

                        Http.GoodStatus_ metadata body ->
                            Ok ( metadata, body )
                )
        , timeout = Nothing
        , tracker = Nothing
        }


requestAppCredential : UUID.UUID -> Time.Posix -> Project -> Cmd SharedMsg
requestAppCredential clientUuid posixTime project =
    let
        appCredentialName =
            String.concat
                [ "exosphere-"
                , UUID.toString clientUuid
                , "-"
                , project.auth.project.name
                , "-"
                , String.fromInt <| Time.posixToMillis posixTime
                ]

        requestBody =
            Encode.object
                [ ( "application_credential"
                  , Encode.object
                        [ ( "name", Encode.string appCredentialName )
                        ]
                  )
                ]

        urlWithVersion =
            keystoneUrlWithVersion project.endpoints.keystone

        errorContext =
            ErrorContext
                ("request application credential for project named \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                (Just "Perhaps you are trying to use a cloud that is too old to support Application Credentials? Exosphere supports OpenStack Queens release and newer. Check with your cloud administrator if you are unsure.")

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\appCred ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveAppCredential appCred)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        (urlWithVersion ++ "/users/" ++ project.auth.user.uuid ++ "/application_credentials")
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody resultToMsg_ decodeAppCredential)


requestUnscopedProjects : UnscopedProvider -> Maybe HelperTypes.Url -> Cmd SharedMsg
requestUnscopedProjects provider maybeProxyUrl =
    requestUnscoped_ provider maybeProxyUrl "auth/projects" "projects" decodeUnscopedProjects ReceiveUnscopedProjects


requestUnscopedRegions : UnscopedProvider -> Maybe HelperTypes.Url -> Cmd SharedMsg
requestUnscopedRegions provider maybeProxyUrl =
    requestUnscoped_ provider maybeProxyUrl "regions" "regions" decodeUnscopedRegions ReceiveUnscopedRegions


requestUnscoped_ : UnscopedProvider -> Maybe HelperTypes.Url -> String -> String -> Decode.Decoder a -> (OSTypes.KeystoneUrl -> a -> SharedMsg) -> Cmd SharedMsg
requestUnscoped_ provider maybeProxyUrl resourcePathFragment resourceStr decoder toSharedMsg =
    let
        correctedUrl =
            let
                maybeUrl =
                    Url.fromString provider.authUrl
            in
            case maybeUrl of
                -- Cannot parse URL, so uh, don't make changes to it. We should never be here
                Nothing ->
                    provider.authUrl

                Just url_ ->
                    { url_ | path = "/v3/" ++ resourcePathFragment } |> Url.toString

        ( url, headers ) =
            case maybeProxyUrl of
                Just proxyUrl ->
                    proxyifyRequest proxyUrl correctedUrl

                Nothing ->
                    ( correctedUrl, [] )

        errorContext =
            ErrorContext
                ("get a list of "
                    ++ resourceStr
                    ++ " for provider \""
                    ++ Helpers.Url.hostnameFromUrl provider.authUrl
                    ++ "\""
                )
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (toSharedMsg provider.authUrl)
    in
    Http.request
        { method = "GET"
        , headers = Http.header "X-Auth-Token" provider.token.tokenValue :: headers
        , url = url
        , body = Http.emptyBody
        , expect =
            expectJsonWithErrorBody
                resultToMsg_
                decoder
        , timeout = Nothing
        , tracker = Nothing
        }



{- JSON Decoders -}


decodeUnscopedAuthToken : Http.Response String -> Result String OSTypes.UnscopedAuthToken
decodeUnscopedAuthToken response =
    decodeAuthTokenHelper response decodeUnscopedAuthTokenDetails


decodeUnscopedAuthTokenDetails : Decode.Decoder (OSTypes.AuthTokenString -> OSTypes.UnscopedAuthToken)
decodeUnscopedAuthTokenDetails =
    Decode.map OSTypes.UnscopedAuthToken
        (Decode.at [ "token", "expires_at" ] Decode.string
            |> Decode.andThen iso8601StringToPosixDecodeError
        )


decodeScopedAuthToken : Http.Response String -> Result String OSTypes.ScopedAuthToken
decodeScopedAuthToken response =
    decodeAuthTokenHelper response decodeScopedAuthTokenDetails


decodeScopedAuthTokenDetails : Decode.Decoder (OSTypes.AuthTokenString -> OSTypes.ScopedAuthToken)
decodeScopedAuthTokenDetails =
    Decode.map6 OSTypes.ScopedAuthToken
        (Decode.at [ "token", "catalog" ] (Decode.list openstackServiceDecoder))
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "project", "name" ] Decode.string)
            (Decode.at [ "token", "project", "id" ] Decode.string)
        )
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "project", "domain", "name" ] Decode.string)
            (Decode.at [ "token", "project", "domain", "id" ] Decode.string)
        )
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "user", "name" ] Decode.string)
            (Decode.at [ "token", "user", "id" ] Decode.string)
        )
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "user", "domain", "name" ] Decode.string)
            (Decode.at [ "token", "user", "domain", "id" ] Decode.string)
        )
        (Decode.at [ "token", "expires_at" ] Decode.string
            |> Decode.andThen iso8601StringToPosixDecodeError
        )


openstackServiceDecoder : Decode.Decoder OSTypes.Service
openstackServiceDecoder =
    Decode.map3 OSTypes.Service
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "endpoints" (Decode.list openstackEndpointDecoder))


openstackEndpointDecoder : Decode.Decoder OSTypes.Endpoint
openstackEndpointDecoder =
    Decode.map3 OSTypes.Endpoint
        (Decode.field "interface" Decode.string
            |> Decode.andThen openstackEndpointInterfaceDecoder
        )
        (Decode.field "url" Decode.string)
        (Decode.field "region_id" Decode.string)


openstackEndpointInterfaceDecoder : String -> Decode.Decoder OSTypes.EndpointInterface
openstackEndpointInterfaceDecoder interface =
    case interface of
        "public" ->
            Decode.succeed OSTypes.Public

        "admin" ->
            Decode.succeed OSTypes.Admin

        "internal" ->
            Decode.succeed OSTypes.Internal

        _ ->
            Decode.fail "unrecognized interface type"


decodeAuthTokenHelper : Http.Response String -> Decode.Decoder (OSTypes.AuthTokenString -> a) -> Result String a
decodeAuthTokenHelper response tokenDetailsDecoder =
    case response of
        Http.GoodStatus_ metadata body ->
            case Decode.decodeString tokenDetailsDecoder body of
                Ok tokenDetailsWithoutTokenString ->
                    case authTokenFromHeader metadata of
                        Ok authTokenString ->
                            Ok (tokenDetailsWithoutTokenString authTokenString)

                        Err errStr ->
                            Err errStr

                Err decodeError ->
                    Err (Decode.errorToString decodeError)

        Http.BadUrl_ url ->
            Err ("BadUrl: " ++ url)

        Http.Timeout_ ->
            Err "Timeout"

        Http.NetworkError_ ->
            Err "NetworkError"

        Http.BadStatus_ metadata body ->
            Err ("BadStatus: " ++ String.fromInt metadata.statusCode ++ " " ++ body)


authTokenFromHeader : Http.Metadata -> Result String String
authTokenFromHeader metadata =
    case Dict.get "X-Subject-Token" metadata.headers of
        Just token ->
            Ok token

        Nothing ->
            -- https://github.com/elm/http/issues/31
            case Dict.get "x-subject-token" metadata.headers of
                Just token2 ->
                    Ok token2

                Nothing ->
                    Err "Could not find an auth token in response headers"


decodeAppCredential : Decode.Decoder OSTypes.ApplicationCredential
decodeAppCredential =
    Decode.map2 OSTypes.ApplicationCredential
        (Decode.at [ "application_credential", "id" ] Decode.string)
        (Decode.at [ "application_credential", "secret" ] Decode.string)


decodeUnscopedProjects : Decode.Decoder (List UnscopedProviderProject)
decodeUnscopedProjects =
    Decode.field "projects" <|
        Decode.list unscopedProjectDecoder


unscopedProjectDecoder : Decode.Decoder UnscopedProviderProject
unscopedProjectDecoder =
    Decode.map4 UnscopedProviderProject
        (Decode.map2 OSTypes.NameAndUuid
            (Decode.field "name" Decode.string)
            (Decode.field "id" Decode.string)
        )
        (Decode.field "description" Decode.string |> Decode.nullable)
        (Decode.field "domain_id" Decode.string)
        (Decode.field "enabled" Decode.bool)


decodeUnscopedRegions : Decode.Decoder (List UnscopedProviderRegion)
decodeUnscopedRegions =
    Decode.field "regions" <|
        Decode.list unscopedRegionDecoder


unscopedRegionDecoder : Decode.Decoder UnscopedProviderRegion
unscopedRegionDecoder =
    Decode.map2 OSTypes.Region
        (Decode.field "id" Decode.string)
        (Decode.field "description" Decode.string)
