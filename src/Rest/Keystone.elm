module Rest.Keystone exposing
    ( decodeScopedAuthToken
    , decodeUnscopedAuthToken
    , ec2CredentialDecoder
    , ec2CredentialsDecoder
    , requestAppCredential
    , requestCreateEc2Credential
    , requestEc2Credentials
    , requestScopedAuthToken
    , requestUnscopedAuthToken
    , requestUnscopedProjects
    , requestUnscopedRegions
    )

import Dict
import Helpers.GetterSetters as GetterSetters
import Helpers.Json exposing (resultToDecoder)
import Helpers.Time exposing (makeIso8601StringToPosixDecoder)
import Helpers.Url
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , idOrName
        , keystoneUrlWithVersion
        , openstackCredentialedRequest
        , proxyifyRequest
        , resultToMsgErrorBody
        , resultToProjectMsgErrorBody
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
            let
                regionPart =
                    case project.region of
                        Just region ->
                            "-" ++ region.id

                        Nothing ->
                            ""
            in
            String.concat
                [ "exosphere-"
                , UUID.toString clientUuid
                , "-"
                , project.auth.project.name
                , regionPart
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
            resultToProjectMsgErrorBody
                (GetterSetters.projectIdentifier project)
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
        ( urlWithVersion, [ "users", project.auth.user.uuid, "application_credentials" ], [] )
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody resultToMsg_ appCredentialDecoder)


{-| GET the user's EC2/S3 credentials (Keystone OS-EC2 extension). The response spans all of the
user's projects; the `ReceiveEc2Credentials` handler filters to the current project's tenant before
caching. Mirrors `requestAppCredential`'s URL shape, but a GET with an empty body.
-}
requestEc2Credentials : Project -> Cmd SharedMsg
requestEc2Credentials project =
    let
        errorContext =
            ErrorContext
                ("get EC2 (S3) credentials for project named \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToProjectMsgErrorBody
                (GetterSetters.projectIdentifier project)
                errorContext
                (\creds ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveEc2Credentials errorContext (Ok creds))
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( keystoneUrlWithVersion project.endpoints.keystone, [ "users", project.auth.user.uuid, "credentials", "OS-EC2" ], [] )
        Http.emptyBody
        (expectJsonWithErrorBody resultToMsg_ ec2CredentialsDecoder)


{-| POST a new EC2/S3 credential for the current project (Keystone OS-EC2 extension). The body is a
TOP-LEVEL `{"tenant_id": <project uuid>}` (not wrapped in an outer key). On success the
`ReceiveCreateEc2Credential` handler re-fires the list request to refresh the cache.
-}
requestCreateEc2Credential : Project -> Cmd SharedMsg
requestCreateEc2Credential project =
    let
        requestBody =
            Encode.object
                [ ( "tenant_id", Encode.string project.auth.project.uuid ) ]

        errorContext =
            ErrorContext
                ("create an EC2 (S3) credential for project named \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToProjectMsgErrorBody
                (GetterSetters.projectIdentifier project)
                errorContext
                (\cred ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveCreateEc2Credential errorContext (Ok cred))
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        ( keystoneUrlWithVersion project.endpoints.keystone, [ "users", project.auth.user.uuid, "credentials", "OS-EC2" ], [] )
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody resultToMsg_ ec2CredentialDecoder)


requestUnscopedProjects : UnscopedProvider -> Maybe HelperTypes.Url -> Cmd SharedMsg
requestUnscopedProjects provider maybeProxyUrl =
    requestUnscoped_ provider maybeProxyUrl "auth/projects" "projects" unscopedProjectsDecoder ReceiveUnscopedProjects


requestUnscopedRegions : UnscopedProvider -> Maybe HelperTypes.Url -> Cmd SharedMsg
requestUnscopedRegions provider maybeProxyUrl =
    requestUnscoped_ provider maybeProxyUrl "regions" "regions" unscopedRegionsDecoder ReceiveUnscopedRegions


requestUnscoped_ : UnscopedProvider -> Maybe HelperTypes.Url -> String -> String -> Decode.Decoder a -> (OSTypes.KeystoneUrl -> ErrorContext -> Result HttpErrorWithBody a -> SharedMsg) -> Cmd SharedMsg
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

        resultToMsg result =
            toSharedMsg provider.authUrl errorContext result
    in
    Http.request
        { method = "GET"
        , headers = Http.header "X-Auth-Token" provider.token.tokenValue :: headers
        , url = url
        , body = Http.emptyBody
        , expect =
            expectJsonWithErrorBody
                resultToMsg
                decoder
        , timeout = Nothing
        , tracker = Nothing
        }



{- JSON Decoders -}
-- TODO


decodeUnscopedAuthToken : Http.Response String -> Result String OSTypes.UnscopedAuthToken
decodeUnscopedAuthToken response =
    decodeAuthTokenHelper response unscopedAuthTokenDetailsDecoder


unscopedAuthTokenDetailsDecoder : Decode.Decoder (OSTypes.AuthTokenString -> OSTypes.UnscopedAuthToken)
unscopedAuthTokenDetailsDecoder =
    Decode.map OSTypes.UnscopedAuthToken
        (Decode.at [ "token", "expires_at" ] Decode.string
            |> Decode.andThen makeIso8601StringToPosixDecoder
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
            |> Decode.andThen makeIso8601StringToPosixDecoder
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
            |> Decode.map parseOpenstackEndpointInterface
            |> Decode.andThen resultToDecoder
        )
        (Decode.field "url" Decode.string)
        (Decode.field "region_id" Decode.string)


parseOpenstackEndpointInterface : String -> Result String OSTypes.EndpointInterface
parseOpenstackEndpointInterface interface =
    case interface of
        "public" ->
            Result.Ok OSTypes.Public

        "admin" ->
            Result.Ok OSTypes.Admin

        "internal" ->
            Result.Ok OSTypes.Internal

        _ ->
            Result.Err "unrecognized interface type"


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


appCredentialDecoder : Decode.Decoder OSTypes.ApplicationCredential
appCredentialDecoder =
    Decode.map2 OSTypes.ApplicationCredential
        (Decode.at [ "application_credential", "id" ] Decode.string)
        (Decode.at [ "application_credential", "secret" ] Decode.string)


{-| A single OS-EC2 credential item: `access`/`secret`/`tenant_id` are TOP-LEVEL fields (the OS-EC2
extension shape, not the generic /v3/credentials `blob`). Extra fields (user\_id, trust\_id, links) are
ignored.
-}
ec2CredentialItemDecoder : Decode.Decoder OSTypes.Ec2Credential
ec2CredentialItemDecoder =
    Decode.map3 OSTypes.Ec2Credential
        (Decode.field "access" Decode.string)
        (Decode.field "secret" Decode.string)
        (Decode.field "tenant_id" Decode.string)


{-| The GET list response: `{"credentials": [ ... ]}`.
-}
ec2CredentialsDecoder : Decode.Decoder (List OSTypes.Ec2Credential)
ec2CredentialsDecoder =
    Decode.field "credentials" (Decode.list ec2CredentialItemDecoder)


{-| The POST create response: `{"credential": { ... }}`.
-}
ec2CredentialDecoder : Decode.Decoder OSTypes.Ec2Credential
ec2CredentialDecoder =
    Decode.field "credential" ec2CredentialItemDecoder


unscopedProjectsDecoder : Decode.Decoder (List UnscopedProviderProject)
unscopedProjectsDecoder =
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


unscopedRegionsDecoder : Decode.Decoder (List UnscopedProviderRegion)
unscopedRegionsDecoder =
    Decode.field "regions" <|
        Decode.list unscopedRegionDecoder


unscopedRegionDecoder : Decode.Decoder UnscopedProviderRegion
unscopedRegionDecoder =
    Decode.map2 OSTypes.Region
        (Decode.field "id" Decode.string)
        (Decode.field "description" Decode.string)
