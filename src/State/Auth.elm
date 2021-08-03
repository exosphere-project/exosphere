module State.Auth exposing
    ( authUrlWithPortAndVersion
    , jetstreamToOpenstackCreds
    , processOpenRc
    , projectUpdateAuthToken
    , requestAuthToken
    , unscopedProviderUpdateAuthToken
    )

import Helpers.GetterSetters as GetterSetters
import OpenStack.Types as OSTypes
import Parser exposing ((|.), (|=))
import Rest.Keystone
import Set
import Types.HelperTypes as HelperTypes exposing (HttpRequestMethod(..), UnscopedProvider)
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..))
import Types.Project exposing (Project, ProjectSecret(..))
import Types.Types exposing (Model)
import Types.View
    exposing
        ( JetstreamCreds
        , JetstreamProvider(..)
        , NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import Url


projectUpdateAuthToken : Model -> Project -> OSTypes.ScopedAuthToken -> ( Model, Cmd Msg )
projectUpdateAuthToken model project authToken =
    -- Update auth token for existing project
    let
        newProject =
            { project | auth = authToken }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    sendPendingRequests newModel newProject


unscopedProviderUpdateAuthToken : Model -> UnscopedProvider -> OSTypes.UnscopedAuthToken -> ( Model, Cmd Msg )
unscopedProviderUpdateAuthToken model provider authToken =
    let
        newProvider =
            { provider | token = authToken }

        newModel =
            GetterSetters.modelUpdateUnscopedProvider model newProvider
    in
    ( newModel, Cmd.none )


sendPendingRequests : Model -> Project -> ( Model, Cmd Msg )
sendPendingRequests model project =
    -- Fires any pending commands which were waiting for auth token renewal
    -- This function assumes our token is valid (does not check for expiry).
    let
        -- Hydrate cmds with auth token
        cmds =
            List.map (\pqr -> pqr project.auth.tokenValue) project.pendingCredentialedRequests

        -- Clear out pendingCredentialedRequests
        newProject =
            { project | pendingCredentialedRequests = [] }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.batch cmds )


requestAuthToken : Model -> Project -> Result String (Cmd Msg)
requestAuthToken model project =
    -- Wraps Rest.Keystone.RequestAuthToken
    case project.secret of
        NoProjectSecret ->
            Err <|
                "Exosphere could not find a usable authentication method for project "
                    ++ project.auth.project.name
                    ++ "."

        ApplicationCredential appCred ->
            Ok <|
                Rest.Keystone.requestScopedAuthToken model.cloudCorsProxyUrl <|
                    OSTypes.AppCreds project.endpoints.keystone project.auth.project.name appCred


jetstreamToOpenstackCreds : JetstreamCreds -> List OSTypes.OpenstackLogin
jetstreamToOpenstackCreds jetstreamCreds =
    let
        authUrlBases =
            case jetstreamCreds.jetstreamProviderChoice of
                {- TODO should we hard-code these elsewhere? -}
                IUCloud ->
                    [ "iu.jetstream-cloud.org" ]

                TACCCloud ->
                    [ "tacc.jetstream-cloud.org" ]

                BothJetstreamClouds ->
                    [ "iu.jetstream-cloud.org"
                    , "tacc.jetstream-cloud.org"
                    ]

        authUrls =
            List.map
                (\baseUrl -> "https://" ++ baseUrl ++ ":5000/v3/auth/tokens")
                authUrlBases
    in
    List.map
        (\authUrl ->
            OSTypes.OpenstackLogin
                authUrl
                "tacc"
                jetstreamCreds.taccUsername
                jetstreamCreds.taccPassword
        )
        authUrls


processOpenRc : OSTypes.OpenstackLogin -> String -> OSTypes.OpenstackLogin
processOpenRc existingCreds openRc =
    let
        parseVar : String -> Maybe String
        parseVar varName =
            let
                parseOptionalDoubleQuote =
                    -- Why does this need to be Parser.succeed () instead of Parser.succeed identity?
                    Parser.oneOf [ Parser.symbol "\"", Parser.succeed () ]

                varParser : Parser.Parser String
                varParser =
                    -- Why does this need to be Parser.succeed identity instead of Parser.succeed ()?
                    Parser.succeed identity
                        |. Parser.spaces
                        |. Parser.oneOf [ Parser.keyword "export", Parser.succeed () ]
                        |. Parser.spaces
                        |. Parser.keyword varName
                        |. Parser.symbol "="
                        |. parseOptionalDoubleQuote
                        |= Parser.variable
                            -- This discards any bash variables defined with other bash variables, e.g. $OS_PASSWORD_INPUT
                            { start = \c -> c /= '$'
                            , inner = \c -> not (List.member c [ '\n', '"' ])
                            , reserved = Set.empty
                            }
                        |. parseOptionalDoubleQuote
                        |. Parser.oneOf [ Parser.symbol "\n", Parser.end ]
            in
            openRc
                |> String.split "\n"
                |> List.map (\line -> Parser.run varParser line)
                |> List.map Result.toMaybe
                |> List.filterMap identity
                |> List.head
    in
    OSTypes.OpenstackLogin
        (parseVar "OS_AUTH_URL" |> Maybe.withDefault existingCreds.authUrl)
        (parseVar "OS_USER_DOMAIN_NAME"
            |> Maybe.withDefault
                (parseVar "OS_USER_DOMAIN_ID"
                    |> Maybe.withDefault existingCreds.userDomain
                )
        )
        (parseVar "OS_USERNAME" |> Maybe.withDefault existingCreds.username)
        (parseVar "OS_PASSWORD" |> Maybe.withDefault existingCreds.password)


authUrlWithPortAndVersion : HelperTypes.Url -> HelperTypes.Url
authUrlWithPortAndVersion authUrlStr =
    -- If user does not provide a port and path in OpenStack auth URL then we guess port 5000 and path "/v3"
    let
        authUrlStrWithProto =
            -- If user doesn't provide a protocol then we add one so that the URL will actually parse
            if String.startsWith "http://" authUrlStr || String.startsWith "https://" authUrlStr then
                authUrlStr

            else
                "https://" ++ authUrlStr

        maybeAuthUrl =
            Url.fromString authUrlStrWithProto
    in
    case maybeAuthUrl of
        Nothing ->
            -- We can't parse this URL so we just return it unmodified
            authUrlStr

        Just authUrl ->
            let
                port_ =
                    case authUrl.port_ of
                        Just _ ->
                            authUrl.port_

                        Nothing ->
                            Just 5000

                path =
                    case authUrl.path of
                        "" ->
                            "/v3"

                        "/" ->
                            "/v3"

                        _ ->
                            authUrl.path
            in
            Url.toString <|
                Url.Url
                    authUrl.protocol
                    authUrl.host
                    port_
                    path
                    -- Query and fragment may not be needed / accepted by OpenStack
                    authUrl.query
                    authUrl.fragment
