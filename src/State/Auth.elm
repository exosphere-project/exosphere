module State.Auth exposing
    ( authUrlWithPortAndVersion
    , jetstreamToOpenstackCreds
    , processOpenRc
    , projectUpdateAuthToken
    , requestAuthToken
    , sendPendingRequests
    , unscopedProviderUpdateAuthToken
    )

import Helpers.Helpers as Helpers
import Maybe.Extra
import OpenStack.Types as OSTypes
import Regex
import Rest.Keystone
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , ExoSetupStatus(..)
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , JetstreamCreds
        , JetstreamProvider(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectSecret(..)
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ServerOrigin(..)
        , UnscopedProvider
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
            Helpers.modelUpdateProject model newProject
    in
    sendPendingRequests newModel newProject


unscopedProviderUpdateAuthToken : Model -> UnscopedProvider -> OSTypes.UnscopedAuthToken -> ( Model, Cmd Msg )
unscopedProviderUpdateAuthToken model provider authToken =
    let
        newProvider =
            { provider | token = authToken }

        newModel =
            Helpers.modelUpdateUnscopedProvider model newProvider
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
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.batch cmds )


requestAuthToken : Model -> Project -> Cmd Msg
requestAuthToken model project =
    -- Wraps Rest.RequestAuthToken, builds OSTypes.PasswordCreds if needed
    let
        creds =
            case project.secret of
                OpenstackPassword password ->
                    OSTypes.PasswordCreds <|
                        OSTypes.OpenstackLogin
                            project.endpoints.keystone
                            (if String.isEmpty project.auth.projectDomain.name then
                                project.auth.projectDomain.uuid

                             else
                                project.auth.projectDomain.name
                            )
                            project.auth.project.name
                            (if String.isEmpty project.auth.userDomain.name then
                                project.auth.userDomain.uuid

                             else
                                project.auth.userDomain.name
                            )
                            project.auth.user.name
                            password

                ApplicationCredential appCred ->
                    OSTypes.AppCreds project.endpoints.keystone project.auth.project.name appCred
    in
    Rest.Keystone.requestScopedAuthToken model.cloudCorsProxyUrl creds


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
                jetstreamCreds.jetstreamProjectName
                "tacc"
                jetstreamCreds.taccUsername
                jetstreamCreds.taccPassword
        )
        authUrls


processOpenRc : OSTypes.OpenstackLogin -> String -> OSTypes.OpenstackLogin
processOpenRc existingCreds openRc =
    let
        regexes =
            { authUrl = Helpers.alwaysRegex "export OS_AUTH_URL=\"?([^\"\n]*)\"?"
            , projectDomain = Helpers.alwaysRegex "export OS_PROJECT_DOMAIN(?:_NAME|_ID)=\"?([^\"\n]*)\"?"
            , projectName = Helpers.alwaysRegex "export OS_PROJECT_NAME=\"?([^\"\n]*)\"?"
            , userDomain = Helpers.alwaysRegex "export OS_USER_DOMAIN(?:_NAME|_ID)=\"?([^\"\n]*)\"?"
            , username = Helpers.alwaysRegex "export OS_USERNAME=\"?([^\"\n]*)\"?"
            , password = Helpers.alwaysRegex "export OS_PASSWORD=\"(.*)\""
            }

        getMatch text regex =
            Regex.findAtMost 1 regex text
                |> List.head
                |> Maybe.map (\x -> x.submatches)
                |> Maybe.andThen List.head
                |> Maybe.Extra.join

        newField regex oldField =
            getMatch openRc regex
                |> Maybe.withDefault oldField
    in
    OSTypes.OpenstackLogin
        (newField regexes.authUrl existingCreds.authUrl)
        (newField regexes.projectDomain existingCreds.projectDomain)
        (newField regexes.projectName existingCreds.projectName)
        (newField regexes.userDomain existingCreds.userDomain)
        (newField regexes.username existingCreds.username)
        (newField regexes.password existingCreds.password)


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
