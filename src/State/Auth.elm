module State.Auth exposing
    ( authUrlWithPortAndVersion
    , jetstreamToOpenstackCreds
    , projectUpdateAuthToken
    , requestAuthToken
    , unscopedProviderUpdateAuthToken
    )

import Helpers.GetterSetters as GetterSetters
import OpenStack.Types as OSTypes
import Rest.Keystone
import Types.HelperTypes as HelperTypes
    exposing
        ( HttpRequestMethod(..)
        , JetstreamCreds
        , JetstreamProvider(..)
        , UnscopedProvider
        )
import Types.OuterModel exposing (OuterModel)
import Types.Project exposing (Project, ProjectSecret(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url


projectUpdateAuthToken : OuterModel -> Project -> OSTypes.ScopedAuthToken -> ( OuterModel, Cmd SharedMsg )
projectUpdateAuthToken outerModel project authToken =
    -- Update auth token for existing project
    let
        newProject =
            { project | auth = authToken }

        newSharedModel =
            GetterSetters.modelUpdateProject outerModel.sharedModel newProject

        newOuterModel =
            { outerModel | sharedModel = newSharedModel }
    in
    sendPendingRequests newOuterModel newProject


unscopedProviderUpdateAuthToken : SharedModel -> UnscopedProvider -> OSTypes.UnscopedAuthToken -> ( SharedModel, Cmd SharedMsg )
unscopedProviderUpdateAuthToken model provider authToken =
    let
        newProvider =
            { provider | token = authToken }

        newModel =
            GetterSetters.modelUpdateUnscopedProvider model newProvider
    in
    ( newModel, Cmd.none )


sendPendingRequests : OuterModel -> Project -> ( OuterModel, Cmd SharedMsg )
sendPendingRequests outerModel project =
    -- Fires any pending commands which were waiting for auth token renewal
    -- This function assumes our token is valid (does not check for expiry).
    let
        pendingRequestsForProject =
            outerModel.pendingCredentialedRequests
                |> List.filter (\pr -> Tuple.first pr == project.auth.project.uuid)
                |> List.map Tuple.second

        pendingRequestsForOtherProjects =
            outerModel.pendingCredentialedRequests
                |> List.filter (\pr -> Tuple.first pr /= project.auth.project.uuid)

        -- Hydrate cmds with auth token
        cmds =
            List.map (\pqr -> pqr project.auth.tokenValue) pendingRequestsForProject

        -- Clear out pendingCredentialedRequests for this project
        newModel =
            { outerModel | pendingCredentialedRequests = pendingRequestsForOtherProjects }
    in
    ( newModel, Cmd.batch cmds )


requestAuthToken : SharedModel -> Project -> Result String (Cmd SharedMsg)
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
