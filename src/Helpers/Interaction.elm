module Helpers.Interaction exposing (interactionDetails, interactionStatus, interactionStatusWordColor)

import Element
import FeatherIcons as Icons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Icon as Icon
import Time
import Types.Guacamole as GuacTypes
import Types.HelperTypes exposing (UserAppProxyHostname)
import Types.Interaction as ITypes
import Types.Project exposing (Project)
import Types.Server exposing (Server, ServerOrigin(..))
import Types.Workflow exposing (ServerCustomWorkflowStatus(..))
import View.Types


interactionStatus : Project -> Server -> ITypes.Interaction -> View.Types.Context -> Time.Posix -> Maybe UserAppProxyHostname -> ITypes.InteractionStatus
interactionStatus project server interaction context currentTime tlsReverseProxyHostname =
    let
        maybeFloatingIpAddress =
            GetterSetters.getServerFloatingIps project server.osProps.uuid
                |> List.map .address
                |> List.head

        customWorkflowInteraction : ITypes.InteractionStatus
        customWorkflowInteraction =
            if context.experimentalFeaturesEnabled then
                customWorkflowInteractionExperimental

            else
                ITypes.Hidden

        customWorkflowInteractionExperimental : ITypes.InteractionStatus
        customWorkflowInteractionExperimental =
            case server.exoProps.serverOrigin of
                ServerNotFromExo ->
                    ITypes.Unavailable <|
                        String.join
                            " "
                            [ context.localization.virtualComputer
                                |> Helpers.String.toTitleCase
                            , "not launched from Exosphere"
                            ]

                ServerFromExo exoOriginProps ->
                    case exoOriginProps.customWorkflowStatus of
                        NotLaunchedWithCustomWorkflow ->
                            if exoOriginProps.exoServerVersion < 3 then
                                ITypes.Hidden

                            else
                                -- Deployed without a workflow
                                ITypes.Hidden

                        LaunchedWithCustomWorkflow customWorkflow ->
                            case customWorkflow.authToken.data of
                                RDPP.DoHave token _ ->
                                    case ( tlsReverseProxyHostname, maybeFloatingIpAddress ) of
                                        ( Just proxyHostname, Just floatingIp ) ->
                                            ITypes.Ready <|
                                                UrlHelpers.buildProxyUrl
                                                    proxyHostname
                                                    floatingIp
                                                    8888
                                                    (customWorkflow.source.path ++ "/?token=" ++ token)
                                                    False

                                        ( Nothing, _ ) ->
                                            ITypes.Unavailable "Cannot find TLS-terminating reverse proxy server"

                                        ( _, Nothing ) ->
                                            ITypes.Unavailable <|
                                                String.join " "
                                                    [ context.localization.virtualComputer
                                                        |> Helpers.String.toTitleCase
                                                    , "does not have a"
                                                    , context.localization.floatingIpAddress
                                                    ]

                                RDPP.DontHave ->
                                    let
                                        fortyMinMillis =
                                            1000 * 60 * 40

                                        newServer =
                                            Helpers.serverLessThanThisOld server currentTime fortyMinMillis

                                        recentServerEvent =
                                            server.events
                                                |> RDPP.withDefault []
                                                -- Ignore server events which don't cause a power cycle
                                                |> List.filter
                                                    (\event ->
                                                        [ "lock", "unlock", {- @nonlocalized -} "image" ]
                                                            |> List.map (\action -> action == event.action)
                                                            |> List.any identity
                                                            |> not
                                                    )
                                                -- Look for the most recent server event
                                                |> List.map .startTime
                                                |> List.map Time.posixToMillis
                                                |> List.sort
                                                |> List.reverse
                                                |> List.head
                                                -- See if most recent event is recent enough
                                                |> Maybe.map
                                                    (\eventTime ->
                                                        eventTime > (Time.posixToMillis currentTime - fortyMinMillis)
                                                    )
                                                |> Maybe.withDefault newServer
                                    in
                                    if recentServerEvent then
                                        ITypes.Unavailable <|
                                            String.join " "
                                                [ context.localization.virtualComputer
                                                    |> Helpers.String.toTitleCase
                                                , "is still booting or the workflow is still deploying, check back in a few minutes"
                                                ]

                                    else
                                        case ( tlsReverseProxyHostname, maybeFloatingIpAddress ) of
                                            ( Nothing, _ ) ->
                                                ITypes.Error "Cannot find TLS-terminating reverse proxy server"

                                            ( _, Nothing ) ->
                                                ITypes.Error <|
                                                    String.join " "
                                                        [ context.localization.virtualComputer
                                                            |> Helpers.String.toTitleCase
                                                        , "does not have a"
                                                        , context.localization.floatingIpAddress
                                                        ]

                                            ( Just _, Just _ ) ->
                                                case customWorkflow.authToken.refreshStatus of
                                                    RDPP.Loading ->
                                                        ITypes.Loading

                                                    RDPP.NotLoading maybeErrorTuple ->
                                                        -- If deployment is complete but we can't get a token, show error to user
                                                        case maybeErrorTuple of
                                                            Nothing ->
                                                                -- This is a slight misrepresentation; we haven't requested
                                                                -- a token yet but orchestration code will make request soon
                                                                ITypes.Loading

                                                            Just ( httpErrorWithBody, _ ) ->
                                                                ITypes.Error
                                                                    ("Exosphere tried to get the console log for the "
                                                                        ++ Helpers.String.toTitleCase context.localization.virtualComputer
                                                                        ++ " and received this error: "
                                                                        ++ Helpers.httpErrorWithBodyToString httpErrorWithBody
                                                                    )

        guac : GuacType -> ITypes.InteractionStatus
        guac guacType =
            let
                guacUpstreamPort =
                    49528

                fortyMinMillis =
                    1000 * 60 * 40

                newServer =
                    Helpers.serverLessThanThisOld server currentTime fortyMinMillis

                recentServerEvent =
                    server.events
                        |> RDPP.withDefault []
                        -- Ignore server events which don't cause a power cycle
                        |> List.filter
                            (\event ->
                                [ "lock", "unlock", {- @nonlocalized -} "image" ]
                                    |> List.map (\action -> action == event.action)
                                    |> List.any identity
                                    |> not
                            )
                        -- Look for the most recent server event
                        |> List.map .startTime
                        |> List.map Time.posixToMillis
                        |> List.sort
                        |> List.reverse
                        |> List.head
                        -- See if most recent event is recent enough
                        |> Maybe.map
                            (\eventTime ->
                                eventTime > (Time.posixToMillis currentTime - fortyMinMillis)
                            )
                        |> Maybe.withDefault newServer

                connectionStringBase64 =
                    -- Per https://sourceforge.net/p/guacamole/discussion/1110834/thread/fb609070/
                    case guacType of
                        Terminal ->
                            -- printf 'shell\0c\0default' | base64
                            "c2hlbGwAYwBkZWZhdWx0"

                        Desktop ->
                            -- printf 'desktop\0c\0default' | base64
                            "ZGVza3RvcABjAGRlZmF1bHQ="
            in
            case server.exoProps.serverOrigin of
                ServerNotFromExo ->
                    ITypes.Unavailable <|
                        String.join
                            " "
                            [ context.localization.virtualComputer
                                |> Helpers.String.toTitleCase
                            , "not launched from Exosphere"
                            ]

                ServerFromExo exoOriginProps ->
                    case exoOriginProps.guacamoleStatus of
                        GuacTypes.NotLaunchedWithGuacamole ->
                            if exoOriginProps.exoServerVersion < 3 then
                                ITypes.Unavailable <|
                                    String.join " "
                                        [ context.localization.virtualComputer
                                            |> Helpers.String.toTitleCase
                                        , "was created with an older version of Exosphere"
                                        ]

                            else
                                ITypes.Unavailable <|
                                    String.join " "
                                        [ context.localization.virtualComputer
                                            |> Helpers.String.toTitleCase
                                        , "was deployed with Guacamole support de-selected"
                                        ]

                        GuacTypes.LaunchedWithGuacamole guacProps ->
                            if not guacProps.vncSupported && (guacType == Desktop) then
                                ITypes.Unavailable <|
                                    String.join " "
                                        [ context.localization.graphicalDesktopEnvironment
                                            |> Helpers.String.toTitleCase
                                        , "was not enabled when"
                                        , context.localization.virtualComputer
                                            |> Helpers.String.toTitleCase
                                        , "was deployed"
                                        ]

                            else
                                case guacProps.authToken.data of
                                    RDPP.DoHave token _ ->
                                        case ( tlsReverseProxyHostname, maybeFloatingIpAddress ) of
                                            ( Just proxyHostname, Just floatingIp ) ->
                                                ITypes.Ready <|
                                                    UrlHelpers.buildProxyUrl
                                                        proxyHostname
                                                        floatingIp
                                                        guacUpstreamPort
                                                        ("/guacamole/#/client/" ++ connectionStringBase64 ++ "?token=" ++ token)
                                                        False

                                            ( Nothing, _ ) ->
                                                ITypes.Unavailable "Cannot find TLS-terminating reverse proxy server"

                                            ( _, Nothing ) ->
                                                ITypes.Unavailable <|
                                                    String.join " "
                                                        [ context.localization.virtualComputer
                                                            |> Helpers.String.toTitleCase
                                                        , "does not have a"
                                                        , context.localization.floatingIpAddress
                                                        ]

                                    RDPP.DontHave ->
                                        if recentServerEvent then
                                            ITypes.Unavailable <|
                                                String.join " "
                                                    [ context.localization.virtualComputer
                                                        |> Helpers.String.toTitleCase
                                                    , "is still booting or Guacamole is still deploying, check back in a few minutes"
                                                    ]

                                        else
                                            case
                                                ( tlsReverseProxyHostname
                                                , maybeFloatingIpAddress
                                                , GetterSetters.getServerExouserPassphrase server.osProps.details
                                                )
                                            of
                                                ( Nothing, _, _ ) ->
                                                    ITypes.Error "Cannot find TLS-terminating reverse proxy server"

                                                ( _, Nothing, _ ) ->
                                                    ITypes.Error <|
                                                        String.join " "
                                                            [ context.localization.virtualComputer
                                                                |> Helpers.String.toTitleCase
                                                            , "does not have a"
                                                            , context.localization.floatingIpAddress
                                                            ]

                                                ( _, _, Nothing ) ->
                                                    ITypes.Error <|
                                                        String.join " "
                                                            [ "Cannot find"
                                                            , context.localization.virtualComputer
                                                            , "passphrase to authenticate"
                                                            ]

                                                ( Just _, Just _, Just _ ) ->
                                                    case guacProps.authToken.refreshStatus of
                                                        RDPP.Loading ->
                                                            ITypes.Loading

                                                        RDPP.NotLoading maybeErrorTuple ->
                                                            -- If deployment is complete but we can't get a token, show error to user
                                                            case maybeErrorTuple of
                                                                Nothing ->
                                                                    -- This is a slight misrepresentation; we haven't requested
                                                                    -- a token yet but orchestration code will make request soon
                                                                    ITypes.Loading

                                                                Just ( httpError, _ ) ->
                                                                    ITypes.Error
                                                                        ("Exosphere tried to authenticate to the Guacamole API, and received this error: "
                                                                            ++ Helpers.httpErrorToString httpError
                                                                        )

        showInteraction =
            case interaction of
                ITypes.GuacTerminal ->
                    guac Terminal

                ITypes.GuacDesktop ->
                    guac Desktop

                ITypes.NativeSSH ->
                    case maybeFloatingIpAddress of
                        Nothing ->
                            ITypes.Unavailable <|
                                String.join " "
                                    [ context.localization.virtualComputer
                                        |> Helpers.String.toTitleCase
                                    , "does not have a"
                                    , context.localization.floatingIpAddress
                                    ]

                        Just floatingIp ->
                            ITypes.Ready <| "exouser@" ++ floatingIp

                ITypes.Console ->
                    case ( server.osProps.consoleUrl.data, server.osProps.consoleUrl.refreshStatus ) of
                        ( RDPP.DoHave consoleUrl _, _ ) ->
                            ITypes.Ready consoleUrl

                        ( _, RDPP.NotLoading Nothing ) ->
                            ITypes.Unavailable "Console URL is not queried yet"

                        ( _, RDPP.Loading ) ->
                            ITypes.Loading

                        ( _, RDPP.NotLoading (Just ( err, _ )) ) ->
                            ITypes.Error ("Exosphere requested a console URL and got the following error: " ++ Helpers.httpErrorToString err.error)

                ITypes.CustomWorkflow ->
                    customWorkflowInteraction
    in
    case server.osProps.details.openstackStatus of
        OSTypes.ServerBuild ->
            ITypes.Unavailable <|
                String.join " "
                    [ context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    , "is still building"
                    ]

        OSTypes.ServerActive ->
            showInteraction

        OSTypes.ServerVerifyResize ->
            showInteraction

        _ ->
            ITypes.Unavailable <|
                String.join " "
                    [ context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    , "is not active"
                    ]


interactionStatusWordColor : Style.Types.ExoPalette -> ITypes.InteractionStatus -> ( String, Element.Color )
interactionStatusWordColor palette status =
    case status of
        ITypes.Unavailable _ ->
            ( "Unavailable", SH.toElementColor palette.muted.default )

        ITypes.Loading ->
            ( "Loading", SH.toElementColor palette.info.default )

        ITypes.Ready _ ->
            ( "Ready", SH.toElementColor palette.success.default )

        ITypes.Warn _ _ ->
            ( "Warning", SH.toElementColor palette.warning.default )

        ITypes.Error _ ->
            ( "Error", SH.toElementColor palette.danger.default )

        ITypes.Hidden ->
            ( "Hidden", SH.toElementColor palette.muted.default )


interactionDetails : ITypes.Interaction -> View.Types.Context -> ITypes.InteractionDetails msg
interactionDetails interaction context =
    case interaction of
        ITypes.GuacTerminal ->
            ITypes.InteractionDetails
                (context.localization.commandDrivenTextInterface
                    |> Helpers.String.toTitleCase
                )
                (String.concat
                    [ "Get a "
                    , context.localization.commandDrivenTextInterface
                    , " session to your "
                    , context.localization.virtualComputer
                    , ". Pro tip, press Ctrl+Alt+Shift inside the "
                    , context.localization.commandDrivenTextInterface
                    , " window to show a graphical file upload/download tool!"
                    ]
                )
                (\_ _ -> Icon.sizedFeatherIcon 18 Icons.terminal)
                ITypes.UrlInteraction

        ITypes.GuacDesktop ->
            ITypes.InteractionDetails
                (Helpers.String.toTitleCase context.localization.graphicalDesktopEnvironment)
                (String.concat
                    [ "Interact with your "
                    , context.localization.virtualComputer
                    , "'s desktop environment"
                    ]
                )
                (\_ _ -> Icon.sizedFeatherIcon 18 Icons.monitor)
                ITypes.UrlInteraction

        ITypes.NativeSSH ->
            ITypes.InteractionDetails
                "Native SSH"
                "Advanced feature: use your computer's native SSH client to get a command-line session with extra capabilities"
                (\_ _ -> Icon.sizedFeatherIcon 18 Icons.terminal)
                ITypes.TextInteraction

        ITypes.Console ->
            ITypes.InteractionDetails
                "Console"
                (String.join " "
                    [ "Advanced feature: Launching the console is like connecting a screen, mouse, and keyboard to your"
                    , context.localization.virtualComputer
                    , "(useful for troubleshooting if the Web " ++ Helpers.String.toTitleCase context.localization.commandDrivenTextInterface ++ " isn't working)"
                    ]
                )
                Icon.console
                ITypes.UrlInteraction

        ITypes.CustomWorkflow ->
            ITypes.InteractionDetails
                "Workflow"
                (String.join " "
                    [ "Access the workflow launched with this"
                    , context.localization.virtualComputer
                    ]
                )
                (\_ _ -> Icon.sizedFeatherIcon 18 Icons.codesandbox)
                ITypes.UrlInteraction


type GuacType
    = Terminal
    | Desktop
