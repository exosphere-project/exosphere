module Helpers.Interaction exposing (interactionDetails, interactionStatus, interactionStatusWordColor)

import Element
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Icon as Icon
import Time
import Types.Guacamole as GuacTypes
import Types.Interaction as ITypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , Server
        , ServerOrigin(..)
        , UserAppProxyHostname
        )
import View.Types


interactionStatus : Server -> ITypes.Interaction -> View.Types.Context -> Time.Posix -> Maybe UserAppProxyHostname -> ITypes.InteractionStatus
interactionStatus server interaction context currentTime tlsReverseProxyHostname =
    let
        maybeFloatingIp =
            GetterSetters.getServerFloatingIp server.osProps.details.ipAddresses

        guacTerminal : ITypes.InteractionStatus
        guacTerminal =
            let
                guacUpstreamPort =
                    49528

                twentyMinMillis =
                    1000 * 60 * 20

                newServer =
                    Helpers.serverLessThanThisOld server currentTime twentyMinMillis

                recentServerEvent =
                    server.events
                        |> RemoteData.withDefault []
                        -- Ignore server events which don't cause a power cycle
                        |> List.filter
                            (\event ->
                                [ "lock", "unlock", "image" ]
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
                                eventTime > (Time.posixToMillis currentTime - twentyMinMillis)
                            )
                        |> Maybe.withDefault newServer
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
                            case guacProps.authToken.data of
                                RDPP.DoHave token _ ->
                                    case ( tlsReverseProxyHostname, maybeFloatingIp ) of
                                        ( Just proxyHostname, Just floatingIp ) ->
                                            ITypes.Ready <|
                                                UrlHelpers.buildProxyUrl
                                                    proxyHostname
                                                    floatingIp
                                                    guacUpstreamPort
                                                    ("/guacamole/#/client/c2hlbGwAYwBkZWZhdWx0?token=" ++ token)
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
                                            , maybeFloatingIp
                                            , GetterSetters.getServerExouserPassword server.osProps.details
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
                                                        , "password to authenticate"
                                                        ]

                                            ( Just _, Just _, Just _ ) ->
                                                case guacProps.authToken.refreshStatus of
                                                    RDPP.Loading _ ->
                                                        ITypes.Loading

                                                    RDPP.NotLoading maybeErrorTuple ->
                                                        -- If deployment is complete but we can't get a token, show error to user
                                                        case maybeErrorTuple of
                                                            Nothing ->
                                                                -- This is a slight misrepresentation; we haven't requested
                                                                -- a token yet but orchestration code will make request soon
                                                                ITypes.Loading

                                                            Just ( error, _ ) ->
                                                                ITypes.Error
                                                                    ("Exosphere tried to authenticate to the Guacamole API, and received this error: "
                                                                        ++ Debug.toString error
                                                                    )

        cockpit : CockpitDashboardOrTerminal -> ITypes.InteractionStatus
        cockpit dashboardOrTerminal =
            ITypes.Hidden
    in
    case server.osProps.details.openstackStatus of
        OSTypes.ServerBuilding ->
            ITypes.Unavailable <|
                String.join " "
                    [ context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    , "is still building"
                    ]

        OSTypes.ServerActive ->
            case interaction of
                ITypes.GuacTerminal ->
                    guacTerminal

                ITypes.GuacDesktop ->
                    -- not implemented yet
                    ITypes.Hidden

                ITypes.CockpitDashboard ->
                    cockpit Dashboard

                ITypes.CockpitTerminal ->
                    cockpit Terminal

                ITypes.NativeSSH ->
                    case maybeFloatingIp of
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
                    case server.osProps.consoleUrl of
                        RemoteData.NotAsked ->
                            ITypes.Unavailable "Console URL is not queried yet"

                        RemoteData.Loading ->
                            ITypes.Loading

                        RemoteData.Failure error ->
                            ITypes.Error ("Exosphere requested a console URL and got the following error: " ++ Debug.toString error)

                        RemoteData.Success consoleUrl ->
                            ITypes.Ready consoleUrl

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
            ( "Unavailable", SH.toElementColor palette.muted )

        ITypes.Loading ->
            ( "Loading", SH.toElementColor palette.warn )

        ITypes.Ready _ ->
            ( "Ready", SH.toElementColor palette.readyGood )

        ITypes.Warn _ _ ->
            ( "Warning", SH.toElementColor palette.warn )

        ITypes.Error _ ->
            ( "Error", SH.toElementColor palette.error )

        ITypes.Hidden ->
            ( "Hidden", SH.toElementColor palette.muted )


interactionDetails : ITypes.Interaction -> View.Types.Context -> ITypes.InteractionDetails msg
interactionDetails interaction context =
    case interaction of
        ITypes.GuacTerminal ->
            ITypes.InteractionDetails
                (context.localization.commandDrivenTextInterface
                    |> Helpers.String.toTitleCase
                )
                (String.concat
                    [ "Get a terminal session to your "
                    , context.localization.virtualComputer
                    , ". Pro tip, press Ctrl+Alt+Shift inside the terminal window to show a graphical file upload/download tool!"
                    ]
                )
                (\_ _ -> FeatherIcons.terminal |> FeatherIcons.toHtml [] |> Element.html)
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
                (\_ _ -> FeatherIcons.monitor |> FeatherIcons.toHtml [] |> Element.html)
                ITypes.UrlInteraction

        ITypes.CockpitDashboard ->
            ITypes.InteractionDetails
                "Server Dashboard"
                "Deprecated feature"
                Icon.gauge
                ITypes.UrlInteraction

        ITypes.CockpitTerminal ->
            ITypes.InteractionDetails
                "Old Web Terminal"
                "Deprecated feature"
                (\_ _ -> FeatherIcons.terminal |> FeatherIcons.toHtml [] |> Element.html)
                ITypes.UrlInteraction

        ITypes.NativeSSH ->
            ITypes.InteractionDetails
                "Native SSH"
                "Advanced feature: use your computer's native SSH client to get a command-line session with extra capabilities"
                (\_ _ -> FeatherIcons.terminal |> FeatherIcons.toHtml [] |> Element.html)
                ITypes.TextInteraction

        ITypes.Console ->
            ITypes.InteractionDetails
                "Console"
                (String.join " "
                    [ "Advanced feature: Launching the console is like connecting a screen, mouse, and keyboard to your"
                    , context.localization.virtualComputer
                    , "(useful for troubleshooting if the Web Terminal isn't working)"
                    ]
                )
                Icon.console
                ITypes.UrlInteraction


type CockpitDashboardOrTerminal
    = Dashboard
    | Terminal
