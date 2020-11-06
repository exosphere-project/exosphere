module Helpers.Interaction exposing (interactionNameDescription, interactionStatus)

import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import RemoteData
import Time
import Types.Guacamole as GuacTypes
import Types.Interaction as ITypes
import Types.Types exposing (CockpitLoginStatus(..), Server, ServerOrigin(..), UserAppProxyHostname)


interactionStatus : Server -> ITypes.Interaction -> Bool -> Time.Posix -> Maybe UserAppProxyHostname -> ITypes.InteractionStatus
interactionStatus server interaction isElectron currentTime tlsReverseProxyHostname =
    let
        maybeFloatingIp =
            Helpers.getServerFloatingIp server.osProps.details.ipAddresses

        guacTerminal : ITypes.InteractionStatus
        guacTerminal =
            let
                guacUpstreamPort =
                    49528

                fifteenMinMillis =
                    1000 * 60 * 15

                newServer =
                    Helpers.serverLessThanThisOld server currentTime
            in
            case server.exoProps.serverOrigin of
                ServerNotFromExo ->
                    ITypes.Unavailable "Server not launched from Exosphere"

                ServerFromExo exoOriginProps ->
                    case exoOriginProps.guacamoleStatus of
                        GuacTypes.NotLaunchedWithGuacamole ->
                            if exoOriginProps.exoServerVersion < 3 then
                                ITypes.Unavailable "Server was created with an older version of Exosphere"

                            else
                                ITypes.Unavailable "Server was deployed with Guacamole support de-selected"

                        GuacTypes.LaunchedWithGuacamole guacProps ->
                            case guacProps.authToken.data of
                                RDPP.DoHave token _ ->
                                    case ( tlsReverseProxyHostname, maybeFloatingIp ) of
                                        ( Just proxyHostname, Just floatingIp ) ->
                                            ITypes.Ready <|
                                                Helpers.buildProxyUrl
                                                    proxyHostname
                                                    floatingIp
                                                    guacUpstreamPort
                                                    ("/guacamole/#/client/c2hlbGwAYwBkZWZhdWx0?token=" ++ token)
                                                    False

                                        ( Nothing, _ ) ->
                                            ITypes.Unavailable "Cannot find TLS-terminating reverse proxy server"

                                        ( _, Nothing ) ->
                                            ITypes.Unavailable "Server does not have a floating IP address"

                                RDPP.DontHave ->
                                    if newServer fifteenMinMillis then
                                        ITypes.Unavailable "Guacamole is still deploying to this new server, check back in a few minutes"

                                    else
                                        case
                                            ( tlsReverseProxyHostname
                                            , maybeFloatingIp
                                            , Helpers.getServerExouserPassword server.osProps.details
                                            )
                                        of
                                            ( Nothing, _, _ ) ->
                                                ITypes.Error "Cannot find TLS-terminating reverse proxy server"

                                            ( _, Nothing, _ ) ->
                                                ITypes.Error "Server does not have a floating IP address"

                                            ( _, _, Nothing ) ->
                                                ITypes.Error "Cannot find server password to authenticate"

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
            if isElectron then
                case server.exoProps.serverOrigin of
                    ServerNotFromExo ->
                        ITypes.Unavailable "Server not launched from Exosphere"

                    ServerFromExo serverFromExoProps ->
                        case ( serverFromExoProps.cockpitStatus, maybeFloatingIp ) of
                            ( NotChecked, _ ) ->
                                ITypes.Unavailable "Status of server dashboard and terminal not available yet"

                            ( CheckedNotReady, _ ) ->
                                ITypes.Unavailable "Not ready"

                            ( _, Nothing ) ->
                                ITypes.Unavailable "Server does not have a floating IP address"

                            ( _, Just floatingIp ) ->
                                case dashboardOrTerminal of
                                    Dashboard ->
                                        ITypes.Ready <|
                                            "https://"
                                                ++ floatingIp
                                                ++ ":9090"

                                    Terminal ->
                                        case guacTerminal of
                                            ITypes.Ready _ ->
                                                ITypes.Hidden

                                            _ ->
                                                ITypes.Ready <|
                                                    "https://"
                                                        ++ floatingIp
                                                        ++ ":9090/cockpit/@localhost/system/terminal.html"

            else
                ITypes.Unavailable "Cockpit-based interactions only available in native desktop client"
    in
    case server.osProps.details.openstackStatus of
        OSTypes.ServerBuilding ->
            ITypes.Unavailable "Server is still building"

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
                            ITypes.Unavailable "Server does not have a floating IP address"

                        Just floatingIp ->
                            -- TODO fix, display copyable text instead of a link
                            ITypes.Ready <| "ssh://exouser@" ++ floatingIp

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
            ITypes.Unavailable "Server is not active"


interactionNameDescription : ITypes.Interaction -> ( String, String )
interactionNameDescription interaction =
    -- TODO provide an icon as well
    case interaction of
        ITypes.GuacTerminal ->
            ( "Web Terminal"
            , "Get a command-line session to your server"
            )

        ITypes.GuacDesktop ->
            ( "Streaming Desktop", "Interact with your server's desktop environment" )

        ITypes.CockpitDashboard ->
            ( "Server Dashboard", "Deprecated feature" )

        ITypes.CockpitTerminal ->
            ( "Web Terminal", "Deprecated feature" )

        ITypes.NativeSSH ->
            ( "Native SSH"
            , "Advanced feature: use your computer's native SSH client to get a command-line session with extra capabilities"
            )

        ITypes.Console ->
            ( "Console"
            , "Advanced feature: Launching the console is like connecting a screen, mouse, and keyboard to your server (useful for troubleshooting if the Web Terminal isn't working)"
            )


type CockpitDashboardOrTerminal
    = Dashboard
    | Terminal
