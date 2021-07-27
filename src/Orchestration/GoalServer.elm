module Orchestration.GoalServer exposing (goalNewServer, goalPollServers)

import Dict
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ServerResourceUsage exposing (getMostRecentDataPoint)
import Helpers.Url as UrlHelpers
import OpenStack.ConsoleLog
import OpenStack.Types as OSTypes
import Orchestration.Helpers exposing (applyStepToAllServers)
import Rest.Guacamole
import Rest.Neutron
import Rest.Nova
import Time
import Types.Guacamole as GuacTypes
import Types.ServerResourceUsage exposing (TimeSeries)
import Types.Types
    exposing
        ( CloudSpecificConfig
        , ExoSetupStatus(..)
        , FloatingIpAssignmentStatus(..)
        , FloatingIpOption(..)
        , FloatingIpReuseOption(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , Server
        , ServerFromExoProps
        , ServerOrigin(..)
        , ServerSpecificMsgConstructor(..)
        , UserAppProxyHostname
        )
import UUID


goalNewServer : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd Msg )
goalNewServer exoClientUuid time project =
    let
        steps =
            [ stepServerRequestPorts time
            , stepServerRequestNetworks time
            , stepServerRequestFloatingIp time
            ]

        ( newProject, newCmds ) =
            List.foldl
                (applyStepToAllServers (Just exoClientUuid))
                ( project, Cmd.none )
                steps
    in
    ( newProject, newCmds )


goalPollServers : Time.Posix -> Maybe CloudSpecificConfig -> Project -> ( Project, Cmd Msg )
goalPollServers time maybeCloudSpecificConfig project =
    let
        userAppProxy =
            maybeCloudSpecificConfig |> Maybe.andThen (\csc -> csc.userAppProxy)

        steps =
            [ stepServerPoll time
            , stepServerPollConsoleLog time
            , stepServerGuacamoleAuth time userAppProxy
            ]

        ( newProject, newCmds ) =
            List.foldl
                (applyStepToAllServers Nothing)
                ( project, Cmd.none )
                steps
    in
    ( newProject, newCmds )


stepServerPoll : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
stepServerPoll time project server =
    let
        frequentPollIntervalMs =
            4500

        infrequentPollIntervalMs =
            60000

        serverReceivedRecentlyEnough =
            let
                receivedTime =
                    case server.exoProps.receivedTime of
                        Just receivedTime_ ->
                            receivedTime_

                        Nothing ->
                            case project.servers.data of
                                RDPP.DoHave _ receivedTime__ ->
                                    receivedTime__

                                RDPP.DontHave ->
                                    Time.millisToPosix 0

                pollInterval =
                    if Helpers.serverNeedsFrequentPoll server then
                        frequentPollIntervalMs

                    else
                        infrequentPollIntervalMs
            in
            Time.posixToMillis time < (Time.posixToMillis receivedTime + pollInterval)

        dontPollBecauseServerIsLoading : Bool
        dontPollBecauseServerIsLoading =
            case project.servers.refreshStatus of
                RDPP.Loading ->
                    True

                RDPP.NotLoading _ ->
                    server.exoProps.loadingSeparately
    in
    if serverReceivedRecentlyEnough then
        ( project, Cmd.none )

    else if dontPollBecauseServerIsLoading then
        ( project, Cmd.none )

    else
        let
            oldExoProps =
                server.exoProps

            newExoProps =
                { oldExoProps
                    | loadingSeparately = True
                }

            newServer =
                { server | exoProps = newExoProps }

            newProject =
                GetterSetters.projectUpdateServer project newServer
        in
        ( newProject, Rest.Nova.requestServer project newServer.osProps.uuid )


stepServerRequestNetworks : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
stepServerRequestNetworks time project server =
    -- TODO DRY with function below?
    let
        requestStuff =
            ( { project | networks = RDPP.setLoading project.networks time }
            , Rest.Neutron.requestNetworks project
            )
    in
    if
        not server.exoProps.deletionAttempted
            && (server.osProps.details.openstackStatus
                    == OSTypes.ServerActive
               )
            && (case
                    Helpers.getNewFloatingIpOption project server.osProps server.exoProps.floatingIpCreationOption
                of
                    UseFloatingIp _ Attemptable ->
                        True

                    _ ->
                        False
               )
    then
        case project.networks.refreshStatus of
            RDPP.NotLoading (Just ( _, receivedTime )) ->
                -- If we got an error, try again 10 seconds later?
                if Time.posixToMillis time - Time.posixToMillis receivedTime > 10000 then
                    requestStuff

                else
                    ( project, Cmd.none )

            RDPP.NotLoading _ ->
                case project.networks.data of
                    RDPP.DontHave ->
                        requestStuff

                    RDPP.DoHave _ _ ->
                        ( project, Cmd.none )

            _ ->
                ( project, Cmd.none )

    else
        ( project, Cmd.none )


stepServerRequestPorts : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
stepServerRequestPorts time project server =
    -- TODO DRY with function above?
    let
        requestStuff =
            ( { project | ports = RDPP.setLoading project.ports time }, Rest.Neutron.requestPorts project )
    in
    if
        not server.exoProps.deletionAttempted
            && (server.osProps.details.openstackStatus
                    == OSTypes.ServerActive
               )
            && (case
                    Helpers.getNewFloatingIpOption project server.osProps server.exoProps.floatingIpCreationOption
                of
                    Automatic ->
                        True

                    UseFloatingIp _ WaitingForResources ->
                        True

                    _ ->
                        False
               )
    then
        case project.ports.refreshStatus of
            RDPP.NotLoading (Just ( _, receivedTime )) ->
                -- If we got an error, try again 10 seconds later?
                if Time.posixToMillis time - Time.posixToMillis receivedTime > 10000 then
                    requestStuff

                else
                    ( project, Cmd.none )

            RDPP.NotLoading _ ->
                case project.ports.data of
                    RDPP.DontHave ->
                        requestStuff

                    RDPP.DoHave portsData _ ->
                        if
                            List.filter (\port_ -> port_.deviceUuid == server.osProps.uuid) portsData
                                |> List.isEmpty
                        then
                            requestStuff

                        else
                            ( project, Cmd.none )

            _ ->
                ( project, Cmd.none )

    else
        ( project, Cmd.none )


stepServerRequestFloatingIp : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
stepServerRequestFloatingIp _ project server =
    -- Request to create/assign floating IP address to new server
    if
        not server.exoProps.deletionAttempted
            && (server.osProps.details.openstackStatus
                    == OSTypes.ServerActive
               )
    then
        case
            ( GetterSetters.getServerPorts project server.osProps.uuid
                |> List.head
            , GetterSetters.getExternalNetwork project
            )
        of
            ( Just port_, Just network ) ->
                case Helpers.getNewFloatingIpOption project server.osProps server.exoProps.floatingIpCreationOption of
                    UseFloatingIp reuseOption Attemptable ->
                        let
                            cmd =
                                case reuseOption of
                                    CreateNewFloatingIp ->
                                        Rest.Neutron.requestCreateFloatingIp project network port_ server

                                    UseExistingFloatingIp ipUuid ->
                                        Rest.Neutron.requestAssignFloatingIp project port_ ipUuid

                            newServer =
                                let
                                    oldExoProps =
                                        server.exoProps
                                in
                                { server | exoProps = { oldExoProps | floatingIpCreationOption = UseFloatingIp reuseOption AttemptedWaiting } }

                            newProject =
                                GetterSetters.projectUpdateServer project newServer
                        in
                        ( newProject, cmd )

                    _ ->
                        ( project, Cmd.none )

            _ ->
                ( project, Cmd.none )

    else
        ( project, Cmd.none )


stepServerPollConsoleLog : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
stepServerPollConsoleLog time project server =
    -- Now polling console log for two possible purposes:
    -- 1. Get system resource usage data
    -- 2. Look for new exoSetup value (e.g. running, complete, or error)
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            -- Don't poll server that won't be logging resource usage to console
            ( project, Cmd.none )

        ServerFromExo exoOriginProps ->
            let
                oneMinMillis =
                    60000

                thirtyMinMillis =
                    1000 * 60 * 30

                curTimeMillis =
                    Time.posixToMillis time

                serverIsActive =
                    server.osProps.details.openstackStatus == OSTypes.ServerActive

                consoleLogNotLoading =
                    -- ugh parallel data structures, should consolidate at some point?
                    case ( exoOriginProps.exoSetupStatus.refreshStatus, exoOriginProps.resourceUsage.refreshStatus ) of
                        ( RDPP.NotLoading _, RDPP.NotLoading _ ) ->
                            True

                        _ ->
                            False

                -- For return type of next functions, the outer maybe determines whether to poll at all. The inner maybe
                -- determines whether we poll the whole log (Nothing) or just a set number of lines (Just Int).
                doPollLinesExoSetupStatus : Maybe (Maybe Int)
                doPollLinesExoSetupStatus =
                    if
                        serverIsActive
                            && (exoOriginProps.exoServerVersion >= 4)
                            && consoleLogNotLoading
                    then
                        case exoOriginProps.exoSetupStatus.data of
                            RDPP.DontHave ->
                                -- Get the whole log
                                Just Nothing

                            RDPP.DoHave exoSetupStatus recTime ->
                                -- If setupStatus is in a non-terminal state and we haven't checked in at least 30 seconds, get the whole log
                                if
                                    List.member exoSetupStatus [ ExoSetupUnknown, ExoSetupWaiting, ExoSetupRunning ]
                                        && (Time.posixToMillis recTime + (30 * 1000) < curTimeMillis)
                                then
                                    Just Nothing

                                else
                                    Nothing

                    else
                        Nothing

                doPollLinesResourceUsage : Maybe (Maybe Int)
                doPollLinesResourceUsage =
                    if
                        serverIsActive
                            && (exoOriginProps.exoServerVersion >= 2)
                            && consoleLogNotLoading
                    then
                        case exoOriginProps.resourceUsage.data of
                            RDPP.DontHave ->
                                -- Get a lot of log if we haven't polled for it before
                                Just Nothing

                            RDPP.DoHave data recTime ->
                                let
                                    tsDataOlderThanOneMinute : TimeSeries -> Bool
                                    tsDataOlderThanOneMinute timeSeries =
                                        getMostRecentDataPoint timeSeries
                                            |> Maybe.map Tuple.first
                                            |> Maybe.map
                                                (\logTimeMillis ->
                                                    (curTimeMillis - logTimeMillis) > oneMinMillis
                                                )
                                            -- Defaults to False if timeseries is empty
                                            |> Maybe.withDefault False

                                    atLeastOneMinSinceLogReceived : Bool
                                    atLeastOneMinSinceLogReceived =
                                        (curTimeMillis - Time.posixToMillis recTime) > oneMinMillis

                                    linesToPoll : Maybe Int
                                    linesToPoll =
                                        if Helpers.serverLessThanThisOld server time thirtyMinMillis || (data.pollingStrikes > 0) then
                                            Nothing

                                        else
                                            Just 10
                                in
                                if
                                    -- Poll if we have time series data with last data point at least one minute old.
                                    ((not <| Dict.isEmpty data.timeSeries)
                                        && tsDataOlderThanOneMinute data.timeSeries
                                    )
                                        -- Poll if server <30 mins old or has <5 polling strikes,
                                        -- and the last time we polled was at least one minute ago.
                                        || ((Helpers.serverLessThanThisOld server time thirtyMinMillis || (data.pollingStrikes < 5))
                                                && atLeastOneMinSinceLogReceived
                                           )
                                then
                                    Just linesToPoll

                                else
                                    Nothing

                    else
                        Nothing

                doPollLinesCombined : Maybe (Maybe Int)
                doPollLinesCombined =
                    -- Poll the maximum amount of whatever log is needed between resource usage graphs and setup status
                    -- This factoring could be nicer
                    let
                        pollEntireLog =
                            [ doPollLinesResourceUsage, doPollLinesExoSetupStatus ]
                                |> List.filterMap identity
                                |> List.any
                                    (\x ->
                                        case x of
                                            Nothing ->
                                                True

                                            Just _ ->
                                                False
                                    )

                        pollExplicitNumLines =
                            [ doPollLinesResourceUsage, doPollLinesExoSetupStatus ]
                                |> List.filterMap identity
                                |> List.filterMap identity
                                |> List.maximum
                    in
                    if pollEntireLog then
                        Just Nothing

                    else
                        case pollExplicitNumLines of
                            Just l ->
                                Just (Just l)

                            Nothing ->
                                Nothing
            in
            case doPollLinesCombined of
                Nothing ->
                    ( project, Cmd.none )

                Just pollLines ->
                    let
                        newExoSetupStatus =
                            RDPP.setLoading exoOriginProps.exoSetupStatus time

                        newResourceUsage =
                            RDPP.setLoading exoOriginProps.resourceUsage time

                        newExoOriginProps =
                            { exoOriginProps
                                | resourceUsage = newResourceUsage
                                , exoSetupStatus = newExoSetupStatus
                            }

                        oldExoProps =
                            server.exoProps

                        newExoProps =
                            { oldExoProps | serverOrigin = ServerFromExo newExoOriginProps }

                        newServer =
                            { server | exoProps = newExoProps }

                        newProject =
                            GetterSetters.projectUpdateServer project newServer
                    in
                    ( newProject
                    , OpenStack.ConsoleLog.requestConsoleLog
                        project
                        server
                        pollLines
                    )


stepServerGuacamoleAuth : Time.Posix -> Maybe Types.Types.UserAppProxyHostname -> Project -> Server -> ( Project, Cmd Msg )
stepServerGuacamoleAuth time maybeUserAppProxy project server =
    -- TODO ensure server is active
    let
        curTimeMillis =
            Time.posixToMillis time

        -- Default value in Guacamole is 60 minutes, using 55 minutes for safety
        maxGuacTokenLifetimeMillis =
            3300000

        errorRetryIntervalMillis =
            15000

        guacUpstreamPort =
            49528

        doNothing =
            ( project, Cmd.none )

        doRequestToken : String -> String -> UserAppProxyHostname -> ServerFromExoProps -> GuacTypes.LaunchedWithGuacProps -> ( Project, Cmd Msg )
        doRequestToken floatingIp password proxyHostname oldExoOriginProps oldGuacProps =
            let
                oldAuthToken =
                    oldGuacProps.authToken

                newAuthToken =
                    { oldAuthToken | refreshStatus = RDPP.Loading }

                newGuacProps =
                    { oldGuacProps | authToken = newAuthToken }

                newExoOriginProps =
                    { oldExoOriginProps | guacamoleStatus = GuacTypes.LaunchedWithGuacamole newGuacProps }

                oldExoProps =
                    server.exoProps

                newExoProps =
                    { oldExoProps | serverOrigin = ServerFromExo newExoOriginProps }

                newServer =
                    { server | exoProps = newExoProps }

                url =
                    UrlHelpers.buildProxyUrl
                        proxyHostname
                        floatingIp
                        guacUpstreamPort
                        "/guacamole/api/tokens"
                        False
            in
            ( GetterSetters.projectUpdateServer project newServer
            , Rest.Guacamole.requestLoginToken
                url
                "exouser"
                password
                (\result ->
                    ProjectMsg project.auth.project.uuid <|
                        ServerMsg server.osProps.uuid <|
                            ReceiveGuacamoleAuthToken result
                )
            )
    in
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            doNothing

        ServerFromExo exoOriginProps ->
            case exoOriginProps.guacamoleStatus of
                GuacTypes.NotLaunchedWithGuacamole ->
                    doNothing

                GuacTypes.LaunchedWithGuacamole launchedWithGuacProps ->
                    case
                        ( GetterSetters.getServerFloatingIps project server.osProps.uuid
                            |> List.map .address
                            |> List.head
                        , GetterSetters.getServerExouserPassword server.osProps.details
                        , maybeUserAppProxy
                        )
                    of
                        ( Just floatingIp, Just password, Just tlsReverseProxyHostname ) ->
                            let
                                doRequestToken_ =
                                    doRequestToken floatingIp password tlsReverseProxyHostname exoOriginProps launchedWithGuacProps
                            in
                            case launchedWithGuacProps.authToken.refreshStatus of
                                RDPP.Loading ->
                                    doNothing

                                RDPP.NotLoading maybeErrorTimeTuple ->
                                    case launchedWithGuacProps.authToken.data of
                                        RDPP.DontHave ->
                                            case maybeErrorTimeTuple of
                                                Nothing ->
                                                    doRequestToken_

                                                Just ( _, receivedTime ) ->
                                                    let
                                                        whenToRetryMillis =
                                                            Time.posixToMillis receivedTime + errorRetryIntervalMillis
                                                    in
                                                    if curTimeMillis <= whenToRetryMillis then
                                                        doNothing

                                                    else
                                                        doRequestToken_

                                        RDPP.DoHave _ receivedTime ->
                                            let
                                                whenToRefreshMillis =
                                                    Time.posixToMillis receivedTime + maxGuacTokenLifetimeMillis
                                            in
                                            if curTimeMillis <= whenToRefreshMillis then
                                                doNothing

                                            else
                                                doRequestToken_

                        _ ->
                            -- Missing either a floating IP, password, or TLS-terminating reverse proxy server
                            doNothing
