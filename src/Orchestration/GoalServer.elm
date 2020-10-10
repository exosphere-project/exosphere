module Orchestration.GoalServer exposing (goalNewServer, goalPollServers)

import Dict
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ServerResourceUsage exposing (getMostRecentDataPoint)
import OpenStack.ConsoleLog
import OpenStack.Types as OSTypes
import Orchestration.Helpers exposing (applyStepToAllServers)
import Rest.Guacamole
import Rest.Neutron
import Rest.Nova
import Time
import Types.ServerResourceUsage exposing (TimeSeries)
import Types.Types
    exposing
        ( FloatingIpState(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , Server
        , ServerOrigin(..)
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


goalPollServers : Time.Posix -> Project -> ( Project, Cmd Msg )
goalPollServers time project =
    let
        steps =
            [ stepServerPoll time
            , stepServerPollConsoleLog time
            , stepServerGuacamoleAuth time
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
                RDPP.Loading _ ->
                    True

                _ ->
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
                Helpers.projectUpdateServer project newServer
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
            && (Helpers.checkFloatingIpState server.osProps.details server.exoProps.priorFloatingIpState
                    == Requestable
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
            && (Helpers.checkFloatingIpState server.osProps.details server.exoProps.priorFloatingIpState
                    == Requestable
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
    -- Request floating IP address for new server
    let
        serverDoWeRequestFloatingIp : Maybe OSTypes.Port
        serverDoWeRequestFloatingIp =
            if
                not server.exoProps.deletionAttempted
                    && (server.osProps.details.openstackStatus
                            == OSTypes.ServerActive
                       )
                    && (Helpers.checkFloatingIpState server.osProps.details server.exoProps.priorFloatingIpState
                            == Requestable
                       )
            then
                RDPP.withDefault [] project.ports
                    |> List.filter (\port_ -> port_.deviceUuid == server.osProps.uuid)
                    |> List.head

            else
                Nothing

        maybeExtNet =
            Helpers.getExternalNetwork project
    in
    -- TODO if we don't find an external network, how do we indicate that to user? Fire a Cmd that shows an error? Or just wait until we have one?
    case ( serverDoWeRequestFloatingIp, maybeExtNet ) of
        ( Just port_, Just extNet ) ->
            let
                newServer =
                    let
                        oldExoProps =
                            server.exoProps
                    in
                    Server server.osProps { oldExoProps | priorFloatingIpState = RequestedWaiting }

                newProject =
                    Helpers.projectUpdateServer project newServer

                newCmd =
                    Rest.Neutron.requestCreateFloatingIp project extNet port_ server
            in
            ( newProject, newCmd )

        _ ->
            ( project, Cmd.none )


stepServerPollConsoleLog : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
stepServerPollConsoleLog time project server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            -- Don't poll server that won't be logging resource usage to console
            ( project, Cmd.none )

        ServerFromExo exoOriginProps ->
            let
                oneMinMillis =
                    60000

                curTimeMillis =
                    Time.posixToMillis time

                doPollLines : Maybe Int
                doPollLines =
                    let
                        serverIsActive =
                            server.osProps.details.openstackStatus == OSTypes.ServerActive

                        consoleLogNotLoading =
                            case exoOriginProps.resourceUsage.refreshStatus of
                                RDPP.NotLoading _ ->
                                    True

                                RDPP.Loading _ ->
                                    False
                    in
                    if
                        serverIsActive
                            && (exoOriginProps.exoServerVersion >= 2)
                            && consoleLogNotLoading
                    then
                        case exoOriginProps.resourceUsage.data of
                            RDPP.DontHave ->
                                -- Get a lot of log if we haven't polled for it before
                                Just 1000

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

                                    linesToPoll : Int
                                    linesToPoll =
                                        if Helpers.serverLessThan30MinsOld server time || (data.pollingStrikes > 0) then
                                            1000

                                        else
                                            10
                                in
                                if
                                    -- Poll if we have time series data with last data point at least one minute old.
                                    ((not <| Dict.isEmpty data.timeSeries)
                                        && tsDataOlderThanOneMinute data.timeSeries
                                    )
                                        -- Poll if server <30 mins old or has <5 polling strikes,
                                        -- and the last time we polled was at least one minute ago.
                                        || ((Helpers.serverLessThan30MinsOld server time || (data.pollingStrikes < 5))
                                                && atLeastOneMinSinceLogReceived
                                           )
                                then
                                    Just linesToPoll

                                else
                                    Nothing

                    else
                        Nothing
            in
            case doPollLines of
                Nothing ->
                    ( project, Cmd.none )

                Just pollLines ->
                    let
                        newResourceUsage =
                            RDPP.setLoading exoOriginProps.resourceUsage time

                        newExoOriginProps =
                            { exoOriginProps | resourceUsage = newResourceUsage }

                        oldExoProps =
                            server.exoProps

                        newExoProps =
                            { oldExoProps | serverOrigin = ServerFromExo newExoOriginProps }

                        newServer =
                            { server | exoProps = newExoProps }

                        newProject =
                            Helpers.projectUpdateServer project newServer
                    in
                    ( newProject
                    , OpenStack.ConsoleLog.requestConsoleLog
                        project
                        server
                        pollLines
                    )


stepServerGuacamoleAuth : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
stepServerGuacamoleAuth time project server =
    let
        -- Default value in Guacamole is 60 minutes, using 55 minutes for safety
        maxGuacTokenLifetimeMillis =
            3300000

        guacUpstreamPort =
            49528
    in
    case ( server.exoProps.serverOrigin, project.tlsReverseProxyHostname ) of
        ( ServerFromExo exoOriginProps, Just proxyHostname ) ->
            let
                ( requestTokenProj, requestTokenCmd ) =
                    -- TODO this logic is very ugly, needs a rework
                    case
                        ( Helpers.getServerFloatingIp server.osProps.details.ipAddresses
                        , Helpers.getServerExouserPassword server.osProps.details
                        )
                    of
                        ( Just floatingIp, Just exouserPassword ) ->
                            let
                                oldGuacToken =
                                    exoOriginProps.guacamoleToken

                                newGuacToken =
                                    { oldGuacToken | refreshStatus = RDPP.Loading time }

                                newExoOriginProps =
                                    { exoOriginProps | guacamoleToken = newGuacToken }

                                oldExoProps =
                                    server.exoProps

                                newExoProps =
                                    { oldExoProps | serverOrigin = ServerFromExo newExoOriginProps }

                                newServer =
                                    { server | exoProps = newExoProps }

                                url =
                                    Helpers.buildProxyUrl
                                        proxyHostname
                                        floatingIp
                                        guacUpstreamPort
                                        "/guacamole/api/tokens"
                                        False
                            in
                            ( Helpers.projectUpdateServer project newServer
                            , Rest.Guacamole.requestLoginToken
                                url
                                "exouser"
                                exouserPassword
                                (\result ->
                                    ProjectMsg (Helpers.getProjectId project) <|
                                        ReceiveGuacamoleAuthToken server.osProps.uuid result
                                )
                            )

                        _ ->
                            ( project, Cmd.none )
            in
            if exoOriginProps.exoServerVersion >= 3 then
                case exoOriginProps.guacamoleToken.refreshStatus of
                    RDPP.Loading _ ->
                        ( project, Cmd.none )

                    RDPP.NotLoading _ ->
                        case exoOriginProps.guacamoleToken.data of
                            RDPP.DontHave ->
                                ( requestTokenProj, requestTokenCmd )

                            RDPP.DoHave _ recTime ->
                                if Time.posixToMillis recTime + maxGuacTokenLifetimeMillis > Time.posixToMillis time then
                                    ( project, Cmd.none )

                                else
                                    ( requestTokenProj, requestTokenCmd )

            else
                ( project, Cmd.none )

        _ ->
            ( project, Cmd.none )
