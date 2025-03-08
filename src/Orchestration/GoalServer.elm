module Orchestration.GoalServer exposing (goalNewServer, goalPollServers, requestFloatingIp)

import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Url as UrlHelpers
import OpenStack.ConsoleLog
import OpenStack.DnsRecordSet
import OpenStack.Types as OSTypes
import Orchestration.Helpers exposing (applyStepToAllServers, pollIntervalToMs, serverPollIntervalMs)
import Orchestration.Types exposing (PollInterval(..))
import Rest.Designate
import Rest.Guacamole
import Rest.Neutron
import Rest.Nova
import Time
import Types.Guacamole as GuacTypes
import Types.HelperTypes
    exposing
        ( CloudSpecificConfig
        , FloatingIpAssignmentStatus(..)
        , FloatingIpOption(..)
        , FloatingIpReuseOption(..)
        , UserAppProxyHostname
        )
import Types.Project exposing (Project)
import Types.Server exposing (ExoSetupStatus(..), Server, ServerFromExoProps, ServerOrigin(..))
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))
import UUID
import Url


goalNewServer : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalNewServer exoClientUuid time project =
    let
        steps =
            [ stepServerRequestPorts time
            , stepServerRequestNetworks time
            , stepServerRequestFloatingIp time
            , stepServerRequestHostname time
            ]
    in
    List.foldl
        (applyStepToAllServers (Just exoClientUuid))
        ( project, Cmd.none )
        steps


goalPollServers : Time.Posix -> Maybe CloudSpecificConfig -> Project -> ( Project, Cmd SharedMsg )
goalPollServers time maybeCloudSpecificConfig project =
    let
        userAppProxy =
            maybeCloudSpecificConfig
                |> Maybe.andThen (GetterSetters.getUserAppProxyFromCloudSpecificConfig project)

        steps =
            [ stepServerPoll time
            , stepServerPollConsoleLog time
            , stepServerPollEvents time
            , stepServerPollSecurityGroups time
            , stepServerGuacamoleAuth time userAppProxy
            ]
    in
    List.foldl
        (applyStepToAllServers Nothing)
        ( project, Cmd.none )
        steps


stepServerPoll : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerPoll time project server =
    let
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
                    serverPollIntervalMs project server
            in
            Time.posixToMillis time < (Time.posixToMillis receivedTime + pollInterval)
    in
    if serverReceivedRecentlyEnough then
        ( project, Cmd.none )

    else
        let
            dontPollBecauseServerIsLoading : Bool
            dontPollBecauseServerIsLoading =
                case project.servers.refreshStatus of
                    RDPP.Loading ->
                        True

                    RDPP.NotLoading _ ->
                        server.exoProps.loadingSeparately
        in
        if dontPollBecauseServerIsLoading then
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


stepServerRequestNetworks : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerRequestNetworks time project server =
    -- TODO DRY with function below?
    if
        not server.exoProps.deletionAttempted
            && serverIsActiveEnough server
            && (case
                    Helpers.getNewFloatingIpOption project server.osProps server.exoProps.floatingIpCreationOption
                of
                    UseFloatingIp _ Attemptable ->
                        True

                    _ ->
                        False
               )
    then
        let
            requestStuff =
                ( { project | networks = RDPP.setLoading project.networks }
                , Rest.Neutron.requestNetworks project
                )
        in
        case project.networks.refreshStatus of
            RDPP.NotLoading (Just ( _, receivedTime )) ->
                -- If we got an error, try again slightly later
                if Time.posixToMillis time - Time.posixToMillis receivedTime > pollIntervalToMs Rapid then
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


stepServerRequestPorts : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerRequestPorts time project server =
    -- TODO DRY with function above?
    if
        not server.exoProps.deletionAttempted
            && serverIsActiveEnough server
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
        let
            requestStuff =
                ( { project | ports = RDPP.setLoading project.ports }, Rest.Neutron.requestPorts project )
        in
        case project.ports.refreshStatus of
            RDPP.NotLoading (Just ( _, receivedTime )) ->
                -- If we got an error, try again slightly later?
                if Time.posixToMillis time - Time.posixToMillis receivedTime > pollIntervalToMs Rapid then
                    requestStuff

                else
                    ( project, Cmd.none )

            RDPP.NotLoading _ ->
                case project.ports.data of
                    RDPP.DontHave ->
                        requestStuff

                    RDPP.DoHave portsData _ ->
                        if
                            portsData
                                |> List.all (\port_ -> port_.deviceUuid /= server.osProps.uuid)
                        then
                            requestStuff

                        else
                            ( project, Cmd.none )

            _ ->
                ( project, Cmd.none )

    else
        ( project, Cmd.none )


stepServerRequestHostname : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerRequestHostname time project server =
    if
        not server.exoProps.deletionAttempted
            && serverIsNew server time
            -- If not requested in last few seconds
            && RDPP.isPollableWithInterval project.dnsRecordSets time (pollIntervalToMs Rapid)
            && (-- If any server ip is without hostname then request records
                GetterSetters.getServerFloatingIps project server.osProps.uuid
                    |> List.any
                        (\{ address } ->
                            List.isEmpty <|
                                OpenStack.DnsRecordSet.lookupRecordsByAddress (project.dnsRecordSets |> RDPP.withDefault []) address
                        )
               )
    then
        ( { project | dnsRecordSets = RDPP.setLoading project.dnsRecordSets }
        , Rest.Designate.requestRecordSets project
        )

    else
        ( project, Cmd.none )


stepServerRequestFloatingIp : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerRequestFloatingIp time project server =
    -- Request to create/assign floating IP address to new server
    if
        not server.exoProps.deletionAttempted
            && serverIsActiveEnough server
            && serverIsNew server time
    then
        requestFloatingIp project server

    else
        ( project, Cmd.none )


requestFloatingIp : Project -> Server -> ( Project, Cmd SharedMsg )
requestFloatingIp project server =
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
                                    Rest.Neutron.requestCreateFloatingIp project network (Just ( port_, server.osProps.uuid )) Nothing

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


stepServerPollConsoleLog : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
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
                doPollLinesCombined : Maybe (Maybe Int)
                doPollLinesCombined =
                    -- Poll the maximum amount of whatever log is needed between resource usage graphs and setup status
                    -- This factoring could be nicer
                    let
                        -- For return type of next functions, the outer maybe determines whether to poll at all. The inner maybe
                        -- determines whether we poll the whole log (Nothing) or just a set number of lines (Just Int).
                        doPollLinesExoSetupStatus : Maybe (Maybe Int)
                        doPollLinesExoSetupStatus =
                            let
                                pollInterval =
                                    -- Poll more frequently if ExoSetupStatus is in a non-terminal state
                                    case exoOriginProps.exoSetupStatus.data of
                                        RDPP.DontHave ->
                                            pollIntervalToMs Rapid

                                        RDPP.DoHave statusTuple _ ->
                                            if
                                                List.member (Tuple.first statusTuple)
                                                    [ ExoSetupUnknown, ExoSetupWaiting, ExoSetupRunning ]
                                            then
                                                pollIntervalToMs Rapid

                                            else
                                                pollIntervalToMs Seldom
                            in
                            if
                                serverIsActiveEnough server
                                    && (exoOriginProps.exoServerVersion >= 4)
                                    && RDPP.isPollableWithInterval exoOriginProps.exoSetupStatus time pollInterval
                            then
                                Just Nothing

                            else
                                Nothing

                        doPollLinesResourceUsage : Maybe (Maybe Int)
                        doPollLinesResourceUsage =
                            if
                                serverIsActiveEnough server
                                    && (exoOriginProps.exoServerVersion >= 2)
                                    && RDPP.isPollableWithInterval exoOriginProps.resourceUsage time (pollIntervalToMs Regular)
                            then
                                Just <|
                                    case exoOriginProps.resourceUsage.data of
                                        RDPP.DontHave ->
                                            -- Get all the log if we don't have it at all yet
                                            Nothing

                                        RDPP.DoHave data _ ->
                                            if Helpers.serverLessThanThisOld server time (pollIntervalToMs Seldom) || (data.pollingStrikes > 0) then
                                                -- Get all the log if server is new or there were polling failures
                                                Nothing

                                            else
                                                -- Only get recent logs
                                                Just 10

                            else
                                Nothing

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
                    in
                    if pollEntireLog then
                        Just Nothing

                    else
                        let
                            pollExplicitNumLines =
                                [ doPollLinesResourceUsage, doPollLinesExoSetupStatus ]
                                    |> List.filterMap identity
                                    |> List.filterMap identity
                                    |> List.maximum
                        in
                        pollExplicitNumLines |> Maybe.map Just
            in
            case doPollLinesCombined of
                Nothing ->
                    ( project, Cmd.none )

                Just pollLines ->
                    let
                        newExoSetupStatus =
                            RDPP.setLoading exoOriginProps.exoSetupStatus

                        newResourceUsage =
                            RDPP.setLoading exoOriginProps.resourceUsage

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


stepServerPollEvents : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerPollEvents time project server =
    let
        pollIntervalMillis =
            pollIntervalToMs Seldom

        curTimeMillis =
            Time.posixToMillis time

        pollIfIntervalExceeded : Time.Posix -> ( Project, Cmd SharedMsg )
        pollIfIntervalExceeded receivedTime =
            if Time.posixToMillis receivedTime + pollIntervalMillis < curTimeMillis then
                doPollEvents

            else
                doNothing project

        doPollEvents =
            let
                newServer =
                    { server | events = RDPP.setLoading server.events }

                newProject =
                    GetterSetters.projectUpdateServer project newServer
            in
            ( newProject, Rest.Nova.requestServerEvents newProject server.osProps.uuid )
    in
    case server.events.refreshStatus of
        RDPP.Loading ->
            doNothing project

        RDPP.NotLoading maybeErrorTimeTuple ->
            case server.events.data of
                RDPP.DoHave _ receivedTime ->
                    pollIfIntervalExceeded receivedTime

                RDPP.DontHave ->
                    case maybeErrorTimeTuple of
                        Nothing ->
                            doPollEvents

                        Just ( _, receivedTime ) ->
                            pollIfIntervalExceeded receivedTime


stepServerPollSecurityGroups : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerPollSecurityGroups time project server =
    let
        pollIntervalMillis =
            pollIntervalToMs Seldom

        curTimeMillis =
            Time.posixToMillis time

        pollIfIntervalExceeded : Time.Posix -> ( Project, Cmd SharedMsg )
        pollIfIntervalExceeded receivedTime =
            if Time.posixToMillis receivedTime + pollIntervalMillis < curTimeMillis then
                doPollSecurityGroups

            else
                doNothing project

        doPollSecurityGroups =
            let
                newServer =
                    { server | securityGroups = RDPP.setLoading server.securityGroups }

                newProject =
                    GetterSetters.projectUpdateServer project newServer
            in
            ( newProject, Rest.Nova.requestServerSecurityGroups newProject server.osProps.uuid )
    in
    case server.securityGroups.refreshStatus of
        RDPP.Loading ->
            doNothing project

        RDPP.NotLoading maybeErrorTimeTuple ->
            case server.securityGroups.data of
                RDPP.DoHave _ receivedTime ->
                    pollIfIntervalExceeded receivedTime

                RDPP.DontHave ->
                    case maybeErrorTimeTuple of
                        Nothing ->
                            doPollSecurityGroups

                        Just ( _, receivedTime ) ->
                            pollIfIntervalExceeded receivedTime


stepServerGuacamoleAuth : Time.Posix -> Maybe UserAppProxyHostname -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerGuacamoleAuth time maybeUserAppProxy project server =
    let
        guacUpstreamPort =
            49528

        doRequestToken : String -> String -> UserAppProxyHostname -> ServerFromExoProps -> GuacTypes.LaunchedWithGuacProps -> ( Project, Cmd SharedMsg )
        doRequestToken floatingIp passphrase proxyHostname oldExoOriginProps oldGuacProps =
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
                        Url.Http
                        [ "guacamole", "api", "tokens" ]
                        []
            in
            ( GetterSetters.projectUpdateServer project newServer
            , Rest.Guacamole.requestLoginToken
                url
                "exouser"
                passphrase
                (\result ->
                    ProjectMsg (GetterSetters.projectIdentifier project) <|
                        ServerMsg server.osProps.uuid <|
                            ReceiveGuacamoleAuthToken result
                )
            )
    in
    case
        ( server.exoProps.serverOrigin
        , serverIsActiveEnough server
        )
    of
        ( ServerFromExo exoOriginProps, True ) ->
            case exoOriginProps.guacamoleStatus of
                GuacTypes.NotLaunchedWithGuacamole ->
                    doNothing project

                GuacTypes.LaunchedWithGuacamole launchedWithGuacProps ->
                    case
                        ( GetterSetters.getServerFloatingIps project server.osProps.uuid
                            |> List.map .address
                            |> List.head
                        , GetterSetters.getServerExouserPassphrase server.osProps.details
                        , maybeUserAppProxy
                        )
                    of
                        ( Just floatingIp, Just passphrase, Just tlsReverseProxyHostname ) ->
                            case launchedWithGuacProps.authToken.refreshStatus of
                                RDPP.Loading ->
                                    doNothing project

                                RDPP.NotLoading maybeErrorTimeTuple ->
                                    let
                                        doRequestToken_ =
                                            doRequestToken floatingIp passphrase tlsReverseProxyHostname exoOriginProps launchedWithGuacProps

                                        curTimeMillis =
                                            Time.posixToMillis time
                                    in
                                    case launchedWithGuacProps.authToken.data of
                                        RDPP.DontHave ->
                                            case maybeErrorTimeTuple of
                                                Nothing ->
                                                    doRequestToken_

                                                Just ( _, receivedTime ) ->
                                                    let
                                                        errorRetryIntervalMillis =
                                                            pollIntervalToMs Rapid

                                                        whenToRetryMillis =
                                                            Time.posixToMillis receivedTime + errorRetryIntervalMillis
                                                    in
                                                    if curTimeMillis <= whenToRetryMillis then
                                                        doNothing project

                                                    else
                                                        doRequestToken_

                                        RDPP.DoHave _ receivedTime ->
                                            let
                                                maxGuacTokenLifetimeMillis =
                                                    -- Default value in Guacamole is 60 minutes, using 55 minutes for safety
                                                    3300000

                                                whenToRefreshMillis =
                                                    Time.posixToMillis receivedTime + maxGuacTokenLifetimeMillis
                                            in
                                            if curTimeMillis <= whenToRefreshMillis then
                                                doNothing project

                                            else
                                                doRequestToken_

                        _ ->
                            -- Missing either a floating IP, passphrase, or TLS-terminating reverse proxy server
                            doNothing project

        _ ->
            doNothing project


serverIsActiveEnough : Server -> Bool
serverIsActiveEnough server =
    List.member server.osProps.details.openstackStatus [ OSTypes.ServerActive, OSTypes.ServerPassword, OSTypes.ServerRescue, OSTypes.ServerVerifyResize ]


{-| Server is considered new if it was created in the last 5 minutes
-}
serverIsNew : Server -> Time.Posix -> Bool
serverIsNew server time =
    let
        fiveMinutesOfMillis =
            5 * 60 * 1000

        serverAgeMillis =
            Time.posixToMillis time
                - Time.posixToMillis server.osProps.details.created
    in
    serverAgeMillis < fiveMinutesOfMillis


doNothing : Project -> ( Project, Cmd SharedMsg )
doNothing project =
    ( project, Cmd.none )
