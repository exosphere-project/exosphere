module Orchestration.GoalServer exposing (goalNewServer, goalPollServers)

import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Url as UrlHelpers
import OpenStack.ConsoleLog
import OpenStack.DnsRecordSet
import OpenStack.Types as OSTypes
import Orchestration.Helpers exposing (applyStepToAllServers)
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


goalNewServer : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalNewServer exoClientUuid time project =
    let
        steps =
            [ stepServerRequestPorts time
            , stepServerRequestNetworks time
            , stepServerRequestFloatingIp time
            , stepServerRequestHostname time
            ]

        ( newProject, newCmds ) =
            List.foldl
                (applyStepToAllServers (Just exoClientUuid))
                ( project, Cmd.none )
                steps
    in
    ( newProject, newCmds )


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
            , stepServerGuacamoleAuth time userAppProxy
            ]

        ( newProject, newCmds ) =
            List.foldl
                (applyStepToAllServers Nothing)
                ( project, Cmd.none )
                steps
    in
    ( newProject, newCmds )


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
                    Helpers.serverPollIntervalMs project server
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


stepServerRequestNetworks : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerRequestNetworks time project server =
    -- TODO DRY with function below?
    let
        requestStuff =
            ( { project | networks = RDPP.setLoading project.networks }
            , Rest.Neutron.requestNetworks project
            )
    in
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


stepServerRequestPorts : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerRequestPorts time project server =
    -- TODO DRY with function above?
    let
        requestStuff =
            ( { project | ports = RDPP.setLoading project.ports }, Rest.Neutron.requestPorts project )
    in
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
            -- If not requested in last 5 seconds
            && RDPP.isPollableWithInterval project.dnsRecordSets time 5000
            && (-- If any server ip is without hostname then request records
                GetterSetters.getServerFloatingIps project server.osProps.uuid
                    |> List.any
                        (\ipAddress ->
                            case OpenStack.DnsRecordSet.addressToRecord (project.dnsRecordSets |> RDPP.withDefault []) ipAddress.address of
                                Just _ ->
                                    False

                                Nothing ->
                                    True
                        )
               )
    then
        ( { project | dnsRecordSets = RDPP.setLoading project.dnsRecordSets }
        , Rest.Designate.requestRecordSets project
        )

    else
        ( project, Cmd.none )


stepServerRequestFloatingIp : Time.Posix -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerRequestFloatingIp _ project server =
    -- Request to create/assign floating IP address to new server
    if
        not server.exoProps.deletionAttempted
            && serverIsActiveEnough server
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
                thirtySecMillis =
                    30000

                oneMinMillis =
                    60000

                thirtyMinMillis =
                    1000 * 60 * 30

                -- For return type of next functions, the outer maybe determines whether to poll at all. The inner maybe
                -- determines whether we poll the whole log (Nothing) or just a set number of lines (Just Int).
                doPollLinesExoSetupStatus : Maybe (Maybe Int)
                doPollLinesExoSetupStatus =
                    let
                        pollInterval =
                            -- Poll more frequently if ExoSetupStatus is in a non-terminal state
                            case exoOriginProps.exoSetupStatus.data of
                                RDPP.DontHave ->
                                    thirtySecMillis

                                RDPP.DoHave statusTuple _ ->
                                    if
                                        List.member (Tuple.first statusTuple)
                                            [ ExoSetupUnknown, ExoSetupWaiting, ExoSetupRunning ]
                                    then
                                        thirtySecMillis

                                    else
                                        thirtyMinMillis
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
                    let
                        linesToPoll : Maybe Int
                        linesToPoll =
                            case exoOriginProps.resourceUsage.data of
                                RDPP.DontHave ->
                                    -- Get all the log if we don't have it at all yet
                                    Nothing

                                RDPP.DoHave data _ ->
                                    if Helpers.serverLessThanThisOld server time thirtyMinMillis || (data.pollingStrikes > 0) then
                                        -- Get all the log if server is new or there were polling failures
                                        Nothing

                                    else
                                        -- Only get recent logs
                                        Just 10
                    in
                    if
                        serverIsActiveEnough server
                            && (exoOriginProps.exoServerVersion >= 2)
                            && RDPP.isPollableWithInterval exoOriginProps.resourceUsage time oneMinMillis
                    then
                        Just linesToPoll

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
            -- 5 minutes
            300000

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


stepServerGuacamoleAuth : Time.Posix -> Maybe UserAppProxyHostname -> Project -> Server -> ( Project, Cmd SharedMsg )
stepServerGuacamoleAuth time maybeUserAppProxy project server =
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
                        "/guacamole/api/tokens"
                        False
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
                            let
                                doRequestToken_ =
                                    doRequestToken floatingIp passphrase tlsReverseProxyHostname exoOriginProps launchedWithGuacProps
                            in
                            case launchedWithGuacProps.authToken.refreshStatus of
                                RDPP.Loading ->
                                    doNothing project

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
                                                        doNothing project

                                                    else
                                                        doRequestToken_

                                        RDPP.DoHave _ receivedTime ->
                                            let
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
