module Orchestration.GoalNewServer exposing (goalNewServer)

import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import Orchestration.Helpers exposing (applyStepToAllServersThisExo)
import Rest.Neutron
import Rest.Nova
import Time
import Types.Types exposing (FloatingIpState(..), Model, Msg, Project, Server)
import UUID


goalNewServer : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd Msg )
goalNewServer exoClientUuid time project =
    let
        steps =
            [ stepServerPoll time
            , stepServerRequestPorts time
            , stepServerRequestNetworks time
            , stepServerRequestFloatingIp time
            ]

        ( newProject, newCmds ) =
            List.foldl
                (applyStepToAllServersThisExo exoClientUuid)
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

                receivedRecently recTime interval =
                    Time.posixToMillis time < (Time.posixToMillis recTime + interval)
            in
            receivedRecently receivedTime pollInterval

        serverIsLoading =
            case project.servers.refreshStatus of
                RDPP.Loading _ ->
                    True

                _ ->
                    server.exoProps.loadingSeparately
    in
    if serverReceivedRecentlyEnough then
        ( project, Cmd.none )

    else if serverIsLoading then
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
