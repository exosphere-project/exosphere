module Orchestration.GoalNewServer exposing (goalNewServer)

import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import Orchestration.Helpers exposing (applyStepToAllServersThisExo)
import Rest.Neutron
import Time
import Types.Types exposing (FloatingIpState(..), Model, Msg, Project, Server)
import UUID


goalNewServer : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd Msg )
goalNewServer exoClientUuid time project =
    let
        tasks =
            [ taskServerPoll time
            , taskServerRequestPorts time
            , taskServerRequestFloatingIp time
            , taskDummy time
            ]

        ( newProject, newCmds ) =
            List.foldl
                (applyStepToAllServersThisExo exoClientUuid)
                ( project, Cmd.none )
                tasks
    in
    ( newProject, newCmds )


taskServerPoll : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
taskServerPoll time project server =
    -- TODO poll server if it hasn't been polled recently.
    -- TODO For this we need to know the last time the server was polled. We need to store in model.
    -- TODO We also need to figure out if server needs frequent polling -- see old context-dependent polling code.
    ( project, Cmd.none )


taskServerRequestPorts : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
taskServerRequestPorts time project server =
    let
        requestStuff =
            let
                oldPorts =
                    project.ports

                newPorts =
                    { oldPorts | refreshStatus = RDPP.Loading time }
            in
            ( { project | ports = newPorts }, Rest.Neutron.requestPorts project server )
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


taskServerRequestFloatingIp : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
taskServerRequestFloatingIp _ project server =
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


taskDummy : Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
taskDummy _ project _ =
    ( project, Cmd.none )
