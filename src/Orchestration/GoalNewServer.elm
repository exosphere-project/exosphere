module Orchestration.GoalNewServer exposing (goalNewServer)

import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import Orchestration.Helpers exposing (applyStepToAllProjectServers)
import Rest.Neutron
import Time
import Types.Types exposing (FloatingIpState(..), Model, Msg, Project, Server)
import UUID


goalNewServer : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd Msg )
goalNewServer exoClientUuid time project =
    let
        tasks =
            [ taskPollServer exoClientUuid time
            , taskRequestFloatingIp exoClientUuid time
            , taskDummy exoClientUuid time
            ]

        ( newProject, newCmds ) =
            List.foldl
                applyStepToAllProjectServers
                ( project, Cmd.none )
                tasks
    in
    ( newProject, newCmds )


taskPollServer : UUID.UUID -> Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
taskPollServer exoClientUuid time project server =
    -- TODO poll server if it hasn't been polled recently.
    -- TODO For this we need to know the last time the server was polled. We need to store in model.
    ( project, Cmd.none )


taskRequestFloatingIp : UUID.UUID -> Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
taskRequestFloatingIp exoClientUuid time project server =
    -- Request floating IP address for new server
    let
        serverDoWeRequestFloatingIp : Maybe OSTypes.Port
        serverDoWeRequestFloatingIp =
            if
                Helpers.serverFromThisExoClient exoClientUuid server
                    && not server.exoProps.deletionAttempted
                    && (server.osProps.details.openstackStatus
                            == OSTypes.ServerActive
                       )
                    && (Helpers.checkFloatingIpState server.osProps.details server.exoProps.priorFloatingIpState
                            == Requestable
                       )
            then
                List.filter (\port_ -> port_.deviceUuid == server.osProps.uuid) project.ports
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


taskDummy : UUID.UUID -> Time.Posix -> Project -> Server -> ( Project, Cmd Msg )
taskDummy exoClientUuid time project server =
    ( project, Cmd.none )
