module Orchestration.Helpers exposing (applyProjectStep, applyStepToAllServers)

import Helpers.Helpers exposing (serverFromThisExoClient)
import Helpers.RemoteDataPlusPlus as RDPP
import Types.Types exposing (Msg, Project, Server)
import UUID



-- These functions help with apply goals and steps to a list of similar resources (e.g. projects, servers)


applyProjectStep : (Project -> ( Project, Cmd Msg )) -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
applyProjectStep step ( project, cmds ) =
    let
        ( stepProj, stepCmds ) =
            step project
    in
    ( stepProj, Cmd.batch [ cmds, stepCmds ] )


applyStepToAllServers : Maybe UUID.UUID -> (Project -> Server -> ( Project, Cmd Msg )) -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
applyStepToAllServers maybeExoClientUuid step ( project, cmds ) =
    -- If maybeExoClientUuid is Just a client UUID, we only apply to servers created by that client UUID.
    -- Otherwise, we apply to all servers in the project.
    let
        applyServerStep server_ ( project_, cmds_ ) =
            let
                ( stepProj, stepCmds ) =
                    step project_ server_
            in
            case maybeExoClientUuid of
                Just exoClientUuid ->
                    if serverFromThisExoClient exoClientUuid server_ then
                        ( stepProj, Cmd.batch [ cmds_, stepCmds ] )

                    else
                        ( project_, cmds_ )

                Nothing ->
                    ( stepProj, Cmd.batch [ cmds_, stepCmds ] )
    in
    case project.servers.data of
        RDPP.DoHave servers _ ->
            List.foldl applyServerStep ( project, cmds ) servers

        _ ->
            ( project, cmds )
