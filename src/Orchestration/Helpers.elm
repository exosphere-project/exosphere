module Orchestration.Helpers exposing (applyProjectStep, applyStepToAllServersThisExo)

import Helpers.Helpers exposing (serverFromThisExoClient)
import Helpers.RemoteDataPlusPlus as RDPP
import Types.Types exposing (FloatingIpState(..), Model, Msg, Project, Server)
import UUID



-- These functions help with apply goals and tasks to a list of similar resources (e.g. projects, servers)


applyProjectStep : (Project -> ( Project, Cmd Msg )) -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
applyProjectStep step ( project, cmds ) =
    let
        ( stepProj, stepCmds ) =
            step project
    in
    ( stepProj, Cmd.batch [ cmds, stepCmds ] )


applyStepToAllServersThisExo : UUID.UUID -> (Project -> Server -> ( Project, Cmd Msg )) -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
applyStepToAllServersThisExo exoClientUuid step ( project, cmds ) =
    let
        applyServerStep server_ ( project_, cmds_ ) =
            let
                ( stepProj, stepCmds ) =
                    step project_ server_
            in
            if serverFromThisExoClient exoClientUuid server_ then
                ( stepProj, Cmd.batch [ cmds_, stepCmds ] )

            else
                ( project_, cmds_ )
    in
    case project.servers.data of
        RDPP.DoHave servers _ ->
            List.foldl applyServerStep ( project, cmds ) servers

        _ ->
            ( project, cmds )
