module Orchestration.Helpers exposing (applyProjectStep, applyStepToAllProjectServers)

import RemoteData
import Types.Types exposing (FloatingIpState(..), Model, Msg, Project, Server)


applyProjectStep : (Project -> ( Project, Cmd Msg )) -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
applyProjectStep step ( project, cmds ) =
    let
        ( stepProj, stepCmds ) =
            step project
    in
    ( stepProj, Cmd.batch [ cmds, stepCmds ] )


applyStepToAllProjectServers : (Project -> Server -> ( Project, Cmd Msg )) -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
applyStepToAllProjectServers step ( project, cmds ) =
    let
        applyServerStep server_ ( project_, cmds_ ) =
            let
                ( stepProj, stepCmds ) =
                    step project_ server_
            in
            ( stepProj, Cmd.batch [ cmds_, stepCmds ] )
    in
    case project.servers of
        RemoteData.Success servers ->
            List.foldl applyServerStep ( project, cmds ) servers

        _ ->
            ( project, cmds )
