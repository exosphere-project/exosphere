module Orchestration.Helpers exposing (applyProjectStep, applyStepToAllServers, pollRDPP)

import Helpers.Helpers exposing (serverFromThisExoClient)
import Helpers.RemoteDataPlusPlus as RDPP
import Time
import Types.Msg exposing (SharedMsg)
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import UUID



-- These functions help with apply goals and steps to a list of similar resources (e.g. projects, servers)


applyProjectStep : (Project -> ( Project, Cmd SharedMsg )) -> ( Project, Cmd SharedMsg ) -> ( Project, Cmd SharedMsg )
applyProjectStep step ( project, cmds ) =
    let
        ( stepProj, stepCmds ) =
            step project
    in
    ( stepProj, Cmd.batch [ cmds, stepCmds ] )


applyStepToAllServers : Maybe UUID.UUID -> (Project -> Server -> ( Project, Cmd SharedMsg )) -> ( Project, Cmd SharedMsg ) -> ( Project, Cmd SharedMsg )
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


pollRDPP : RDPP.RemoteDataPlusPlus e a -> Time.Posix -> Int -> Bool
pollRDPP rdpp time pollIntervalMs =
    let
        receivedRecentlyEnough =
            case rdpp.data of
                RDPP.DoHave _ receivedTime ->
                    Time.posixToMillis time < (Time.posixToMillis receivedTime + pollIntervalMs)

                RDPP.DontHave ->
                    case rdpp.refreshStatus of
                        RDPP.NotLoading (Just ( _, errorReceivedTime )) ->
                            -- Wait the poll interval if we don't have data,
                            -- but we do have an error from last time we requested it
                            Time.posixToMillis time < (Time.posixToMillis errorReceivedTime + pollIntervalMs)

                        _ ->
                            False

        loading =
            case rdpp.refreshStatus of
                RDPP.Loading ->
                    True

                RDPP.NotLoading _ ->
                    False
    in
    not receivedRecentlyEnough && not loading
