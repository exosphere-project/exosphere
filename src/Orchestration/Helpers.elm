module Orchestration.Helpers exposing (applyProjectStep, applyStepToAllServers, pollRDPP, serverPollIntervalMs)

import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers exposing (serverFromThisExoClient)
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import Time
import Types.Project exposing (Project)
import Types.Server exposing (ExoSetupStatus(..), Server, ServerFromExoProps, ServerOrigin(..))
import Types.SharedMsg exposing (SharedMsg)
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


serverPollIntervalMs : Project -> Server -> Int
serverPollIntervalMs project server =
    case GetterSetters.serverCreatedByCurrentUser project server.osProps.uuid of
        Just True ->
            myOwnServerPollIntervalMs server

        _ ->
            1800000


myOwnServerPollIntervalMs : Server -> Int
myOwnServerPollIntervalMs server =
    case
        server.osProps.details.openstackStatus
    of
        OSTypes.ServerBuild ->
            15000

        _ ->
            case
                ( server.exoProps.deletionAttempted
                , server.exoProps.targetOpenstackStatus
                , server.exoProps.serverOrigin
                )
            of
                ( False, Nothing, ServerNotFromExo ) ->
                    -- Not created from Exosphere, not deleting or waiting a pending server action
                    120000

                ( False, Nothing, ServerFromExo fromExoProps ) ->
                    myOwnServerFromExoPollIntervalMs fromExoProps

                _ ->
                    -- We're expecting OpenStack status to change (or server to be deleted) very soon
                    15000


myOwnServerFromExoPollIntervalMs : ServerFromExoProps -> Int
myOwnServerFromExoPollIntervalMs { exoSetupStatus } =
    case exoSetupStatus.data of
        RDPP.DoHave ( ExoSetupWaiting, _ ) _ ->
            -- Exosphere-created, booting up for the first time
            15000

        RDPP.DoHave ( ExoSetupRunning, _ ) _ ->
            -- Exosphere-created, running setup
            15000

        RDPP.DoHave _ _ ->
            -- Exosphere-created, not waiting for setup to complete
            120000

        RDPP.DontHave ->
            -- Exosphere-created and Exosphere setup status unknown
            15000
