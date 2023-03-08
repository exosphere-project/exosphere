module Orchestration.GoalProject exposing (goalPollProject)

import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Quotas exposing (requestVolumeQuota)
import OpenStack.Types exposing (isTransitioningStatus)
import OpenStack.Volumes exposing (requestVolumeSnapshots)
import Time
import Types.Project exposing (Project)
import Types.SharedMsg exposing (SharedMsg(..))


transitioningSnapshotPollInterval : Int
transitioningSnapshotPollInterval =
    3000


goalPollProject : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalPollProject time project =
    let
        steps =
            [ stepSnapshotPoll time

            -- add stepVolumePoll here
            -- add stepInstancePoll here
            ]

        joinCommands prev next =
            Cmd.batch [ prev, next ]

        ( newProject, newCmds ) =
            List.foldl
                (\step ( prevProject, prevCmds ) ->
                    Tuple.mapSecond
                        (joinCommands prevCmds)
                        (step prevProject)
                )
                ( project, Cmd.none )
                steps
    in
    ( newProject, newCmds )


stepSnapshotPoll : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepSnapshotPoll time project =
    let
        snapshots =
            project.volumeSnapshots

        onlyTransitioning =
            List.filter (isTransitioningStatus << .status)

        shouldPoll =
            case ( .data <| RDPP.map onlyTransitioning snapshots, snapshots.refreshStatus ) of
                -- No snapshots means nothing to update
                ( RDPP.DontHave, _ ) ->
                    False

                -- If no snapshots are currently transitioning we don't need to proactively poll.
                ( RDPP.DoHave [] _, _ ) ->
                    False

                -- Only poll if it's been a few seconds since the last one.
                ( RDPP.DoHave _ _, _ ) ->
                    RDPP.isPollableWithInterval snapshots time transitioningSnapshotPollInterval
    in
    if shouldPoll then
        ( { project | volumeSnapshots = RDPP.setLoading project.volumeSnapshots }
        , Cmd.batch [ requestVolumeSnapshots project, requestVolumeQuota project ]
        )

    else
        ( project, Cmd.none )
