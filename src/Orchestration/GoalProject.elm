module Orchestration.GoalProject exposing (goalPollProject)

import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Quotas exposing (requestVolumeQuota)
import OpenStack.VolumeSnapshots
import OpenStack.Volumes exposing (requestVolumeSnapshots)
import Orchestration.Helpers exposing (applyProjectStep, pollIntervalToMs)
import Orchestration.Types exposing (PollInterval(..))
import Time
import Types.Project exposing (Project)
import Types.SharedMsg exposing (SharedMsg)


goalPollProject : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalPollProject time project =
    let
        steps =
            [ stepSnapshotPoll time

            -- add stepVolumePoll here
            ]
    in
    List.foldl
        applyProjectStep
        ( project, Cmd.none )
        steps


stepSnapshotPoll : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepSnapshotPoll time project =
    let
        snapshots =
            project.volumeSnapshots

        onlyTransitioning =
            List.filter OpenStack.VolumeSnapshots.isTransitioning

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
                    RDPP.isPollableWithInterval snapshots time (pollIntervalToMs Rapid)
    in
    if shouldPoll then
        ( { project | volumeSnapshots = RDPP.setLoading project.volumeSnapshots }
        , Cmd.batch [ requestVolumeSnapshots project, requestVolumeQuota project ]
        )

    else
        ( project, Cmd.none )
