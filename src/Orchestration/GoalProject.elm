module Orchestration.GoalProject exposing (goalPollProject)

import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import List
import OpenStack.Quotas exposing (requestVolumeQuota)
import OpenStack.Types as OSTypes
import OpenStack.VolumeSnapshots
import OpenStack.Volumes as OSVolumes exposing (requestVolumeSnapshots)
import Orchestration.Helpers exposing (applyProjectStep, pollIntervalToMs, pollRDPP)
import Orchestration.Types exposing (PollInterval(..))
import Time
import Types.Project exposing (Project)
import Types.SharedMsg exposing (SharedMsg)
import Types.View exposing (ProjectViewConstructor(..), ViewState(..))


goalPollProject : Time.Posix -> ViewState -> Project -> ( Project, Cmd SharedMsg )
goalPollProject time viewState project =
    let
        steps =
            [ stepSnapshotPoll time
            , stepVolumePoll time viewState
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


stepVolumePoll : Time.Posix -> ViewState -> Project -> ( Project, Cmd SharedMsg )
stepVolumePoll time viewState project =
    let
        pollInterval =
            case viewState of
                ProjectView _ projectViewState ->
                    let
                        volumeNeedsFrequentPoll volume =
                            OSTypes.isVolumeTransitioning volume
                                -- Reserved is a transition state except for shelved instances.
                                && (not <| GetterSetters.isVolumeReservedForShelvedInstance project volume)

                        anyVolumeNeedsFrequentPoll =
                            List.any volumeNeedsFrequentPoll (RDPP.withDefault [] project.volumes)

                        serverVolumeNeedsFrequentPoll server =
                            GetterSetters.getVolsAttachedToServer project server
                                |> List.any volumeNeedsFrequentPoll
                    in
                    case projectViewState of
                        ProjectOverview _ ->
                            if anyVolumeNeedsFrequentPoll then
                                Rapid

                            else
                                Regular

                        VolumeList _ ->
                            if anyVolumeNeedsFrequentPoll then
                                Rapid

                            else
                                Regular

                        VolumeDetail pageModel ->
                            case GetterSetters.volumeLookup project pageModel.volumeUuid of
                                Just volume ->
                                    if volumeNeedsFrequentPoll volume then
                                        Rapid

                                    else
                                        Regular

                                Nothing ->
                                    Regular

                        ServerDetail pageModel ->
                            case GetterSetters.serverLookup project pageModel.serverUuid of
                                Just server ->
                                    if serverVolumeNeedsFrequentPoll server then
                                        Rapid

                                    else
                                        Regular

                                Nothing ->
                                    Seldom

                        _ ->
                            Seldom

                _ ->
                    Seldom
    in
    if pollRDPP project.volumes time (pollIntervalToMs pollInterval) then
        let
            newProject =
                GetterSetters.projectSetVolumesLoading project
        in
        ( newProject
        , OSVolumes.requestVolumes newProject
        )

    else
        ( project, Cmd.none )
