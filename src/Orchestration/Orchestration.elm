module Orchestration.Orchestration exposing (orchModel)

import Helpers.GetterSetters
import Orchestration.GoalNetworkResources exposing (goalPollNetworkResources)
import Orchestration.GoalServer exposing (goalNewServer, goalPollServers)
import Orchestration.Helpers exposing (applyProjectStep)
import Time
import Types.Msg exposing (Msg)
import Types.Project exposing (Project)
import Types.Types exposing (CloudSpecificConfig, Model)
import UUID


orchModel : Model -> Time.Posix -> ( Model, Cmd Msg )
orchModel model time =
    let
        ( newProjects, newCmds ) =
            model.projects
                |> List.map (\proj -> ( Helpers.GetterSetters.cloudConfigLookup model proj, proj ))
                |> List.map (\( cloudConfig, proj ) -> orchProject model.clientUuid time cloudConfig proj)
                |> List.unzip
    in
    ( { model | projects = newProjects }, Cmd.batch newCmds )


orchProject : UUID.UUID -> Time.Posix -> Maybe CloudSpecificConfig -> Project -> ( Project, Cmd Msg )
orchProject exoClientUuid time maybeCloudSpecificConfig project =
    let
        goals =
            [ goalDummy exoClientUuid time
            , goalNewServer exoClientUuid time
            , goalPollServers time maybeCloudSpecificConfig
            , goalPollNetworkResources time
            ]

        ( newProject, newCmds ) =
            List.foldl
                applyProjectStep
                ( project, Cmd.none )
                goals
    in
    ( newProject, newCmds )


goalDummy : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd Msg )
goalDummy _ _ project =
    ( project, Cmd.none )
