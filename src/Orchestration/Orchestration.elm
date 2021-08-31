module Orchestration.Orchestration exposing (orchModel)

import Helpers.GetterSetters
import Orchestration.GoalNetworkResources exposing (goalPollNetworkResources)
import Orchestration.GoalServer exposing (goalNewServer, goalPollServers)
import Orchestration.Helpers exposing (applyProjectStep)
import Time
import Types.HelperTypes exposing (CloudSpecificConfig)
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg)
import UUID


orchModel : SharedModel -> Time.Posix -> ( SharedModel, Cmd SharedMsg )
orchModel model time =
    let
        ( newProjects, newCmds ) =
            model.projects
                |> List.map (\proj -> ( Helpers.GetterSetters.cloudConfigLookup model.cloudSpecificConfigs proj, proj ))
                |> List.map (\( cloudConfig, proj ) -> orchProject model.clientUuid time cloudConfig proj)
                |> List.unzip
    in
    ( { model | projects = newProjects }, Cmd.batch newCmds )


orchProject : UUID.UUID -> Time.Posix -> Maybe CloudSpecificConfig -> Project -> ( Project, Cmd SharedMsg )
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


goalDummy : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalDummy _ _ project =
    ( project, Cmd.none )
