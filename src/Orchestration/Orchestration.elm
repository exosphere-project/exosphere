module Orchestration.Orchestration exposing (orchModel)

import Helpers.GetterSetters
import Orchestration.GoalNetworkResources exposing (goalPollNetworkResources)
import Orchestration.GoalProject exposing (goalPollProject)
import Orchestration.GoalServer exposing (goalNewServer, goalPollServers)
import Orchestration.GoalShare exposing (goalNewShare)
import Orchestration.Helpers exposing (applyProjectStep)
import Time
import Types.HelperTypes exposing (CloudSpecificConfig)
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg)
import Types.View exposing (ViewState)
import UUID


orchModel : ViewState -> SharedModel -> Time.Posix -> ( SharedModel, Cmd SharedMsg )
orchModel viewState model time =
    let
        ( newProjects, newCmds ) =
            model.projects
                |> List.map (\proj -> ( Helpers.GetterSetters.cloudSpecificConfigLookup model.viewContext.cloudSpecificConfigs proj, proj ))
                |> List.map (\( cloudConfig, proj ) -> orchProject model.clientUuid time cloudConfig viewState proj)
                |> List.unzip
    in
    ( { model | projects = newProjects }, Cmd.batch newCmds )


orchProject : UUID.UUID -> Time.Posix -> Maybe CloudSpecificConfig -> ViewState -> Project -> ( Project, Cmd SharedMsg )
orchProject exoClientUuid time maybeCloudSpecificConfig viewState project =
    let
        goals =
            [ goalDummy exoClientUuid time
            , goalNewServer exoClientUuid time
            , goalNewShare exoClientUuid time
            , goalPollServers time maybeCloudSpecificConfig
            , goalPollNetworkResources time
            , goalPollProject time viewState
            ]
    in
    List.foldl
        applyProjectStep
        ( project, Cmd.none )
        goals


goalDummy : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalDummy _ _ project =
    ( project, Cmd.none )
