module Orchestration.Orchestration exposing (orchModel)

import Orchestration.GoalServer exposing (goalNewServer, goalPollServers)
import Orchestration.Helpers exposing (applyProjectStep)
import Time
import Types.Types exposing (FloatingIpState(..), Model, Msg, Project)
import UUID


orchModel : Model -> Time.Posix -> ( Model, Cmd Msg )
orchModel model time =
    let
        ( newProjects, newCmds ) =
            model.projects
                |> List.map (orchProject model.clientUuid time)
                |> List.unzip
    in
    ( { model | projects = newProjects }, Cmd.batch newCmds )


orchProject : UUID.UUID -> Time.Posix -> Project -> ( Project, Cmd Msg )
orchProject exoClientUuid time project =
    let
        goals =
            [ goalDummy exoClientUuid time
            , goalNewServer exoClientUuid time
            , goalPollServers time
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
