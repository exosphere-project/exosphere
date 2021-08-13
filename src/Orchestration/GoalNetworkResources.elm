module Orchestration.GoalNetworkResources exposing (goalPollNetworkResources)

import Helpers.GetterSetters as GetterSetters
import Orchestration.Helpers exposing (applyProjectStep, pollRDPP)
import Rest.Neutron
import Time
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))


goalPollNetworkResources : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalPollNetworkResources time project =
    let
        steps =
            [ stepPollFloatingIps time
            , stepPollPorts time
            ]

        ( newProject, newCmds ) =
            List.foldl
                applyProjectStep
                ( project, Cmd.none )
                steps
    in
    ( newProject, newCmds )


stepPollFloatingIps : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepPollFloatingIps time project =
    let
        requestStuff =
            ( GetterSetters.projectSetFloatingIpsLoading project
            , Rest.Neutron.requestFloatingIps project
            )

        pollIntervalMs =
            120000
    in
    if pollRDPP project.floatingIps time pollIntervalMs then
        requestStuff

    else
        ( project, Cmd.none )


stepPollPorts : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepPollPorts time project =
    let
        requestStuff =
            ( GetterSetters.projectSetPortsLoading project
            , Rest.Neutron.requestPorts project
            )

        pollIntervalMs =
            120000
    in
    if pollRDPP project.ports time pollIntervalMs then
        requestStuff

    else
        ( project, Cmd.none )
