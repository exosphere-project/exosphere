module Orchestration.GoalNetworkResources exposing (goalPollNetworkResources)

import Helpers.GetterSetters as GetterSetters
import Orchestration.Helpers exposing (applyProjectStep, pollRDPP)
import Rest.Neutron
import Time
import Types.Project exposing (Project)
import Types.SharedMsg exposing (SharedMsg)


goalPollNetworkResources : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
goalPollNetworkResources time project =
    let
        steps =
            [ stepPollFloatingIps time
            , stepPollPorts time
            , stepPollSecurityGroups time
            ]
    in
    List.foldl
        applyProjectStep
        ( project, Cmd.none )
        steps


stepPollFloatingIps : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepPollFloatingIps time project =
    let
        pollIntervalMs =
            120000
    in
    if pollRDPP project.floatingIps time pollIntervalMs then
        ( GetterSetters.projectSetFloatingIpsLoading project
        , Rest.Neutron.requestFloatingIps project
        )

    else
        ( project, Cmd.none )


stepPollPorts : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepPollPorts time project =
    let
        pollIntervalMs =
            120000
    in
    if pollRDPP project.ports time pollIntervalMs then
        ( GetterSetters.projectSetPortsLoading project
        , Rest.Neutron.requestPorts project
        )

    else
        ( project, Cmd.none )


stepPollSecurityGroups : Time.Posix -> Project -> ( Project, Cmd SharedMsg )
stepPollSecurityGroups time project =
    let
        pollIntervalMs =
            120000
    in
    if pollRDPP project.securityGroups time pollIntervalMs then
        ( GetterSetters.projectSetSecurityGroupsLoading project
        , Rest.Neutron.requestSecurityGroups project
        )

    else
        ( project, Cmd.none )
