module Orchestration.GoalNetworkResources exposing (goalPollNetworkResources)

import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Orchestration.Helpers exposing (applyProjectStep)
import Rest.Neutron
import Time
import Types.Types
    exposing
        ( CloudSpecificConfig
        , ExoSetupStatus(..)
        , FloatingIpAssignmentStatus(..)
        , FloatingIpOption(..)
        , FloatingIpReuseOption(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , Server
        , ServerFromExoProps
        , ServerOrigin(..)
        , ServerSpecificMsgConstructor(..)
        , UserAppProxyHostname
        )


goalPollNetworkResources : Time.Posix -> Project -> ( Project, Cmd Msg )
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


stepPollFloatingIps : Time.Posix -> Project -> ( Project, Cmd Msg )
stepPollFloatingIps time project =
    let
        requestStuff =
            ( GetterSetters.projectSetFloatingIpsLoading time project
            , Rest.Neutron.requestFloatingIps project
            )

        pollIntervalMs =
            120000

        receivedRecentlyEnough =
            let
                receivedTime =
                    case project.floatingIps.data of
                        RDPP.DoHave _ receivedTime_ ->
                            receivedTime_

                        RDPP.DontHave ->
                            Time.millisToPosix 0
            in
            Time.posixToMillis time < (Time.posixToMillis receivedTime + pollIntervalMs)

        dontPollBecauseLoading =
            case project.floatingIps.refreshStatus of
                RDPP.Loading _ ->
                    True

                _ ->
                    False
    in
    if receivedRecentlyEnough || dontPollBecauseLoading then
        ( project, Cmd.none )

    else
        requestStuff


stepPollPorts : Time.Posix -> Project -> ( Project, Cmd Msg )
stepPollPorts time project =
    let
        requestStuff =
            ( GetterSetters.projectSetPortsLoading time project
            , Rest.Neutron.requestPorts project
            )

        pollIntervalMs =
            120000

        receivedRecentlyEnough =
            let
                receivedTime =
                    case project.ports.data of
                        RDPP.DoHave _ receivedTime_ ->
                            receivedTime_

                        RDPP.DontHave ->
                            Time.millisToPosix 0
            in
            Time.posixToMillis time < (Time.posixToMillis receivedTime + pollIntervalMs)

        dontPollBecauseLoading =
            case project.ports.refreshStatus of
                RDPP.Loading _ ->
                    True

                _ ->
                    False
    in
    if receivedRecentlyEnough || dontPollBecauseLoading then
        ( project, Cmd.none )

    else
        requestStuff
