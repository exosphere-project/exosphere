module Rest.Neutron exposing
    ( receiveCreateExoSecurityGroupAndRequestCreateRules
    , receiveCreateFloatingIp
    , receiveDeleteFloatingIp
    , receiveFloatingIps
    , receiveNetworks
    , receiveSecurityGroupsAndEnsureExoGroup
    , requestAssignFloatingIp
    , requestAutoAllocatedNetwork
    , requestCreateFloatingIp
    , requestDeleteFloatingIp
    , requestFloatingIps
    , requestNetworks
    , requestPorts
    , requestSecurityGroups
    , requestUnassignFloatingIp
    )

import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import OpenStack.SecurityGroupRule as SecurityGroupRule exposing (SecurityGroupRule, securityGroupRuleDecoder)
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , expectStringWithErrorBody
        , openstackCredentialedRequest
        , resultToMsgErrorBody
        )
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (FloatingIpOption(..), HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.Server exposing (NewServerNetworkOptions(..), Server, ServerOrigin(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))



{- HTTP Requests -}


requestNetworks : Project -> Cmd SharedMsg
requestNetworks project =
    let
        errorContext =
            ErrorContext
                ("get list of networks for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveNetworks errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/networks")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodeNetworks
        )


requestAutoAllocatedNetwork : Project -> Cmd SharedMsg
requestAutoAllocatedNetwork project =
    let
        errorContext =
            ErrorContext
                ("get/create auto-allocated network for project \"" ++ project.auth.project.name ++ "\"")
                ErrorDebug
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveAutoAllocatedNetwork errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/auto-allocated-topology/" ++ project.auth.project.uuid)
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.at [ "auto_allocated_topology", "id" ] Decode.string)
        )


requestFloatingIps : Project -> Cmd SharedMsg
requestFloatingIps project =
    let
        errorContext =
            ErrorContext
                ("get list of floating IPs for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\ips ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveFloatingIps ips)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/floatingips")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            decodeFloatingIps
        )


requestPorts : Project -> Cmd SharedMsg
requestPorts project =
    let
        errorContext =
            ErrorContext
                ("get list of ports for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceivePorts errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/ports")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodePorts
        )


requestCreateFloatingIp : Project -> OSTypes.Network -> Maybe ( OSTypes.Port, Server ) -> Cmd SharedMsg
requestCreateFloatingIp project network maybePortServer =
    let
        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object <|
                        List.filterMap identity
                            [ Just ( "floating_network_id", Encode.string network.uuid )
                            , maybePortServer
                                |> Maybe.map Tuple.first
                                |> Maybe.map (\port_ -> ( "port_id", Encode.string port_.uuid ))
                            ]
                  )
                ]

        errorContext =
            let
                forPort =
                    maybePortServer
                        |> Maybe.map Tuple.first
                        |> Maybe.map (\port_ -> " for port" ++ port_.uuid)
                        |> Maybe.withDefault ""
            in
            ErrorContext
                ("create a floating IP address on network " ++ network.name ++ forPort)
                ErrorCrit
                (Just "It's possible your cloud has run out of public IP address space; ask your cloud administrator.")

        resultToMsg_ =
            \result ->
                ProjectMsg (GetterSetters.projectIdentifier project) <|
                    case maybePortServer of
                        Just ( _, server ) ->
                            ServerMsg server.osProps.uuid <|
                                ReceiveCreateFloatingIp errorContext result

                        Nothing ->
                            ReceiveCreateFloatingIp_ errorContext result

        requestCmd =
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Post
                Nothing
                []
                (project.endpoints.neutron ++ "/v2.0/floatingips")
                (Http.jsonBody requestBody)
                (expectJsonWithErrorBody
                    resultToMsg_
                    decodeFloatingIp
                )
    in
    requestCmd


requestDeleteFloatingIp : Project -> Types.Error.ErrorContext -> OSTypes.IpAddressUuid -> Cmd SharedMsg
requestDeleteFloatingIp project errorContext uuid =
    let
        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveDeleteFloatingIp uuid)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ uuid)
        Http.emptyBody
        (expectStringWithErrorBody
            resultToMsg_
        )


requestAssignFloatingIp : Project -> OSTypes.Port -> OSTypes.IpAddressUuid -> Cmd SharedMsg
requestAssignFloatingIp project port_ floatingIpUuid =
    let
        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object
                        [ ( "port_id", Encode.string port_.uuid )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("Assign floating IP address " ++ floatingIpUuid ++ " to port " ++ port_.uuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\ip ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveAssignFloatingIp ip)
                )

        requestCmd =
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Put
                Nothing
                []
                (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ floatingIpUuid)
                (Http.jsonBody requestBody)
                (expectJsonWithErrorBody
                    resultToMsg_
                    decodeFloatingIp
                )
    in
    requestCmd


requestUnassignFloatingIp : Project -> OSTypes.IpAddressUuid -> Cmd SharedMsg
requestUnassignFloatingIp project floatingIpUuid =
    let
        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object
                        [ ( "port_id", Encode.null )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("Unassign floating IP address " ++ floatingIpUuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\ip ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveUnassignFloatingIp ip)
                )

        requestCmd =
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Put
                Nothing
                []
                (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ floatingIpUuid)
                (Http.jsonBody requestBody)
                (expectJsonWithErrorBody
                    resultToMsg_
                    decodeFloatingIp
                )
    in
    requestCmd


requestSecurityGroups : Project -> Cmd SharedMsg
requestSecurityGroups project =
    let
        errorContext =
            ErrorContext
                ("get a list of security groups for project " ++ project.auth.project.name)
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveSecurityGroups errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodeSecurityGroups
        )


requestCreateExoSecurityGroup : Project -> Cmd SharedMsg
requestCreateExoSecurityGroup project =
    let
        desc =
            "Security group for instances launched via Exosphere"

        requestBody =
            Encode.object
                [ ( "security_group"
                  , Encode.object
                        [ ( "name", Encode.string "exosphere" )
                        , ( "description", Encode.string desc )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create security group for Exosphere in project " ++ project.auth.project.name)
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCreateExoSecurityGroup errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg
            decodeNewSecurityGroup
        )


requestCreateExoSecurityGroupRules : SharedModel -> Project -> List SecurityGroupRule -> ( SharedModel, Cmd SharedMsg )
requestCreateExoSecurityGroupRules model project rules =
    let
        maybeSecurityGroup =
            RDPP.withDefault [] project.securityGroups
                |> List.Extra.find (\g -> g.name == "exosphere")
    in
    case maybeSecurityGroup of
        Nothing ->
            -- No security group found, may have been deleted? Nothing to do
            ( model, Cmd.none )

        Just group ->
            let
                cmds =
                    requestCreateSecurityGroupRules
                        project
                        group
                        rules
                        "create rules for Exosphere security group"
            in
            ( model, Cmd.batch cmds )


requestCreateSecurityGroupRules : Project -> OSTypes.SecurityGroup -> List SecurityGroupRule -> String -> List (Cmd SharedMsg)
requestCreateSecurityGroupRules project group rules errorMessage =
    let
        errorContext =
            ErrorContext
                errorMessage
                --"create rules for Exosphere security group"
                ErrorCrit
                Nothing

        buildRequestCmd body =
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Post
                Nothing
                []
                (project.endpoints.neutron ++ "/v2.0/security-group-rules")
                (Http.jsonBody body)
                (expectStringWithErrorBody
                    (resultToMsgErrorBody errorContext (\_ -> NoOp))
                )

        bodies =
            rules
                |> List.map (SecurityGroupRule.encode group.uuid)

        cmds =
            bodies |> List.map buildRequestCmd
    in
    cmds



{- HTTP Response Handling -}


receiveNetworks : SharedModel -> Project -> List OSTypes.Network -> Project
receiveNetworks sharedModel project networks =
    let
        newNetsRDPP =
            RDPP.RemoteDataPlusPlus (RDPP.DoHave networks sharedModel.clientCurrentTime) (RDPP.NotLoading Nothing)
    in
    { project | networks = newNetsRDPP }


receiveFloatingIps : SharedModel -> Project -> List OSTypes.FloatingIp -> ( SharedModel, Cmd SharedMsg )
receiveFloatingIps model project floatingIps =
    let
        newProject =
            { project
                | floatingIps =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave floatingIps model.clientCurrentTime)
                        (RDPP.NotLoading Nothing)
            }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveCreateFloatingIp : SharedModel -> Project -> Maybe Server -> OSTypes.FloatingIp -> ( SharedModel, Cmd SharedMsg )
receiveCreateFloatingIp model project maybeServer floatingIp =
    let
        projectUpdatedServer =
            case maybeServer of
                Just server ->
                    let
                        newServer =
                            let
                                oldExoProps =
                                    server.exoProps
                            in
                            { server
                                | exoProps = { oldExoProps | floatingIpCreationOption = DoNotUseFloatingIp }
                            }
                    in
                    GetterSetters.projectUpdateServer project newServer

                Nothing ->
                    project

        newFloatingIps =
            floatingIp
                :: (project.floatingIps
                        |> RDPP.withDefault []
                   )

        newProject =
            { projectUpdatedServer
                | floatingIps =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave newFloatingIps model.clientCurrentTime)
                        (RDPP.NotLoading Nothing)
            }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveDeleteFloatingIp : SharedModel -> Project -> OSTypes.IpAddressUuid -> ( SharedModel, Cmd SharedMsg )
receiveDeleteFloatingIp model project uuid =
    case project.floatingIps.data of
        RDPP.DoHave floatingIps _ ->
            let
                newFloatingIps =
                    List.filter (\f -> f.uuid /= uuid) floatingIps

                newProject =
                    { project
                        | floatingIps =
                            RDPP.RemoteDataPlusPlus
                                (RDPP.DoHave newFloatingIps model.clientCurrentTime)
                                (RDPP.NotLoading Nothing)
                    }

                newModel =
                    GetterSetters.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


receiveSecurityGroupsAndEnsureExoGroup : SharedModel -> Project -> List OSTypes.SecurityGroup -> ( SharedModel, Cmd SharedMsg )
receiveSecurityGroupsAndEnsureExoGroup model project securityGroups =
    {- Create an "exosphere" security group unless one already exists -}
    let
        newSecurityGroups =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave securityGroups model.clientCurrentTime)
                (RDPP.NotLoading Nothing)

        newProject =
            { project | securityGroups = newSecurityGroups }

        newModel =
            GetterSetters.modelUpdateProject model newProject

        cmds =
            case List.Extra.find (\a -> a.name == "exosphere") securityGroups of
                Just exoGroup ->
                    -- check rules, ensure rules are latest set and none missing
                    -- if rules are missing, request to create them
                    -- assumes additive rules for now (i.e. add missing rules,
                    -- but do not subtract rules that shouldn't be there)
                    let
                        existingRules =
                            exoGroup.rules

                        defaultExosphereRules =
                            SecurityGroupRule.defaultExosphereRules

                        missingRules =
                            defaultExosphereRules
                                |> List.filterMap
                                    (\defaultRule ->
                                        let
                                            ruleExists =
                                                existingRules
                                                    |> List.any
                                                        (\existingRule ->
                                                            SecurityGroupRule.matchRule existingRule defaultRule
                                                        )
                                        in
                                        if ruleExists then
                                            Nothing

                                        else
                                            Just defaultRule
                                    )
                    in
                    requestCreateSecurityGroupRules
                        newProject
                        exoGroup
                        missingRules
                        "create missing rules for Exosphere security group"

                Nothing ->
                    [ requestCreateExoSecurityGroup newProject ]
    in
    ( newModel, Cmd.batch cmds )


receiveCreateExoSecurityGroupAndRequestCreateRules : SharedModel -> Project -> OSTypes.SecurityGroup -> ( SharedModel, Cmd SharedMsg )
receiveCreateExoSecurityGroupAndRequestCreateRules model project newSecGroup =
    let
        newSecGroups =
            newSecGroup
                :: (project.securityGroups |> RDPP.withDefault [])

        newProject =
            { project
                | securityGroups =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave newSecGroups model.clientCurrentTime)
                        (RDPP.NotLoading Nothing)
            }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    requestCreateExoSecurityGroupRules
        newModel
        newProject
        SecurityGroupRule.defaultExosphereRules



{- JSON Decoders -}


decodeNetworks : Decode.Decoder (List OSTypes.Network)
decodeNetworks =
    Decode.field "networks" (Decode.list networkDecoder)


networkDecoder : Decode.Decoder OSTypes.Network
networkDecoder =
    Decode.map5 OSTypes.Network
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)
        (Decode.field "router:external" Decode.bool)


decodeFloatingIps : Decode.Decoder (List OSTypes.FloatingIp)
decodeFloatingIps =
    Decode.field "floatingips" (Decode.list floatingIpDecoder)


decodeFloatingIp : Decode.Decoder OSTypes.FloatingIp
decodeFloatingIp =
    Decode.field "floatingip" floatingIpDecoder


floatingIpDecoder : Decode.Decoder OSTypes.FloatingIp
floatingIpDecoder =
    Decode.map6 OSTypes.FloatingIp
        (Decode.field "id" Decode.string)
        (Decode.field "floating_ip_address" Decode.string)
        (Decode.field "status" Decode.string
            |> Decode.andThen ipAddressStatusDecoder
        )
        (Decode.field "port_id" <| Decode.nullable Decode.string)
        (Decode.maybe <| Decode.field "dns_domain" Decode.string)
        (Decode.maybe <| Decode.field "dns_name" Decode.string)


ipAddressStatusDecoder : String -> Decode.Decoder OSTypes.IpAddressStatus
ipAddressStatusDecoder status =
    case status of
        "ACTIVE" ->
            Decode.succeed OSTypes.IpAddressActive

        "DOWN" ->
            Decode.succeed OSTypes.IpAddressDown

        "ERROR" ->
            Decode.succeed OSTypes.IpAddressError

        _ ->
            Decode.fail "unrecognised IP address type"


decodePorts : Decode.Decoder (List OSTypes.Port)
decodePorts =
    Decode.field "ports" (Decode.list portDecoder)


portDecoder : Decode.Decoder OSTypes.Port
portDecoder =
    Decode.map5 OSTypes.Port
        (Decode.field "id" Decode.string)
        (Decode.field "device_id" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)
        (Decode.field "fixed_ips"
            (Decode.list (Decode.field "ip_address" Decode.string))
        )


decodeSecurityGroups : Decode.Decoder (List OSTypes.SecurityGroup)
decodeSecurityGroups =
    Decode.field "security_groups" (Decode.list securityGroupDecoder)


decodeNewSecurityGroup : Decode.Decoder OSTypes.SecurityGroup
decodeNewSecurityGroup =
    Decode.field "security_group" securityGroupDecoder


securityGroupDecoder : Decode.Decoder OSTypes.SecurityGroup
securityGroupDecoder =
    Decode.map4 OSTypes.SecurityGroup
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "security_group_rules" (Decode.list securityGroupRuleDecoder))
