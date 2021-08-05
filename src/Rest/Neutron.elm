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
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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
import Types.Msg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))
import Types.OuterModel exposing (OuterModel)
import Types.Project exposing (Project)
import Types.Server exposing (NewServerNetworkOptions(..), Server, ServerOrigin(..))
import Types.Types exposing (SharedModel)
import Types.View exposing (ProjectViewConstructor(..), ViewState(..))



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
                project.auth.project.uuid
                (ReceiveNetworks errorContext result)
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
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
                project.auth.project.uuid
                (ReceiveAutoAllocatedNetwork errorContext result)
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
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
                        project.auth.project.uuid
                        (ReceiveFloatingIps ips)
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
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
                project.auth.project.uuid
                (ReceivePorts errorContext result)
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
        (project.endpoints.neutron ++ "/v2.0/ports")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodePorts
        )


requestCreateFloatingIp : Project -> OSTypes.Network -> OSTypes.Port -> Server -> Cmd SharedMsg
requestCreateFloatingIp project network port_ server =
    let
        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object
                        [ ( "floating_network_id", Encode.string network.uuid )
                        , ( "port_id", Encode.string port_.uuid )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create a floating IP address on network " ++ network.name ++ " for port " ++ port_.uuid)
                ErrorCrit
                (Just "It's possible your cloud has run out of public IP address space; ask your cloud administrator.")

        resultToMsg_ =
            \result ->
                ProjectMsg project.auth.project.uuid <|
                    ServerMsg server.osProps.uuid <|
                        ReceiveCreateFloatingIp errorContext result

        requestCmd =
            openstackCredentialedRequest
                project.auth.project.uuid
                Post
                Nothing
                (project.endpoints.neutron ++ "/v2.0/floatingips")
                (Http.jsonBody requestBody)
                (expectJsonWithErrorBody
                    resultToMsg_
                    decodeFloatingIp
                )
    in
    requestCmd


requestDeleteFloatingIp : Project -> OSTypes.IpAddressUuid -> Cmd SharedMsg
requestDeleteFloatingIp project uuid =
    let
        errorContext =
            ErrorContext
                ("delete floating IP address with UUID " ++ uuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveDeleteFloatingIp uuid)
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Delete
        Nothing
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
                        project.auth.project.uuid
                        (ReceiveAssignFloatingIp ip)
                )

        requestCmd =
            openstackCredentialedRequest
                project.auth.project.uuid
                Put
                Nothing
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
                        project.auth.project.uuid
                        (ReceiveUnassignFloatingIp ip)
                )

        requestCmd =
            openstackCredentialedRequest
                project.auth.project.uuid
                Put
                Nothing
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

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\groups ->
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveSecurityGroups groups)
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
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

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\group ->
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveCreateExoSecurityGroup group)
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Post
        Nothing
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg_
            decodeNewSecurityGroup
        )


requestCreateExoSecurityGroupRules : SharedModel -> Project -> List SecurityGroupRule -> ( SharedModel, Cmd SharedMsg )
requestCreateExoSecurityGroupRules model project rules =
    let
        maybeSecurityGroup =
            List.filter (\g -> g.name == "exosphere") project.securityGroups |> List.head
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
                project.auth.project.uuid
                Post
                Nothing
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


receiveNetworks : OuterModel -> Project -> List OSTypes.Network -> ( OuterModel, Cmd SharedMsg )
receiveNetworks outerModel project networks =
    -- TODO this code should not care about view state
    let
        newProject =
            let
                newNetsRDPP =
                    RDPP.RemoteDataPlusPlus (RDPP.DoHave networks outerModel.sharedModel.clientCurrentTime) (RDPP.NotLoading Nothing)
            in
            { project | networks = newNetsRDPP }

        -- If we have a CreateServerRequest with no network UUID, populate it with a reasonable guess of a private network.
        -- Same comments above (in receiveFlavors) apply here.
        viewState =
            case outerModel.viewState of
                ProjectView _ viewParams projectViewConstructor ->
                    case projectViewConstructor of
                        CreateServer createServerViewParams ->
                            if createServerViewParams.networkUuid == Nothing then
                                case Helpers.newServerNetworkOptions newProject of
                                    AutoSelectedNetwork netUuid ->
                                        ProjectView
                                            project.auth.project.uuid
                                            viewParams
                                            (CreateServer
                                                { createServerViewParams
                                                    | networkUuid = Just netUuid
                                                }
                                            )

                                    _ ->
                                        outerModel.viewState

                            else
                                outerModel.viewState

                        _ ->
                            outerModel.viewState

                _ ->
                    outerModel.viewState

        newSharedModel =
            GetterSetters.modelUpdateProject outerModel.sharedModel newProject
    in
    ( { outerModel | viewState = viewState, sharedModel = newSharedModel }, Cmd.none )


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


receiveCreateFloatingIp : SharedModel -> Project -> Server -> OSTypes.FloatingIp -> ( SharedModel, Cmd SharedMsg )
receiveCreateFloatingIp model project server floatingIp =
    let
        newServer =
            let
                oldExoProps =
                    server.exoProps
            in
            { server
                | exoProps = { oldExoProps | floatingIpCreationOption = DoNotUseFloatingIp }
            }

        projectUpdatedServer =
            GetterSetters.projectUpdateServer project newServer

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
        newProject =
            { project | securityGroups = securityGroups }

        newModel =
            GetterSetters.modelUpdateProject model newProject

        cmds =
            case List.filter (\a -> a.name == "exosphere") securityGroups |> List.head of
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
            newSecGroup :: project.securityGroups

        newProject =
            { project | securityGroups = newSecGroups }

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
    Decode.map4 OSTypes.FloatingIp
        (Decode.field "id" Decode.string)
        (Decode.field "floating_ip_address" Decode.string)
        (Decode.field "status" Decode.string
            |> Decode.andThen ipAddressStatusDecoder
        )
        (Decode.field "port_id" <| Decode.nullable Decode.string)


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
