module Rest.Neutron exposing
    ( addFloatingIpInServerDetails
    , decodeFloatingIpCreation
    , decodeNetworks
    , decodePorts
    , networkDecoder
    , portDecoder
    , receiveCreateExoSecurityGroupAndRequestCreateRules
    , receiveCreateFloatingIp
    , receiveDeleteFloatingIp
    , receiveFloatingIps
    , receiveNetworks
    , receiveSecurityGroupsAndEnsureExoGroup
    , requestCreateExoSecurityGroupRules
    , requestCreateFloatingIp
    , requestDeleteFloatingIp
    , requestFloatingIps
    , requestNetworks
    , requestPorts
    , requestSecurityGroups
    )

import Helpers.Error exposing (ErrorContext, ErrorLevel(..))
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
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerOrigin(..)
        , ViewState(..)
        )



{- HTTP Requests -}


requestNetworks : Project -> Cmd Msg
requestNetworks project =
    let
        errorContext =
            ErrorContext
                ("get list of networks for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (Helpers.getProjectId project)
                (ReceiveNetworks errorContext result)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.neutron ++ "/v2.0/networks")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodeNetworks
        )


requestFloatingIps : Project -> Cmd Msg
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
                        (Helpers.getProjectId project)
                        (ReceiveFloatingIps ips)
                )
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.neutron ++ "/v2.0/floatingips")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            decodeFloatingIps
        )


requestPorts : Project -> Cmd Msg
requestPorts project =
    let
        errorContext =
            ErrorContext
                ("get list of ports for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (Helpers.getProjectId project)
                (ReceivePorts errorContext result)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.neutron ++ "/v2.0/ports")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodePorts
        )


requestCreateFloatingIp : Project -> OSTypes.Network -> OSTypes.Port -> Server -> Cmd Msg
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
                ("create a floating IP address on network " ++ network.name ++ "for port " ++ port_.uuid)
                ErrorCrit
                (Just "It's possible your cloud has run out of public IP address space; ask your cloud administrator.")

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\ip ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveCreateFloatingIp server.osProps.uuid ip)
                )

        requestCmd =
            openstackCredentialedRequest
                project
                Post
                Nothing
                (project.endpoints.neutron ++ "/v2.0/floatingips")
                (Http.jsonBody requestBody)
                (expectJsonWithErrorBody
                    resultToMsg_
                    decodeFloatingIpCreation
                )
    in
    requestCmd


requestDeleteFloatingIp : Project -> OSTypes.IpAddressUuid -> Cmd Msg
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
                        (Helpers.getProjectId project)
                        (ReceiveDeleteFloatingIp uuid)
                )
    in
    openstackCredentialedRequest
        project
        Delete
        Nothing
        (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ uuid)
        Http.emptyBody
        (expectStringWithErrorBody
            resultToMsg_
        )


requestSecurityGroups : Project -> Cmd Msg
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
                        (Helpers.getProjectId project)
                        (ReceiveSecurityGroups groups)
                )
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            decodeSecurityGroups
        )


requestCreateExoSecurityGroup : Project -> Cmd Msg
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
                        (Helpers.getProjectId project)
                        (ReceiveCreateExoSecurityGroup group)
                )
    in
    openstackCredentialedRequest
        project
        Post
        Nothing
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg_
            decodeNewSecurityGroup
        )


requestCreateExoSecurityGroupRules : Model -> Project -> List SecurityGroupRule -> ( Model, Cmd Msg )
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


requestCreateSecurityGroupRules : Project -> OSTypes.SecurityGroup -> List SecurityGroupRule -> String -> List (Cmd Msg)
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
                project
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


receiveNetworks : Model -> Project -> List OSTypes.Network -> ( Model, Cmd Msg )
receiveNetworks model project networks =
    let
        newProject =
            let
                newNetsRDPP =
                    RDPP.RemoteDataPlusPlus (RDPP.DoHave networks model.clientCurrentTime) (RDPP.NotLoading Nothing)
            in
            { project | networks = newNetsRDPP }

        -- If we have a CreateServerRequest with no network UUID, populate it with a reasonable guess of a private network.
        -- Same comments above (in receiveFlavors) apply here.
        viewState =
            case model.viewState of
                ProjectView _ viewParams projectViewConstructor ->
                    case projectViewConstructor of
                        CreateServer createServerViewParams ->
                            if createServerViewParams.networkUuid == "" then
                                let
                                    defaultNetUuid =
                                        case Helpers.newServerNetworkOptions newProject of
                                            NoNetsAutoAllocate ->
                                                "auto"

                                            OneNet net ->
                                                net.uuid

                                            MultipleNetsWithGuess _ guessNet _ ->
                                                guessNet.uuid
                                in
                                ProjectView
                                    (Helpers.getProjectId project)
                                    viewParams
                                    (CreateServer
                                        { createServerViewParams
                                            | networkUuid = defaultNetUuid
                                        }
                                    )

                            else
                                model.viewState

                        _ ->
                            model.viewState

                _ ->
                    model.viewState

        newModel =
            Helpers.modelUpdateProject { model | viewState = viewState } newProject
    in
    ( newModel, Cmd.none )


receiveFloatingIps : Model -> Project -> List OSTypes.IpAddress -> ( Model, Cmd Msg )
receiveFloatingIps model project floatingIps =
    let
        newProject =
            { project | floatingIps = floatingIps }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveCreateFloatingIp : Model -> Project -> OSTypes.ServerUuid -> OSTypes.IpAddress -> ( Model, Cmd Msg )
receiveCreateFloatingIp model project serverUuid ipAddress =
    case Helpers.serverLookup project serverUuid of
        Nothing ->
            -- No server found, may have been deleted, nothing to do
            ( model, Cmd.none )

        Just server ->
            {- This repeats a lot of code in receiveCockpitStatus, badly needs a refactor -}
            let
                newServer =
                    let
                        oldOSProps =
                            server.osProps

                        oldExoProps =
                            server.exoProps

                        details =
                            addFloatingIpInServerDetails
                                server.osProps.details
                                ipAddress
                    in
                    Server
                        { oldOSProps | details = details }
                        { oldExoProps | priorFloatingIpState = Success }

                newProject =
                    Helpers.projectUpdateServer project newServer

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )


receiveDeleteFloatingIp : Model -> Project -> OSTypes.IpAddressUuid -> ( Model, Cmd Msg )
receiveDeleteFloatingIp model project uuid =
    let
        newFloatingIps =
            List.filter (\f -> f.uuid /= Just uuid) project.floatingIps

        newProject =
            { project | floatingIps = newFloatingIps }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


addFloatingIpInServerDetails : OSTypes.ServerDetails -> OSTypes.IpAddress -> OSTypes.ServerDetails
addFloatingIpInServerDetails details ipAddress =
    let
        newIps =
            ipAddress :: details.ipAddresses
    in
    { details | ipAddresses = newIps }


receiveSecurityGroupsAndEnsureExoGroup : Model -> Project -> List OSTypes.SecurityGroup -> ( Model, Cmd Msg )
receiveSecurityGroupsAndEnsureExoGroup model project securityGroups =
    {- Create an "exosphere" security group unless one already exists -}
    let
        newProject =
            { project | securityGroups = securityGroups }

        newModel =
            Helpers.modelUpdateProject model newProject

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


receiveCreateExoSecurityGroupAndRequestCreateRules : Model -> Project -> OSTypes.SecurityGroup -> ( Model, Cmd Msg )
receiveCreateExoSecurityGroupAndRequestCreateRules model project newSecGroup =
    let
        newSecGroups =
            newSecGroup :: project.securityGroups

        newProject =
            { project | securityGroups = newSecGroups }

        newModel =
            Helpers.modelUpdateProject model newProject
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


decodeFloatingIps : Decode.Decoder (List OSTypes.IpAddress)
decodeFloatingIps =
    Decode.field "floatingips" (Decode.list floatingIpDecoder)


floatingIpDecoder : Decode.Decoder OSTypes.IpAddress
floatingIpDecoder =
    Decode.map3 OSTypes.IpAddress
        (Decode.field "id" Decode.string |> Decode.map (\i -> Just i))
        (Decode.field "floating_ip_address" Decode.string)
        (Decode.succeed OSTypes.IpAddressFloating)


decodePorts : Decode.Decoder (List OSTypes.Port)
decodePorts =
    Decode.field "ports" (Decode.list portDecoder)


portDecoder : Decode.Decoder OSTypes.Port
portDecoder =
    Decode.map4 OSTypes.Port
        (Decode.field "id" Decode.string)
        (Decode.field "device_id" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)


decodeFloatingIpCreation : Decode.Decoder OSTypes.IpAddress
decodeFloatingIpCreation =
    Decode.map3 OSTypes.IpAddress
        (Decode.at [ "floatingip", "id" ] Decode.string |> Decode.map (\i -> Just i))
        (Decode.at [ "floatingip", "floating_ip_address" ] Decode.string)
        (Decode.succeed OSTypes.IpAddressFloating)


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
