module Rest.Neutron exposing
    ( NeutronError
    , neutronErrorDecoder
    , receiveCreateDefaultSecurityGroupAndRequestCreateRules
    , receiveCreateFloatingIp
    , receiveDeleteFloatingIp
    , receiveFloatingIps
    , receiveNetworks
    , receiveSecurityGroupsAndEnsureDefaultGroup
    , reconcileSecurityGroupRules
    , requestAssignFloatingIp
    , requestAutoAllocatedNetwork
    , requestCreateFloatingIp
    , requestDeleteFloatingIp
    , requestFloatingIps
    , requestNetworks
    , requestPorts
    , requestSecurityGroups
    , requestUnassignFloatingIp
    , requestUpdateSecurityGroup
    , requestUpdateSecurityGroupRules
    , requestUpdateSecurityGroupTags
    , securityGroupsDecoder
    )

import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Time
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import OpenStack.SecurityGroupRule as SecurityGroupRule exposing (SecurityGroupRule, SecurityGroupRuleUuid, securityGroupRuleDecoder)
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
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))



{- HTTP Requests -}


{-| The shape of an error response from Neutron:

    {
        "NeutronError": {
            "type": "SecurityGroupProtocolRequiredWithPorts",
            "message": "Must also specify protocol if port range is given.",
            "detail": ""
        }
    }

-}
type alias NeutronError =
    { message : String }


neutronErrorDecoder : Decode.Decoder NeutronError
neutronErrorDecoder =
    Decode.map NeutronError <| Decode.field "message" Decode.string


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
            networksDecoder
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
            floatingIpsDecoder
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
            portsDecoder
        )


requestCreateFloatingIp : Project -> OSTypes.Network -> Maybe ( OSTypes.Port, OSTypes.ServerUuid ) -> Maybe OSTypes.IpAddressValue -> Cmd SharedMsg
requestCreateFloatingIp project network maybePortServerUuid maybeIp =
    let
        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object <|
                        List.filterMap identity
                            [ Just ( "floating_network_id", Encode.string network.uuid )
                            , maybePortServerUuid
                                |> Maybe.map Tuple.first
                                |> Maybe.map (\port_ -> ( "port_id", Encode.string port_.uuid ))
                            , maybeIp
                                |> Maybe.map (\ip -> ( "floating_ip_address", Encode.string ip ))
                            ]
                  )
                ]

        errorContext =
            let
                forPort =
                    maybePortServerUuid
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
                    case maybePortServerUuid of
                        Just ( _, serverUuid ) ->
                            ServerMsg serverUuid <|
                                ReceiveCreateServerFloatingIp errorContext result

                        Nothing ->
                            ReceiveCreateProjectFloatingIp errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/floatingips")
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg_
            floatingIpDecoder
        )


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
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ floatingIpUuid)
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg_
            floatingIpDecoder
        )


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
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ floatingIpUuid)
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg_
            floatingIpDecoder
        )


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
            securityGroupsDecoder
        )


requestCreateDefaultSecurityGroup : Project -> OSTypes.SecurityGroupTemplate -> Cmd SharedMsg
requestCreateDefaultSecurityGroup project securityGroup =
    let
        requestBody =
            Encode.object
                [ ( "security_group"
                  , Encode.object
                        [ ( "name", Encode.string securityGroup.name )
                        , ( "description", Encode.string <| Maybe.withDefault "" securityGroup.description )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create default security group for Exosphere in project " ++ project.auth.project.name)
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCreateDefaultSecurityGroup errorContext result securityGroup)
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
            securityGroupDecoder
        )


requestUpdateSecurityGroup :
    Project
    -> OSTypes.SecurityGroupUuid
    ->
        { a
            | name : String
            , description : Maybe String
        }
    -> Cmd SharedMsg
requestUpdateSecurityGroup project securityGroupUuid securityGroupUpdate =
    let
        requestBody =
            Encode.object
                [ ( "security_group"
                  , Encode.object
                        [ ( "name", Encode.string securityGroupUpdate.name )
                        , ( "description", Encode.string <| Maybe.withDefault "" securityGroupUpdate.description )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("update security group uuid " ++ securityGroupUuid ++ " in project " ++ project.auth.project.name)
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveUpdateSecurityGroup errorContext securityGroupUuid result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/security-groups/" ++ securityGroupUuid)
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg
            securityGroupDecoder
        )


requestUpdateSecurityGroupTags : Project -> OSTypes.SecurityGroupUuid -> List OSTypes.SecurityGroupTag -> Cmd SharedMsg
requestUpdateSecurityGroupTags project securityGroupUuid tags =
    let
        requestBody =
            Encode.object
                [ ( "tags", Encode.list Encode.string tags )
                ]

        errorContext =
            ErrorContext
                ("update tags for security group uuid " ++ securityGroupUuid ++ " in project " ++ project.auth.project.name)
                ErrorCrit
                Nothing

        resultToMsg =
            resultToMsgErrorBody
                errorContext
                (\t ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveUpdateSecurityGroupTags ( securityGroupUuid, t ))
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/security-groups/" ++ securityGroupUuid ++ "/tags")
        (Http.jsonBody requestBody)
        (expectJsonWithErrorBody
            resultToMsg
            tagDecoder
        )


requestCreateSecurityGroupRules : Project -> OSTypes.SecurityGroupUuid -> List SecurityGroupRule -> String -> List (Cmd SharedMsg)
requestCreateSecurityGroupRules project securityGroupUuid rules errorMessage =
    let
        errorContext =
            ErrorContext
                errorMessage
                --"create rules for Exosphere security group"
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCreateSecurityGroupRule errorContext securityGroupUuid result)

        buildRequestCmd body =
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Post
                Nothing
                []
                (project.endpoints.neutron ++ "/v2.0/security-group-rules")
                (Http.jsonBody body)
                (expectJsonWithErrorBody
                    resultToMsg
                    (Decode.at [ "security_group_rule" ] <| securityGroupRuleDecoder)
                )

        bodies =
            rules
                |> List.map (SecurityGroupRule.encode securityGroupUuid)
    in
    bodies |> List.map buildRequestCmd


requestDeleteSecurityGroupRules : Project -> OSTypes.SecurityGroupUuid -> List SecurityGroupRuleUuid -> String -> List (Cmd SharedMsg)
requestDeleteSecurityGroupRules project securityGroupUuid ruleUuids errorMessage =
    let
        errorContext =
            ErrorContext
                errorMessage
                ErrorWarn
                Nothing

        buildRequestCmd ruleUuid =
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Delete
                Nothing
                []
                (project.endpoints.neutron ++ "/v2.0/security-group-rules/" ++ ruleUuid)
                Http.emptyBody
                (Http.expectWhatever
                    (\result -> ProjectMsg (GetterSetters.projectIdentifier project) <| ReceiveDeleteSecurityGroupRule errorContext ( securityGroupUuid, ruleUuid ) result)
                )
    in
    ruleUuids |> List.map buildRequestCmd



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


receiveSecurityGroupsAndEnsureDefaultGroup : SharedModel -> Project -> List OSTypes.SecurityGroup -> ( SharedModel, Cmd SharedMsg )
receiveSecurityGroupsAndEnsureDefaultGroup model project securityGroups =
    {- Create a default security group unless one already exists -}
    let
        newSecurityGroups =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave securityGroups model.clientCurrentTime)
                (RDPP.NotLoading Nothing)

        newProject =
            { project | securityGroups = newSecurityGroups }

        defaultSecurityGroup : OSTypes.SecurityGroupTemplate
        defaultSecurityGroup =
            GetterSetters.projectDefaultSecurityGroup model.viewContext project

        newModel =
            GetterSetters.modelUpdateProject model newProject

        cmds =
            case List.Extra.find (\a -> a.name == defaultSecurityGroup.name) securityGroups of
                Just defaultGroup ->
                    reconcileDefaultSecurityGroupRules
                        newProject
                        defaultGroup
                        defaultSecurityGroup.rules

                Nothing ->
                    [ requestCreateDefaultSecurityGroup newProject defaultSecurityGroup ]
    in
    ( newModel, Cmd.batch cmds )


requestUpdateSecurityGroupRules : Project -> OSTypes.SecurityGroup -> List SecurityGroupRule.SecurityGroupRuleTemplate -> List (Cmd SharedMsg)
requestUpdateSecurityGroupRules project securityGroup updatedRules =
    let
        existingRules =
            securityGroup.rules

        expectedRules =
            updatedRules |> List.map SecurityGroupRule.securityGroupRuleTemplateToRule
    in
    reconcileSecurityGroupRules project securityGroup existingRules expectedRules


{-| Check default rules, ensure rules are the latest set & none are missing compared to the default rules template.

  - If rules are missing, request to create them.
  - If there are extra rules, request to delete them.
    (Especially since the [default OpenStack Networking security group rules](https://docs.openstack.org/api-ref/network/v2/index.html#list-security-group-default-rules)
    are added to all new security groups, and those might differ from our application default rules.)

-}
reconcileDefaultSecurityGroupRules : Project -> OSTypes.SecurityGroup -> List SecurityGroupRule.SecurityGroupRuleTemplate -> List (Cmd SharedMsg)
reconcileDefaultSecurityGroupRules =
    requestUpdateSecurityGroupRules


{-| Compare existing & updated security group rules:

  - If rules are missing, request to create them.
  - If there are extra rules, request to delete them.

-}
reconcileSecurityGroupRules : Project -> OSTypes.SecurityGroup -> List SecurityGroupRule -> List SecurityGroupRule -> List (Cmd SharedMsg)
reconcileSecurityGroupRules project securityGroup existingRules updatedRules =
    let
        { missing, extra } =
            SecurityGroupRule.compareSecurityGroupRuleLists existingRules updatedRules
    in
    requestCreateSecurityGroupRules
        project
        securityGroup.uuid
        missing
        ("create missing rules for " ++ securityGroup.name ++ " security group")
        ++ requestDeleteSecurityGroupRules
            project
            securityGroup.uuid
            (extra |> List.map .uuid)
            ("remove extra rules from " ++ securityGroup.name ++ " security group")


receiveCreateDefaultSecurityGroupAndRequestCreateRules : SharedModel -> Project -> OSTypes.SecurityGroup -> OSTypes.SecurityGroupTemplate -> ( SharedModel, Cmd SharedMsg )
receiveCreateDefaultSecurityGroupAndRequestCreateRules model project defaultGroup securityGroupTemplate =
    let
        newSecGroups =
            defaultGroup
                :: (project.securityGroups |> RDPP.withDefault [])

        newProject =
            { project
                | securityGroups =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave newSecGroups model.clientCurrentTime)
                        (RDPP.NotLoading Nothing)
            }
    in
    ( model
    , Cmd.batch <|
        -- All security groups are created with the project's OpenStack default security group rules.
        -- Any overlapping rules will cause a SecurityGroupRuleExists 409 ConflictException.
        -- So we create rules based on the difference, even for brand new groups.
        reconcileDefaultSecurityGroupRules
            newProject
            defaultGroup
            securityGroupTemplate.rules
    )



{- JSON Decoders -}


networksDecoder : Decode.Decoder (List OSTypes.Network)
networksDecoder =
    Decode.field "networks" (Decode.list networkDecoder)


networkDecoder : Decode.Decoder OSTypes.Network
networkDecoder =
    Decode.map5 OSTypes.Network
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)
        (Decode.field "router:external" Decode.bool)


floatingIpsDecoder : Decode.Decoder (List OSTypes.FloatingIp)
floatingIpsDecoder =
    Decode.field "floatingips" (Decode.list floatingIpValueDecoder)


floatingIpDecoder : Decode.Decoder OSTypes.FloatingIp
floatingIpDecoder =
    Decode.field "floatingip" floatingIpValueDecoder


floatingIpValueDecoder : Decode.Decoder OSTypes.FloatingIp
floatingIpValueDecoder =
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


portsDecoder : Decode.Decoder (List OSTypes.Port)
portsDecoder =
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


securityGroupsDecoder : Decode.Decoder (List OSTypes.SecurityGroup)
securityGroupsDecoder =
    Decode.field "security_groups" (Decode.list securityGroupValueDecoder)


securityGroupDecoder : Decode.Decoder OSTypes.SecurityGroup
securityGroupDecoder =
    Decode.field "security_group" securityGroupValueDecoder


tagDecoder : Decode.Decoder (List String)
tagDecoder =
    Decode.field "tags" (Decode.list Decode.string)


securityGroupValueDecoder : Decode.Decoder OSTypes.SecurityGroup
securityGroupValueDecoder =
    Decode.map6 OSTypes.SecurityGroup
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "security_group_rules" (Decode.list securityGroupRuleDecoder))
        (Decode.field "created_at" (Decode.string |> Decode.andThen Helpers.Time.makeIso8601StringToPosixDecoder))
        tagDecoder
