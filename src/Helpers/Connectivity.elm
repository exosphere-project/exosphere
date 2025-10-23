module Helpers.Connectivity exposing (ConnectionEtherType(..), ConnectionPorts(..), ConnectionRemote(..), ConnectivityRule, isConnectionPermitted, securityGroupRuleTemplateToConnectivtyRule)

import Helpers.String exposing (removeEmptiness)
import OpenStack.SecurityGroupRule exposing (Remote(..), SecurityGroupRule, SecurityGroupRuleDirection, SecurityGroupRuleEthertype, SecurityGroupRuleProtocol, SecurityGroupRuleTemplate, getRemote, portRangeSubsumedBy, protocolSubsumedBy, remoteMatch)


type ConnectionEtherType
    = SpecificEtherType SecurityGroupRuleEthertype
    | SomeEtherType


type ConnectionRemote
    = AllRemotes
    | SpecificRemote Remote
    | SomeRemote


type ConnectionPorts
    = AllPorts
    | PortRange Int Int
    | SomePort


type alias ConnectivityRule =
    { ethertype : ConnectionEtherType
    , direction : SecurityGroupRuleDirection
    , protocol : Maybe SecurityGroupRuleProtocol
    , ports : ConnectionPorts
    , remote : ConnectionRemote
    , description : Maybe String
    }


securityGroupRuleTemplateToConnectivtyRule : SecurityGroupRuleTemplate -> ConnectivityRule
securityGroupRuleTemplateToConnectivtyRule { ethertype, direction, protocol, portRangeMin, portRangeMax, remoteIpPrefix, remoteGroupUuid, description } =
    { ethertype = SpecificEtherType ethertype
    , direction = direction
    , protocol = protocol
    , ports =
        case ( portRangeMin, portRangeMax ) of
            ( Just min, Just max ) ->
                PortRange min max

            ( Nothing, Nothing ) ->
                -- Rules often don't have a port range & "at least some port" is a more intuitive connectivity conversion.
                SomePort

            -- These single unbounded ranges shouldn't occur in real life.
            ( Just _, Nothing ) ->
                SomePort

            ( Nothing, Just _ ) ->
                SomePort
    , remote =
        case ( remoteIpPrefix, remoteGroupUuid ) of
            ( Just ipPrefix, Nothing ) ->
                SpecificRemote (RemoteIpPrefix ipPrefix)

            ( Nothing, Just groupUuid ) ->
                SpecificRemote (RemoteGroupUuid groupUuid)

            ( Nothing, Nothing ) ->
                -- Rules often don't have a remote & "at least some remote" is a more intuitive connectivity conversion.
                SomeRemote

            ( Just _, Just _ ) ->
                -- Should not happen since IP & remote group are mutually exclusive.
                SomeRemote
    , description = removeEmptiness description
    }


isConnectionPermitted : ConnectivityRule -> List SecurityGroupRule -> Bool
isConnectionPermitted connection rules =
    rules
        |> List.any
            (\rule ->
                etherTypePermittedBy connection.ethertype rule.ethertype
                    && (rule.direction == connection.direction)
                    && protocolSubsumedBy connection.protocol rule.protocol
                    && portsPermittedBy connection.ports ( rule.portRangeMin, rule.portRangeMax )
                    && remotePermittedBy connection.remote rule
            )


etherTypePermittedBy : ConnectionEtherType -> SecurityGroupRuleEthertype -> Bool
etherTypePermittedBy connectionEtherType ruleEtherType =
    case connectionEtherType of
        SpecificEtherType ethertype ->
            ethertype == ruleEtherType

        SomeEtherType ->
            True


portsPermittedBy : ConnectionPorts -> ( Maybe Int, Maybe Int ) -> Bool
portsPermittedBy connectionPorts ( portMin, portMax ) =
    case ( connectionPorts, portMin, portMax ) of
        ( PortRange min max, Just _, Just _ ) ->
            portRangeSubsumedBy ( Just min, Just max ) ( portMin, portMax )

        ( SomePort, Just _, Just _ ) ->
            -- Any single port is permitted.
            True

        ( _, Nothing, Nothing ) ->
            -- All ports are permitted.
            True

        ( AllPorts, Just 0, Just 65535 ) ->
            True

        _ ->
            False


remotePermittedBy : ConnectionRemote -> SecurityGroupRule -> Bool
remotePermittedBy remoteA rule =
    let
        remoteB =
            getRemote rule
    in
    case ( remoteA, remoteB ) of
        ( _, Nothing ) ->
            -- Rule allows all connections.
            True

        ( SomeRemote, Just _ ) ->
            -- Any remote connectivity will do.
            True

        ( AllRemotes, Just _ ) ->
            -- Rule is too narrow.
            False

        ( SpecificRemote a, Just b ) ->
            remoteMatch a b
