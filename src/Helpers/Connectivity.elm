module Helpers.Connectivity exposing (ConnectionEtherType(..), ConnectionPorts(..), ConnectionRemote(..), ConnectivityRule, incomingGuacamoleRule, incomingSshRule, incomingVncRule, isConnectionPermitted, outgoingDnsTcpRule, outgoingDnsUdpRule, outgoingHttpRule, outgoingHttpsRule, securityGroupRuleTemplateToConnectivtyRule)

import Helpers.String exposing (removeEmptiness)
import OpenStack.SecurityGroupRule exposing (Remote(..), SecurityGroupRule, SecurityGroupRuleDirection(..), SecurityGroupRuleEthertype, SecurityGroupRuleProtocol(..), SecurityGroupRuleTemplate, getRemote, portRangeSubsumedBy, protocolSubsumedBy, remoteMatch)
import View.Types exposing (Context)


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


outgoingHttpsRule : ConnectivityRule
outgoingHttpsRule =
    { ethertype = SomeEtherType
    , direction = Egress
    , protocol = Just ProtocolTcp
    , ports = PortRange 443 443
    , remote = SomeRemote
    , description = Just "Outgoing secure web requests (HTTPS)"
    }


outgoingHttpRule : ConnectivityRule
outgoingHttpRule =
    { ethertype = SomeEtherType
    , direction = Egress
    , protocol = Just ProtocolTcp
    , ports = PortRange 443 443
    , remote = SomeRemote
    , description = Just "Outgoing web requests (HTTP)"
    }


outgoingDnsUdpRule : ConnectivityRule
outgoingDnsUdpRule =
    { ethertype = SomeEtherType
    , direction = Egress
    , protocol = Just ProtocolUdp
    , ports = PortRange 53 53
    , remote = SomeRemote
    , description = Just "Domain name lookups (DNS)"
    }


outgoingDnsTcpRule : ConnectivityRule
outgoingDnsTcpRule =
    { ethertype = SomeEtherType
    , direction = Egress
    , protocol = Just ProtocolTcp
    , ports = PortRange 53 53
    , remote = SomeRemote
    , description = Just "Domain name lookups (DNS fallback)"
    }


incomingSshRule : Context -> ConnectivityRule
incomingSshRule context =
    { ethertype = SomeEtherType
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , ports = PortRange 22 22
    , remote = SomeRemote
    , description = Just <| "Secure " ++ context.localization.commandDrivenTextInterface ++ " login (SSH)"
    }


incomingGuacamoleRule : Context -> ConnectivityRule
incomingGuacamoleRule context =
    { ethertype = SomeEtherType
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , ports = PortRange 49528 49528
    , remote = SomeRemote
    , description =
        Just <|
            "Remote "
                ++ context.localization.commandDrivenTextInterface
                ++ " and "
                ++ context.localization.graphicalDesktopEnvironment
                ++ " (Guacamole)"
    }


incomingVncRule : Context -> ConnectivityRule
incomingVncRule context =
    { ethertype = SomeEtherType
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , ports = PortRange 5901 5901
    , remote = SomeRemote
    , description = Just <| "Remote " ++ context.localization.graphicalDesktopEnvironment ++ " (VNC)"
    }
