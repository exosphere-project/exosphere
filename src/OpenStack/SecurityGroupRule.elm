module OpenStack.SecurityGroupRule exposing
    ( SecurityGroupRule
    , SecurityGroupRuleDirection(..)
    , SecurityGroupRuleEthertype(..)
    , SecurityGroupRuleProtocol(..)
    , SecurityGroupRuleUuid
    , SecurityGroupUuid
    , defaultExosphereRules
    , directionToString
    , encode
    , etherTypeToString
    , matchRule
    , portRangeToString
    , protocolToString
    , securityGroupRuleDecoder
    , securityGroupRuleDiff
    , stringToSecurityGroupRuleProtocol
    )

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import String


type alias SecurityGroupRule =
    { uuid : SecurityGroupRuleUuid
    , ethertype : SecurityGroupRuleEthertype
    , direction : SecurityGroupRuleDirection
    , protocol : Maybe SecurityGroupRuleProtocol
    , port_range_min : Maybe Int
    , port_range_max : Maybe Int
    , remoteIpPrefix : Maybe String -- TODO: Encode remote IP prefix in requests.
    , remoteGroupUuid : Maybe SecurityGroupRuleUuid -- TODO: Encode remote security group in requests.
    , description : Maybe String
    }


matchRule : SecurityGroupRule -> SecurityGroupRule -> Bool
matchRule ruleA ruleB =
    (ruleA.ethertype == ruleB.ethertype)
        && (ruleA.direction == ruleB.direction)
        && (ruleA.protocol == ruleB.protocol)
        && (ruleA.port_range_min == ruleB.port_range_min)
        && (ruleA.port_range_max == ruleB.port_range_max)


buildRuleTCP : Int -> String -> SecurityGroupRule
buildRuleTCP portNumber description =
    { uuid = ""
    , ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , port_range_min = Just portNumber
    , port_range_max = Just portNumber
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just description
    }


buildRuleIcmp : SecurityGroupRule
buildRuleIcmp =
    { uuid = ""
    , ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolIcmp
    , port_range_min = Nothing
    , port_range_max = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Ping"
    }


buildRuleMosh : SecurityGroupRule
buildRuleMosh =
    { uuid = ""
    , ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolUdp
    , port_range_min = Just 60000
    , port_range_max = Just 61000
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Mosh"
    }


buildRuleExposeAllIncomingPorts : SecurityGroupRule
buildRuleExposeAllIncomingPorts =
    { uuid = ""
    , ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , port_range_min = Nothing
    , port_range_max = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Expose all incoming ports"
    }


buildRuleAllowAllOutgoingIPv4 : SecurityGroupRule
buildRuleAllowAllOutgoingIPv4 =
    { uuid = ""
    , ethertype = Ipv4
    , direction = Egress
    , protocol = Nothing
    , port_range_min = Nothing
    , port_range_max = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Allow all outgoing IPv4 traffic"
    }


buildRuleAllowAllOutgoingIPv6 : SecurityGroupRule
buildRuleAllowAllOutgoingIPv6 =
    { uuid = ""
    , ethertype = Ipv6
    , direction = Egress
    , protocol = Nothing
    , port_range_min = Nothing
    , port_range_max = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Allow all outgoing IPv6 traffic"
    }


defaultExosphereRules : List SecurityGroupRule
defaultExosphereRules =
    [ buildRuleTCP 22 "SSH"
    , buildRuleIcmp
    , buildRuleMosh
    , buildRuleExposeAllIncomingPorts
    , buildRuleAllowAllOutgoingIPv4
    , buildRuleAllowAllOutgoingIPv6
    ]


{-| Returns rules that are in the first list but not in the second list. (Difference read as A minus B.)
-}
securityGroupRuleDiff : List SecurityGroupRule -> List SecurityGroupRule -> List SecurityGroupRule
securityGroupRuleDiff rulesA rulesB =
    rulesA
        |> List.filterMap
            (\defaultRule ->
                let
                    ruleExists =
                        rulesB
                            |> List.any
                                (\existingRule ->
                                    matchRule existingRule defaultRule
                                )
                in
                if ruleExists then
                    Nothing

                else
                    Just defaultRule
            )


type alias SecurityGroupRuleUuid =
    String


type alias SecurityGroupUuid =
    String


type SecurityGroupRuleDirection
    = Ingress
    | Egress
    | UnsupportedDirection String


type SecurityGroupRuleEthertype
    = Ipv4
    | Ipv6
    | UnsupportedEthertype String


type SecurityGroupRuleProtocol
    = AnyProtocol
    | ProtocolIcmp
    | ProtcolIcmpv6
    | ProtocolTcp
    | ProtocolUdp
    | ProtocolAh
    | ProtocolDccp
    | ProtocolEgp
    | ProtocolEsp
    | ProtocolGre
    | ProtocolIgmp
    | ProtocolIpv6Encap
    | ProtocolIpv6Frag
    | ProtocolIpv6Nonxt
    | ProtocolIpv6Opts
    | ProtocolIpv6Route
    | ProtocolOspf
    | ProtocolPgm
    | ProtocolRsvp
    | ProtocolSctp
    | ProtocolUdpLite
    | ProtocolVrrp
    | UnsupportedProtocol String


type PortRangeType
    = PortRangeMin
    | PortRangeMax


encode : SecurityGroupUuid -> SecurityGroupRule -> Encode.Value
encode securityGroupUuid { ethertype, direction, protocol, port_range_min, port_range_max, description } =
    Encode.object
        [ ( "security_group_rule"
          , [ ( "security_group_id", Encode.string securityGroupUuid ) ]
                |> encodeEthertype ethertype
                |> encodeDirection direction
                |> encodeProtocol protocol
                |> encodePort port_range_min PortRangeMin
                |> encodePort port_range_max PortRangeMax
                |> encodeDescription description
                |> Encode.object
          )
        ]


encodeDescription : Maybe String -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeDescription maybeDescription object =
    case maybeDescription of
        Just description ->
            ( "description", Encode.string description ) :: object

        Nothing ->
            object


portRangeToString :
    { a
        | port_range_min : Maybe Int
        , port_range_max : Maybe Int
    }
    -> String
portRangeToString { port_range_min, port_range_max } =
    case ( port_range_min, port_range_max ) of
        ( Just min, Just max ) ->
            if min == max then
                String.fromInt min

            else
                String.fromInt min ++ " - " ++ String.fromInt max

        ( Nothing, Nothing ) ->
            "Any"

        -- These cases of single unbounded ranges shouldn't occur in real life.
        ( Just min, Nothing ) ->
            String.fromInt min ++ " - "

        ( Nothing, Just max ) ->
            " - " ++ String.fromInt max


encodePort : Maybe Int -> PortRangeType -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodePort maybePort portRangeType object =
    case maybePort of
        Just portNumber ->
            case portRangeType of
                PortRangeMin ->
                    ( "port_range_min", portNumber |> String.fromInt |> Encode.string ) :: object

                PortRangeMax ->
                    ( "port_range_max", portNumber |> String.fromInt |> Encode.string ) :: object

        Nothing ->
            object


protocolToString : SecurityGroupRuleProtocol -> String
protocolToString protocol =
    case protocol of
        AnyProtocol ->
            "Any"

        ProtocolIcmp ->
            "ICMP"

        ProtcolIcmpv6 ->
            "ICMPv6"

        ProtocolTcp ->
            "TCP"

        ProtocolUdp ->
            "UDP"

        ProtocolAh ->
            "AH"

        ProtocolDccp ->
            "DCCP"

        ProtocolEgp ->
            "EGP"

        ProtocolEsp ->
            "ESP"

        ProtocolGre ->
            "GRE"

        ProtocolIgmp ->
            "IGMP"

        ProtocolIpv6Encap ->
            "IPv6-Encap"

        ProtocolIpv6Frag ->
            "IPv6-Frag"

        ProtocolIpv6Nonxt ->
            "IPv6-Nonxt"

        ProtocolIpv6Opts ->
            "IPv6-Opts"

        ProtocolIpv6Route ->
            "IPv6-Route"

        ProtocolOspf ->
            "OSPF"

        ProtocolPgm ->
            "PGM"

        ProtocolRsvp ->
            "RSVP"

        ProtocolSctp ->
            "SCTP"

        ProtocolUdpLite ->
            "UDPLite"

        ProtocolVrrp ->
            "VRRP"

        UnsupportedProtocol str ->
            str


stringToSecurityGroupRuleProtocol : String -> SecurityGroupRuleProtocol
stringToSecurityGroupRuleProtocol protocol =
    case protocol of
        "any" ->
            AnyProtocol

        "icmp" ->
            ProtocolIcmp

        "icmpv6" ->
            ProtcolIcmpv6

        "ipv6-icmp" ->
            ProtcolIcmpv6

        "tcp" ->
            ProtocolTcp

        "udp" ->
            ProtocolUdp

        "ah" ->
            ProtocolAh

        "dccp" ->
            ProtocolDccp

        "egp" ->
            ProtocolEgp

        "esp" ->
            ProtocolEsp

        "gre" ->
            ProtocolGre

        "igmp" ->
            ProtocolIgmp

        "ipv6-encap" ->
            ProtocolIpv6Encap

        "ipv6-frag" ->
            ProtocolIpv6Frag

        "ipv6-nonxt" ->
            ProtocolIpv6Nonxt

        "ipv6-opts" ->
            ProtocolIpv6Opts

        "ipv6-route" ->
            ProtocolIpv6Route

        "ospf" ->
            ProtocolOspf

        "pgm" ->
            ProtocolPgm

        "rsvp" ->
            ProtocolRsvp

        "sctp" ->
            ProtocolSctp

        "udplite" ->
            ProtocolUdpLite

        "vrrp" ->
            ProtocolVrrp

        _ ->
            UnsupportedProtocol protocol


encodeProtocol : Maybe SecurityGroupRuleProtocol -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeProtocol maybeProtocol object =
    case maybeProtocol of
        Just protocol ->
            let
                protocolString =
                    protocolToString protocol |> String.toLower
            in
            ( "protocol", Encode.string protocolString ) :: object

        Nothing ->
            object


directionToString : SecurityGroupRuleDirection -> String
directionToString direction =
    case direction of
        Ingress ->
            "ingress"

        Egress ->
            "egress"

        UnsupportedDirection str ->
            str


stringToSecurityGroupRuleDirection : String -> SecurityGroupRuleDirection
stringToSecurityGroupRuleDirection direction =
    case direction of
        "ingress" ->
            Ingress

        "egress" ->
            Egress

        _ ->
            UnsupportedDirection direction


encodeDirection : SecurityGroupRuleDirection -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeDirection direction object =
    ( "direction", Encode.string <| directionToString direction ) :: object


etherTypeToString : SecurityGroupRuleEthertype -> String
etherTypeToString ethertype =
    case ethertype of
        Ipv4 ->
            "IPv4"

        Ipv6 ->
            "IPv6"

        UnsupportedEthertype str ->
            str


stringToSecurityGroupRuleEthertype : String -> SecurityGroupRuleEthertype
stringToSecurityGroupRuleEthertype ethertype =
    case ethertype of
        "IPv4" ->
            Ipv4

        "IPv6" ->
            Ipv6

        _ ->
            UnsupportedEthertype ethertype


encodeEthertype : SecurityGroupRuleEthertype -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeEthertype ethertype object =
    ( "ethertype", Encode.string <| etherTypeToString ethertype ) :: object


securityGroupRuleDecoder : Decode.Decoder SecurityGroupRule
securityGroupRuleDecoder =
    Decode.succeed
        SecurityGroupRule
        |> Pipeline.optional "id" Decode.string ""
        |> Pipeline.required "ethertype" (Decode.string |> Decode.map stringToSecurityGroupRuleEthertype)
        |> Pipeline.required "direction" (Decode.string |> Decode.map stringToSecurityGroupRuleDirection)
        |> Pipeline.optional "protocol" (Decode.nullable (Decode.string |> Decode.map stringToSecurityGroupRuleProtocol)) Nothing
        |> Pipeline.optional "port_range_min" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "port_range_max" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "remote_ip_prefix" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "remote_group_id" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "description" (Decode.nullable Decode.string) Nothing
