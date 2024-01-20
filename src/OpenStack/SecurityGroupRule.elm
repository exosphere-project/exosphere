module OpenStack.SecurityGroupRule exposing
    ( SecurityGroupRule
    , SecurityGroupRuleDirection(..)
    , SecurityGroupRuleEthertype(..)
    , SecurityGroupRuleProtocol(..)
    , SecurityGroupRuleUuid
    , SecurityGroupUuid
    , defaultExosphereRules
    , encode
    , matchRule
    , securityGroupRuleDecoder
    )

import Json.Decode as Decode
import Json.Encode as Encode


type alias SecurityGroupRule =
    { uuid : SecurityGroupRuleUuid
    , ethertype : SecurityGroupRuleEthertype
    , direction : SecurityGroupRuleDirection
    , protocol : Maybe SecurityGroupRuleProtocol
    , port_range_min : Maybe Int
    , port_range_max : Maybe Int
    , remoteGroupUuid : Maybe SecurityGroupRuleUuid -- TODO: not encoded
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
    , remoteGroupUuid = Nothing
    , description = Just "Ping"
    }


buildRuleExposeAllIncomingPorts : SecurityGroupRule
buildRuleExposeAllIncomingPorts =
    { uuid = ""
    , ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , port_range_min = Nothing
    , port_range_max = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Expose all incoming ports"
    }


defaultExosphereRules : List SecurityGroupRule
defaultExosphereRules =
    [ buildRuleTCP 22 "SSH"
    , buildRuleIcmp
    , buildRuleExposeAllIncomingPorts
    ]


type alias SecurityGroupRuleUuid =
    String


type alias SecurityGroupUuid =
    String


type SecurityGroupRuleDirection
    = Ingress
    | Egress


type SecurityGroupRuleEthertype
    = Ipv4
    | Ipv6


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


encodeProtocol : Maybe SecurityGroupRuleProtocol -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeProtocol maybeProtocol object =
    case maybeProtocol of
        Just protocol ->
            let
                protocolString =
                    case protocol of
                        AnyProtocol ->
                            "any"

                        ProtocolIcmp ->
                            "icmp"

                        ProtcolIcmpv6 ->
                            "icmpv6"

                        ProtocolTcp ->
                            "tcp"

                        ProtocolUdp ->
                            "udp"

                        ProtocolAh ->
                            "ah"

                        ProtocolDccp ->
                            "dccp"

                        ProtocolEgp ->
                            "egp"

                        ProtocolEsp ->
                            "esp"

                        ProtocolGre ->
                            "gre"

                        ProtocolIgmp ->
                            "igmp"

                        ProtocolIpv6Encap ->
                            "ipv6-encap"

                        ProtocolIpv6Frag ->
                            "ipv6-frag"

                        ProtocolIpv6Nonxt ->
                            "ipv6-nonxt"

                        ProtocolIpv6Opts ->
                            "ipv6-opts"

                        ProtocolIpv6Route ->
                            "ipv6-route"

                        ProtocolOspf ->
                            "ospf"

                        ProtocolPgm ->
                            "pgm"

                        ProtocolRsvp ->
                            "rsvp"

                        ProtocolSctp ->
                            "sctp"

                        ProtocolUdpLite ->
                            "udplite"

                        ProtocolVrrp ->
                            "vrrp"
            in
            ( "protocol", Encode.string protocolString ) :: object

        Nothing ->
            object


encodeDirection : SecurityGroupRuleDirection -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeDirection direction object =
    let
        directionString =
            case direction of
                Ingress ->
                    "ingress"

                Egress ->
                    "egress"
    in
    ( "direction", Encode.string directionString ) :: object


encodeEthertype : SecurityGroupRuleEthertype -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeEthertype ethertype object =
    let
        ethertypeString =
            case ethertype of
                Ipv4 ->
                    "IPv4"

                Ipv6 ->
                    "IPv6"
    in
    ( "ethertype", Encode.string ethertypeString ) :: object


securityGroupRuleDecoder : Decode.Decoder SecurityGroupRule
securityGroupRuleDecoder =
    Decode.map8 SecurityGroupRule
        (Decode.field "id" Decode.string)
        (Decode.field "ethertype" Decode.string |> Decode.andThen securityGroupRuleEthertypeDecoder)
        (Decode.field "direction" Decode.string |> Decode.andThen securityGroupRuleDirectionDecoder)
        (Decode.field "protocol" (Decode.nullable (Decode.string |> Decode.andThen securityGroupRuleProtocolDecoder)))
        (Decode.field "port_range_min" (Decode.nullable Decode.int))
        (Decode.field "port_range_max" (Decode.nullable Decode.int))
        (Decode.field "remote_group_id" (Decode.nullable Decode.string))
        (Decode.field "description" (Decode.nullable Decode.string))


securityGroupRuleEthertypeDecoder : String -> Decode.Decoder SecurityGroupRuleEthertype
securityGroupRuleEthertypeDecoder ethertype =
    case ethertype of
        "IPv4" ->
            Decode.succeed Ipv4

        "IPv6" ->
            Decode.succeed Ipv6

        _ ->
            Decode.fail "Ooooooops, unrecognised security group rule ethertype"


securityGroupRuleDirectionDecoder : String -> Decode.Decoder SecurityGroupRuleDirection
securityGroupRuleDirectionDecoder dir =
    case dir of
        "ingress" ->
            Decode.succeed Ingress

        "egress" ->
            Decode.succeed Egress

        _ ->
            Decode.fail "Ooooooops, unrecognised security group rule direction"


securityGroupRuleProtocolDecoder : String -> Decode.Decoder SecurityGroupRuleProtocol
securityGroupRuleProtocolDecoder prot =
    case prot of
        "any" ->
            Decode.succeed AnyProtocol

        "icmp" ->
            Decode.succeed ProtocolIcmp

        "icmpv6" ->
            Decode.succeed ProtcolIcmpv6

        "ipv6-icmp" ->
            Decode.succeed ProtcolIcmpv6

        "tcp" ->
            Decode.succeed ProtocolTcp

        "udp" ->
            Decode.succeed ProtocolUdp

        "ah" ->
            Decode.succeed ProtocolAh

        "dccp" ->
            Decode.succeed ProtocolDccp

        "egp" ->
            Decode.succeed ProtocolEgp

        "esp" ->
            Decode.succeed ProtocolEsp

        "gre" ->
            Decode.succeed ProtocolGre

        "igmp" ->
            Decode.succeed ProtocolIgmp

        "ipv6-encap" ->
            Decode.succeed ProtocolIpv6Encap

        "ipv6-frag" ->
            Decode.succeed ProtocolIpv6Frag

        "ipv6-nonxt" ->
            Decode.succeed ProtocolIpv6Nonxt

        "ipv6-opts" ->
            Decode.succeed ProtocolIpv6Opts

        "ipv6-route" ->
            Decode.succeed ProtocolIpv6Route

        "ospf" ->
            Decode.succeed ProtocolOspf

        "pgm" ->
            Decode.succeed ProtocolPgm

        "rsvp" ->
            Decode.succeed ProtocolRsvp

        "sctp" ->
            Decode.succeed ProtocolSctp

        "udplite" ->
            Decode.succeed ProtocolUdpLite

        "vrrp" ->
            Decode.succeed ProtocolVrrp

        _ ->
            Decode.fail "Ooooooops, unrecognised security group rule protocol"
