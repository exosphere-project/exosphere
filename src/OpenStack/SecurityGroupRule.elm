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
    , protocolToString
    , securityGroupRuleDecoder
    )

import Helpers.Json exposing (resultToDecoder)
import Json.Decode as Decode
import Json.Encode as Encode
import String


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


encodeEthertype : SecurityGroupRuleEthertype -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeEthertype ethertype object =
    ( "ethertype", Encode.string <| etherTypeToString ethertype ) :: object


securityGroupRuleDecoder : Decode.Decoder SecurityGroupRule
securityGroupRuleDecoder =
    Decode.map8 SecurityGroupRule
        (Decode.field "id" Decode.string)
        (Decode.field "ethertype" Decode.string |> Decode.map parseSecurityGroupRuleEthertype |> Decode.andThen resultToDecoder)
        (Decode.field "direction" Decode.string |> Decode.map parseSecurityGroupRuleDirection |> Decode.andThen resultToDecoder)
        (Decode.field "protocol" (Decode.nullable (Decode.string |> Decode.map parseSecurityGroupRuleProtocol |> Decode.andThen resultToDecoder)))
        (Decode.field "port_range_min" (Decode.nullable Decode.int))
        (Decode.field "port_range_max" (Decode.nullable Decode.int))
        (Decode.field "remote_group_id" (Decode.nullable Decode.string))
        (Decode.field "description" (Decode.nullable Decode.string))


parseSecurityGroupRuleEthertype : String -> Result String SecurityGroupRuleEthertype
parseSecurityGroupRuleEthertype ethertype =
    case ethertype of
        "IPv4" ->
            Result.Ok Ipv4

        "IPv6" ->
            Result.Ok Ipv6

        _ ->
            Result.Err "Ooooooops, unrecognised security group rule ethertype"


parseSecurityGroupRuleDirection : String -> Result String SecurityGroupRuleDirection
parseSecurityGroupRuleDirection dir =
    case dir of
        "ingress" ->
            Result.Ok Ingress

        "egress" ->
            Result.Ok Egress

        _ ->
            Result.Err "Ooooooops, unrecognised security group rule direction"


parseSecurityGroupRuleProtocol : String -> Result String SecurityGroupRuleProtocol
parseSecurityGroupRuleProtocol prot =
    case prot of
        "any" ->
            Result.Ok AnyProtocol

        "icmp" ->
            Result.Ok ProtocolIcmp

        "icmpv6" ->
            Result.Ok ProtcolIcmpv6

        "ipv6-icmp" ->
            Result.Ok ProtcolIcmpv6

        "tcp" ->
            Result.Ok ProtocolTcp

        "udp" ->
            Result.Ok ProtocolUdp

        "ah" ->
            Result.Ok ProtocolAh

        "dccp" ->
            Result.Ok ProtocolDccp

        "egp" ->
            Result.Ok ProtocolEgp

        "esp" ->
            Result.Ok ProtocolEsp

        "gre" ->
            Result.Ok ProtocolGre

        "igmp" ->
            Result.Ok ProtocolIgmp

        "ipv6-encap" ->
            Result.Ok ProtocolIpv6Encap

        "ipv6-frag" ->
            Result.Ok ProtocolIpv6Frag

        "ipv6-nonxt" ->
            Result.Ok ProtocolIpv6Nonxt

        "ipv6-opts" ->
            Result.Ok ProtocolIpv6Opts

        "ipv6-route" ->
            Result.Ok ProtocolIpv6Route

        "ospf" ->
            Result.Ok ProtocolOspf

        "pgm" ->
            Result.Ok ProtocolPgm

        "rsvp" ->
            Result.Ok ProtocolRsvp

        "sctp" ->
            Result.Ok ProtocolSctp

        "udplite" ->
            Result.Ok ProtocolUdpLite

        "vrrp" ->
            Result.Ok ProtocolVrrp

        _ ->
            Result.Err "Ooooooops, unrecognised security group rule protocol"
