module OpenStack.SecurityGroupRule exposing
    ( SecurityGroupRule
    , SecurityGroupRuleDirection(..)
    , SecurityGroupRuleEthertype(..)
    , SecurityGroupRuleProtocol(..)
    , buildRuleExposeAllIncomingPorts
    , buildRuleIcmp
    , buildRuleTCP
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
    , protocol = Just Tcp
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
    , protocol = Just Icmp
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
    , protocol = Just Tcp
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
    | Icmp
    | Icmpv6
    | Tcp
    | Udp


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

                        Icmp ->
                            "icmp"

                        Icmpv6 ->
                            "icmpv6"

                        Tcp ->
                            "tcp"

                        Udp ->
                            "udp"
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
            Decode.succeed Icmp

        "icmpv6" ->
            Decode.succeed Icmpv6

        "tcp" ->
            Decode.succeed Tcp

        "udp" ->
            Decode.succeed Udp

        _ ->
            Decode.fail "Ooooooops, unrecognised security group rule protocol"
