module OpenStack.SecurityGroupRule exposing
    ( Remote(..)
    , SecurityGroupRule
    , SecurityGroupRuleDirection(..)
    , SecurityGroupRuleEthertype(..)
    , SecurityGroupRuleProtocol(..)
    , SecurityGroupRuleTemplate
    , SecurityGroupRuleUuid
    , SecurityGroupUuid
    , compareSecurityGroupRuleLists
    , decodeDirection
    , defaultRules
    , directionToString
    , encode
    , etherTypeToString
    , getRemote
    , isRuleShadowed
    , matchRule
    , portRangeToString
    , protocolToString
    , remoteToString
    , remoteToStringInput
    , securityGroupRuleDecoder
    , securityGroupRuleDiff
    , securityGroupRuleTemplateToRule
    , securityGroupRuleToTemplate
    , stringToSecurityGroupRuleDirection
    , stringToSecurityGroupRuleEthertype
    , stringToSecurityGroupRuleProtocol
    )

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import OpenStack.HelperTypes as HelperTypes
import String


type alias SecurityGroupRule =
    { uuid : SecurityGroupRuleUuid
    , ethertype : SecurityGroupRuleEthertype
    , direction : SecurityGroupRuleDirection
    , protocol : Maybe SecurityGroupRuleProtocol
    , portRangeMin : Maybe Int
    , portRangeMax : Maybe Int
    , remoteIpPrefix : Maybe String -- TODO: Encode remote IP prefix in requests.
    , remoteGroupUuid : Maybe SecurityGroupRuleUuid -- TODO: Encode remote security group in requests.
    , description : Maybe String
    }


type alias SecurityGroupRuleTemplate =
    { ethertype : SecurityGroupRuleEthertype
    , direction : SecurityGroupRuleDirection
    , protocol : Maybe SecurityGroupRuleProtocol
    , portRangeMin : Maybe Int
    , portRangeMax : Maybe Int
    , remoteIpPrefix : Maybe String
    , remoteGroupUuid : Maybe SecurityGroupRuleUuid
    , description : Maybe String
    }


securityGroupRuleTemplateToRule : SecurityGroupRuleTemplate -> SecurityGroupRule
securityGroupRuleTemplateToRule { ethertype, direction, protocol, portRangeMin, portRangeMax, remoteIpPrefix, remoteGroupUuid, description } =
    { uuid = ""
    , ethertype = ethertype
    , direction = direction
    , protocol = protocol
    , portRangeMin = portRangeMin
    , portRangeMax = portRangeMax
    , remoteIpPrefix = remoteIpPrefix
    , remoteGroupUuid = remoteGroupUuid
    , description = description
    }


securityGroupRuleToTemplate : SecurityGroupRule -> SecurityGroupRuleTemplate
securityGroupRuleToTemplate { ethertype, direction, protocol, portRangeMin, portRangeMax, remoteIpPrefix, remoteGroupUuid, description } =
    { ethertype = ethertype
    , direction = direction
    , protocol = protocol
    , portRangeMin = portRangeMin
    , portRangeMax = portRangeMax
    , remoteIpPrefix = remoteIpPrefix
    , remoteGroupUuid = remoteGroupUuid
    , description = description
    }


{-| Compare security group rules. If they have the same impact, they are equal.
-}
matchRule : SecurityGroupRule -> SecurityGroupRule -> Bool
matchRule ruleA ruleB =
    (ruleA.ethertype == ruleB.ethertype)
        && (ruleA.direction == ruleB.direction)
        && (ruleA.protocol == ruleB.protocol)
        && (ruleA.portRangeMin == ruleB.portRangeMin)
        && (ruleA.portRangeMax == ruleB.portRangeMax)
        && (case ( getRemote ruleA, getRemote ruleB ) of
                ( Just remoteA, Just remoteB ) ->
                    remoteMatch remoteA remoteB

                ( Nothing, Nothing ) ->
                    True

                _ ->
                    False
           )


{-| Compare security group rules based on rule impact & description.
-}
matchRuleAndDescription : SecurityGroupRule -> SecurityGroupRule -> Bool
matchRuleAndDescription ruleA ruleB =
    matchRule ruleA ruleB
        && (ruleA.description == ruleB.description)


isRuleShadowed : SecurityGroupRule -> List SecurityGroupRule -> Bool
isRuleShadowed rule rules =
    List.any (\r -> isSubsumedBy rule r) rules


isSubsumedBy : SecurityGroupRule -> SecurityGroupRule -> Bool
isSubsumedBy ruleA ruleB =
    ruleA.uuid
        /= ruleB.uuid
        && (not <| matchRule ruleA ruleB)
        && (ruleA.direction == ruleB.direction)
        && (ruleA.ethertype == ruleB.ethertype)
        && protocolSubsumedBy ruleA.protocol ruleB.protocol
        && portRangeSubsumedBy ( ruleA.portRangeMin, ruleA.portRangeMax ) ( ruleB.portRangeMin, ruleB.portRangeMax )
        && remoteSubsumedBy ruleA ruleB


protocolSubsumedBy : Maybe SecurityGroupRuleProtocol -> Maybe SecurityGroupRuleProtocol -> Bool
protocolSubsumedBy protocolA protocolB =
    case ( protocolA, protocolB ) of
        ( Nothing, Nothing ) ->
            True

        ( Just _, Nothing ) ->
            -- RuleB applies to any protocol.
            True

        ( Just pA, Just pB ) ->
            pA == pB

        ( Nothing, Just _ ) ->
            -- RuleA applies to any protocol, but RuleB is more specific.
            False


portRangeSubsumedBy : ( Maybe Int, Maybe Int ) -> ( Maybe Int, Maybe Int ) -> Bool
portRangeSubsumedBy ( portMinA, portMaxA ) ( portMinB, portMaxB ) =
    let
        minA =
            Maybe.withDefault 0 portMinA

        maxA =
            Maybe.withDefault 65535 portMaxA

        minB =
            Maybe.withDefault 0 portMinB

        maxB =
            Maybe.withDefault 65535 portMaxB
    in
    (minB <= minA) && (maxA <= maxB)


{-| A remote is either an IP prefix or a security group uuid.
-}
type Remote
    = RemoteIpPrefix String
    | RemoteGroupUuid String


getRemote : SecurityGroupRule -> Maybe Remote
getRemote rule =
    case ( rule.remoteIpPrefix, rule.remoteGroupUuid ) of
        ( Just ipPrefix, Nothing ) ->
            Just (RemoteIpPrefix ipPrefix)

        ( Nothing, Just groupUuid ) ->
            Just (RemoteGroupUuid groupUuid)

        ( Nothing, Nothing ) ->
            Nothing

        ( Just _, Just _ ) ->
            -- Should not happen since IP & remote group are mutually exclusive.
            Nothing


remoteToString : Maybe Remote -> String
remoteToString remote =
    case remote of
        Just (RemoteIpPrefix ip) ->
            ip

        Just (RemoteGroupUuid groupUuid) ->
            groupUuid

        Nothing ->
            "Any"


remoteToStringInput : Maybe Remote -> String
remoteToStringInput remote =
    remote
        |> remoteToString
        |> (\remoteString ->
                if remoteString == "Any" then
                    ""

                else
                    remoteString
           )


remoteMatch : Remote -> Remote -> Bool
remoteMatch remoteA remoteB =
    case ( remoteA, remoteB ) of
        ( RemoteIpPrefix ipA, RemoteIpPrefix ipB ) ->
            ipA == ipB

        ( RemoteGroupUuid groupA, RemoteGroupUuid groupB ) ->
            groupA == groupB

        ( _, _ ) ->
            False


remoteSubsumedBy : SecurityGroupRule -> SecurityGroupRule -> Bool
remoteSubsumedBy ruleA ruleB =
    let
        remoteA =
            getRemote ruleA

        remoteB =
            getRemote ruleB
    in
    case ( remoteA, remoteB ) of
        ( Nothing, Nothing ) ->
            True

        ( Just _, Nothing ) ->
            -- RuleB applies to any remote, subsumes RuleA.
            True

        ( Just ra, Just rb ) ->
            -- TODO: Parse CIDR notation and compare ranges instead of using strict match for IP prefix.
            remoteMatch ra rb

        ( Nothing, Just _ ) ->
            -- RuleA applies to any remote, RuleB is more specific.
            False


buildRuleTCP : Int -> String -> SecurityGroupRuleTemplate
buildRuleTCP portNumber description =
    { ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , portRangeMin = Just portNumber
    , portRangeMax = Just portNumber
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just description
    }


buildRuleIcmp : SecurityGroupRuleTemplate
buildRuleIcmp =
    { ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolIcmp
    , portRangeMin = Nothing
    , portRangeMax = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Ping"
    }


buildRuleIcmpIPv6 : SecurityGroupRuleTemplate
buildRuleIcmpIPv6 =
    { ethertype = Ipv6
    , direction = Ingress
    , protocol = Just ProtocolIcmpv6
    , portRangeMin = Nothing
    , portRangeMax = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Ping IPv6"
    }


buildRuleMosh : SecurityGroupRuleTemplate
buildRuleMosh =
    { ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolUdp
    , portRangeMin = Just 60000
    , portRangeMax = Just 61000
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Mosh"
    }


buildRuleExposeAllIncomingPorts : SecurityGroupRuleTemplate
buildRuleExposeAllIncomingPorts =
    { ethertype = Ipv4
    , direction = Ingress
    , protocol = Just ProtocolTcp
    , portRangeMin = Nothing
    , portRangeMax = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Expose all incoming ports"
    }


buildRuleAllowAllOutgoingIPv4 : SecurityGroupRuleTemplate
buildRuleAllowAllOutgoingIPv4 =
    { ethertype = Ipv4
    , direction = Egress
    , protocol = Nothing
    , portRangeMin = Nothing
    , portRangeMax = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Allow all outgoing IPv4 traffic"
    }


buildRuleAllowAllOutgoingIPv6 : SecurityGroupRuleTemplate
buildRuleAllowAllOutgoingIPv6 =
    { ethertype = Ipv6
    , direction = Egress
    , protocol = Nothing
    , portRangeMin = Nothing
    , portRangeMax = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Just "Allow all outgoing IPv6 traffic"
    }


defaultRules : List SecurityGroupRuleTemplate
defaultRules =
    [ buildRuleTCP 22 "SSH"
    , buildRuleIcmp
    , buildRuleIcmpIPv6
    , buildRuleMosh
    , buildRuleExposeAllIncomingPorts
    , buildRuleAllowAllOutgoingIPv4
    , buildRuleAllowAllOutgoingIPv6
    ]


{-| Returns rules that are in the first list but not in the second list. (Difference read as A minus B.)

Note: This includes differences in rule description.

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
                                    matchRuleAndDescription existingRule defaultRule
                                )
                in
                if ruleExists then
                    Nothing

                else
                    Just defaultRule
            )


{-| Given two lists of security group rules, determine which are missing or extra when comparing the first list to the second.

Note: Rules that have the same impact but different descriptions are considered different.

-}
compareSecurityGroupRuleLists : List SecurityGroupRule -> List SecurityGroupRule -> { extra : List SecurityGroupRule, missing : List SecurityGroupRule }
compareSecurityGroupRuleLists existingRules updatedRules =
    let
        missingRules =
            securityGroupRuleDiff updatedRules existingRules

        extraRules =
            securityGroupRuleDiff existingRules updatedRules
    in
    { extra = extraRules, missing = missingRules }


type alias SecurityGroupRuleUuid =
    String


type alias SecurityGroupUuid =
    HelperTypes.Uuid


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
    | ProtocolIcmpv6
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
encode securityGroupUuid { ethertype, direction, protocol, portRangeMin, portRangeMax, remoteIpPrefix, remoteGroupUuid, description } =
    Encode.object
        [ ( "security_group_rule"
          , [ ( "security_group_id", Encode.string securityGroupUuid ) ]
                |> encodeEthertype ethertype
                |> encodeDirection direction
                |> encodeProtocol protocol
                |> encodePort portRangeMin PortRangeMin
                |> encodePort portRangeMax PortRangeMax
                |> encodeRemoteIpPrefix remoteIpPrefix
                |> encodeRemoteGroupId remoteGroupUuid
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


encodeRemoteGroupId : Maybe SecurityGroupRuleUuid -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeRemoteGroupId maybeRemoteGroupId object =
    case maybeRemoteGroupId of
        Just remoteGroupId ->
            ( "remote_group_id", Encode.string remoteGroupId ) :: object

        Nothing ->
            object


encodeRemoteIpPrefix : Maybe String -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeRemoteIpPrefix maybeRemoteIpPrefix object =
    case maybeRemoteIpPrefix of
        Just remoteIpPrefix ->
            ( "remote_ip_prefix", Encode.string remoteIpPrefix ) :: object

        Nothing ->
            object


portRangeToString :
    { a
        | portRangeMin : Maybe Int
        , portRangeMax : Maybe Int
    }
    -> String
portRangeToString { portRangeMin, portRangeMax } =
    case ( portRangeMin, portRangeMax ) of
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

        ProtocolIcmpv6 ->
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
    -- forgive mixed case strings like "ICMPv6"
    case String.toLower protocol of
        "any" ->
            AnyProtocol

        "icmp" ->
            ProtocolIcmp

        "icmpv6" ->
            ProtocolIcmpv6

        "ipv6-icmp" ->
            ProtocolIcmpv6

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
            "incoming"

        Egress ->
            "outgoing"

        UnsupportedDirection str ->
            str


stringToSecurityGroupRuleDirection : String -> SecurityGroupRuleDirection
stringToSecurityGroupRuleDirection direction =
    case direction of
        "incoming" ->
            Ingress

        "outgoing" ->
            Egress

        _ ->
            UnsupportedDirection direction


decodeDirection : String -> SecurityGroupRuleDirection
decodeDirection direction =
    case direction of
        "ingress" ->
            Ingress

        "egress" ->
            Egress

        _ ->
            UnsupportedDirection direction


encodeDirection : SecurityGroupRuleDirection -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
encodeDirection direction object =
    ( "direction"
    , Encode.string <|
        case direction of
            Ingress ->
                "ingress"

            Egress ->
                "egress"

            UnsupportedDirection str ->
                str
    )
        :: object


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
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "ethertype" (Decode.string |> Decode.map stringToSecurityGroupRuleEthertype)
        |> Pipeline.required "direction" (Decode.string |> Decode.map decodeDirection)
        |> Pipeline.optional "protocol" (Decode.nullable (Decode.string |> Decode.map stringToSecurityGroupRuleProtocol)) Nothing
        |> Pipeline.optional "port_range_min" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "port_range_max" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "remote_ip_prefix" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "remote_group_id" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "description" (Decode.nullable Decode.string) Nothing
