module OpenStack.SecurityGroupRule exposing
    ( PortRangeBounds(..)
    , Remote(..)
    , RemoteType(..)
    , SecurityGroupRule
    , SecurityGroupRuleDirection(..)
    , SecurityGroupRuleEthertype(..)
    , SecurityGroupRuleProtocol(..)
    , SecurityGroupRuleTemplate
    , SecurityGroupRuleUuid
    , SecurityGroupUuid
    , allPortRangeBounds
    , decodeDirection
    , defaultRules
    , directionOptions
    , directionToString
    , encode
    , etherTypeOptions
    , etherTypeToString
    , getRemote
    , isRuleShadowed
    , matchRule
    , portRangeBoundsOptions
    , portRangeBoundsToString
    , portRangeToBounds
    , portRangeToString
    , protocolOptions
    , protocolToString
    , remoteOptions
    , remoteToRemoteType
    , remoteToString
    , remoteToStringInput
    , remoteTypeToString
    , securityGroupRuleDecoder
    , securityGroupRuleDiff
    , securityGroupRuleTemplateToRule
    , stringToPortRangeBounds
    , stringToRemoteType
    , stringToSecurityGroupRuleDirection
    , stringToSecurityGroupRuleEthertype
    , stringToSecurityGroupRuleProtocol
    )

import Helpers.String exposing (toTitleCase)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
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


type RemoteType
    = Any
    | IpPrefix
    | GroupId


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


type PortRangeBounds
    = PortRangeAny
    | PortRangeSingle
    | PortRangeMinMax


encode : SecurityGroupUuid -> SecurityGroupRule -> Encode.Value
encode securityGroupUuid { ethertype, direction, protocol, portRangeMin, portRangeMax, description } =
    Encode.object
        [ ( "security_group_rule"
          , [ ( "security_group_id", Encode.string securityGroupUuid ) ]
                |> encodeEthertype ethertype
                |> encodeDirection direction
                |> encodeProtocol protocol
                |> encodePort portRangeMin PortRangeMin
                |> encodePort portRangeMax PortRangeMax
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


portRangeToBounds :
    { a
        | portRangeMin : Maybe Int
        , portRangeMax : Maybe Int
    }
    -> PortRangeBounds
portRangeToBounds { portRangeMin, portRangeMax } =
    case ( portRangeMin, portRangeMax ) of
        ( Just min, Just max ) ->
            if min == max then
                PortRangeSingle

            else
                PortRangeMinMax

        _ ->
            PortRangeAny


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
    case protocol of
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


allDirections : List SecurityGroupRuleDirection
allDirections =
    [ Ingress, Egress ]


directionOptions : List ( String, String )
directionOptions =
    List.map (\direction -> ( directionToString direction, directionToString direction |> toTitleCase )) allDirections


allEtherTypes : List SecurityGroupRuleEthertype
allEtherTypes =
    [ Ipv4, Ipv6 ]


etherTypeOptions : List ( String, String )
etherTypeOptions =
    List.map (\etherType -> ( etherTypeToString etherType, etherTypeToString etherType |> toTitleCase )) allEtherTypes


allProtocols : List SecurityGroupRuleProtocol
allProtocols =
    [ AnyProtocol
    , ProtocolIcmp
    , ProtocolIcmpv6
    , ProtocolTcp
    , ProtocolUdp
    , ProtocolAh
    , ProtocolDccp
    , ProtocolEgp
    , ProtocolEsp
    , ProtocolGre
    , ProtocolIgmp
    , ProtocolIpv6Encap
    , ProtocolIpv6Frag
    , ProtocolIpv6Nonxt
    , ProtocolIpv6Opts
    , ProtocolIpv6Route
    , ProtocolOspf
    , ProtocolPgm
    , ProtocolRsvp
    , ProtocolSctp
    , ProtocolUdpLite
    , ProtocolVrrp
    ]


protocolOptions : List ( String, String )
protocolOptions =
    List.map (\protocol -> ( protocolToString protocol, protocolToString protocol |> toTitleCase )) allProtocols


portRangeBoundsOptions : List ( String, String )
portRangeBoundsOptions =
    List.map
        (\bounds -> ( portRangeBoundsToString bounds, portRangeBoundsToString bounds ))
        allPortRangeBounds


allPortRangeBounds : List PortRangeBounds
allPortRangeBounds =
    [ PortRangeAny, PortRangeSingle, PortRangeMinMax ]


portRangeBoundsToString : PortRangeBounds -> String
portRangeBoundsToString bounds =
    case bounds of
        PortRangeAny ->
            "Any"

        PortRangeSingle ->
            "Single"

        PortRangeMinMax ->
            "Min - Max"


stringToPortRangeBounds : String -> PortRangeBounds
stringToPortRangeBounds bounds =
    case bounds of
        "Single" ->
            PortRangeSingle

        "Min - Max" ->
            PortRangeMinMax

        _ ->
            PortRangeAny


remoteOptions : List ( String, String )
remoteOptions =
    List.map
        (\remoteType -> ( remoteTypeToString remoteType, remoteTypeToString remoteType |> toTitleCase ))
        allRemoteTypes


allRemoteTypes : List RemoteType
allRemoteTypes =
    [ Any, IpPrefix, GroupId ]


stringToRemoteType : String -> RemoteType
stringToRemoteType remoteType =
    case remoteType of
        "IP Prefix" ->
            IpPrefix

        "Group ID" ->
            GroupId

        _ ->
            Any


remoteTypeToString : RemoteType -> String
remoteTypeToString remoteType =
    case remoteType of
        IpPrefix ->
            "IP Prefix"

        GroupId ->
            "Group ID"

        Any ->
            "Any"


remoteToRemoteType : Maybe Remote -> RemoteType
remoteToRemoteType remote =
    case remote of
        Just (RemoteIpPrefix _) ->
            IpPrefix

        Just (RemoteGroupUuid _) ->
            GroupId

        _ ->
            Any
