module Page.SecurityGroupRuleForm exposing (Model, Msg(..), PortInput, RemoteType, init, newBlankRule, update, view)

import Element
import Element.Input as Input
import Helpers.String exposing (toTitleCase)
import List
import OpenStack.SecurityGroupRule
    exposing
        ( Remote(..)
        , SecurityGroupRule
        , SecurityGroupRuleDirection(..)
        , SecurityGroupRuleEthertype(..)
        , SecurityGroupRuleProtocol(..)
        , directionToString
        , etherTypeToString
        , getRemote
        , protocolToString
        , stringToSecurityGroupRuleDirection
        , stringToSecurityGroupRuleEthertype
        , stringToSecurityGroupRuleProtocol
        )
import Style.Widgets.NumericTextInput.NumericTextInput as NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Select
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import View.Helpers as VH
import View.Types


type PortInput
    = StartingPort NumericTextInput
    | EndingPort NumericTextInput


type RemoteType
    = Any
    | IpPrefix
    | GroupId


type Msg
    = GotRuleUpdate SecurityGroupRule
    | GotPortInput PortInput
    | GotRemoteTypeUpdate RemoteType
    | GotRemote (Maybe Remote)
    | NoOp


type alias Model =
    { rule : SecurityGroupRule
    , startingPortInput : NumericTextInput
    , endingPortInput : NumericTextInput
    , remoteType : RemoteType
    }


init : SecurityGroupRule -> Model
init rule =
    { rule = rule
    , startingPortInput =
        case rule.portRangeMin of
            Just portRangeMin ->
                ValidNumericTextInput portRangeMin

            Nothing ->
                BlankNumericTextInput
    , endingPortInput =
        case rule.portRangeMax of
            Just portRangeMax ->
                ValidNumericTextInput portRangeMax

            Nothing ->
                BlankNumericTextInput
    , remoteType = remoteToRemoteType <| getRemote rule
    }


newBlankRule : Int -> SecurityGroupRule
newBlankRule index =
    { uuid =
        -- OpenStack generates a `uuid` for new rules. This dummy value is used to index the rule in the model.
        String.fromInt index
    , direction = Ingress
    , ethertype = Ipv4
    , protocol = Nothing
    , portRangeMin = Nothing
    , portRangeMax = Nothing
    , remoteIpPrefix = Nothing
    , remoteGroupUuid = Nothing
    , description = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRuleUpdate rule ->
            ( { model
                | rule =
                    rule
              }
            , Cmd.none
            )

        GotPortInput portInput ->
            ( { model
                | startingPortInput =
                    case portInput of
                        StartingPort input ->
                            input

                        _ ->
                            model.startingPortInput
                , endingPortInput =
                    case portInput of
                        EndingPort input ->
                            input

                        _ ->
                            model.endingPortInput
                , rule =
                    let
                        rule =
                            model.rule
                    in
                    case portInput of
                        StartingPort i ->
                            { rule
                                | portRangeMin = NumericTextInput.toMaybe i
                            }

                        EndingPort i ->
                            { rule
                                | portRangeMax = NumericTextInput.toMaybe i
                            }
              }
            , Cmd.none
            )

        GotRemoteTypeUpdate remoteType ->
            ( { model
                | remoteType = remoteType
              }
            , Cmd.none
            )

        GotRemote remote ->
            ( { model
                | rule =
                    let
                        rule =
                            model.rule
                    in
                    case remote of
                        Just remote_ ->
                            { rule
                                | remoteIpPrefix =
                                    case remote_ of
                                        RemoteIpPrefix ipPrefix ->
                                            Just ipPrefix

                                        _ ->
                                            Nothing
                                , remoteGroupUuid =
                                    case remote_ of
                                        RemoteGroupUuid groupUuid ->
                                            Just groupUuid

                                        _ ->
                                            Nothing
                            }

                        Nothing ->
                            { rule
                                | remoteIpPrefix = Nothing
                                , remoteGroupUuid = Nothing
                            }
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


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


remoteToInput : Maybe Remote -> String
remoteToInput remote =
    case remote of
        Just (RemoteIpPrefix ip) ->
            ip

        Just (RemoteGroupUuid groupUuid) ->
            groupUuid

        Nothing ->
            ""


form :
    View.Types.Context
    -> Model
    -> Element.Element Msg
form context model =
    let
        rule =
            model.rule

        consistentHeight =
            Element.height <| Element.px spacer.px48
    in
    Element.column [ Element.spacing spacer.px12 ]
        [ Element.wrappedRow [ Element.spacing spacer.px12 ]
            [ Element.column [ Element.spacing spacer.px12 ]
                [ Text.body "Direction"
                , Style.Widgets.Select.select
                    []
                    context.palette
                    { label = "Choose a direction"
                    , onChange =
                        \direction ->
                            case direction of
                                Just dir ->
                                    GotRuleUpdate { rule | direction = stringToSecurityGroupRuleDirection dir }

                                Nothing ->
                                    NoOp
                    , options = directionOptions
                    , selected = Just (directionToString rule.direction)
                    }
                ]
            , Element.column [ Element.spacing spacer.px12 ]
                [ Text.body "Ether Type"
                , Style.Widgets.Select.select
                    []
                    context.palette
                    { label = "Choose an ether type"
                    , onChange =
                        \etherType ->
                            case etherType of
                                Just et ->
                                    GotRuleUpdate { rule | ethertype = stringToSecurityGroupRuleEthertype et }

                                Nothing ->
                                    NoOp
                    , options = etherTypeOptions
                    , selected = Just (etherTypeToString rule.ethertype)
                    }
                ]
            , Element.column [ Element.spacing spacer.px12 ]
                [ Text.body "Protocol"
                , Style.Widgets.Select.select
                    []
                    context.palette
                    { label = "Choose a protocol"
                    , onChange =
                        \protocol ->
                            case protocol of
                                Just prot ->
                                    GotRuleUpdate { rule | protocol = Just <| stringToSecurityGroupRuleProtocol prot }

                                Nothing ->
                                    NoOp
                    , options = protocolOptions
                    , selected = Just (protocolToString <| Maybe.withDefault AnyProtocol rule.protocol)
                    }
                ]
            , Element.row [ Element.spacing spacer.px12 ]
                [ numericTextInput
                    context.palette
                    (VH.inputItemAttributes context.palette ++ [ consistentHeight ])
                    model.startingPortInput
                    { labelText = "Starting port"
                    , minVal = Just 1
                    , maxVal = Just 65535
                    , defaultVal = Nothing
                    , required = False
                    }
                    (\input -> GotPortInput <| StartingPort input)
                , numericTextInput
                    context.palette
                    (VH.inputItemAttributes context.palette ++ [ consistentHeight ])
                    model.endingPortInput
                    { labelText = "Ending port"
                    , minVal = Just 1
                    , maxVal = Just 65535
                    , defaultVal = Nothing
                    , required = False
                    }
                    (\input -> GotPortInput <| EndingPort input)
                ]
            , Element.row [ Element.spacingXY spacer.px12 0, Element.width <| Element.fill ]
                [ Element.column [ Element.spacing spacer.px12 ]
                    [ Text.body "Remote"
                    , Style.Widgets.Select.select
                        []
                        context.palette
                        { label = "Choose a remote type"
                        , onChange =
                            \remoteType ->
                                case remoteType of
                                    Just remoteType_ ->
                                        GotRemoteTypeUpdate <| stringToRemoteType remoteType_

                                    Nothing ->
                                        NoOp
                        , options = remoteOptions
                        , selected = Just (remoteTypeToString <| remoteToRemoteType <| getRemote rule)
                        }
                    ]
                , case model.remoteType of
                    Any ->
                        Element.none

                    remoteType ->
                        -- TODO: renderInvalidReason invalidReason
                        Input.text
                            (VH.inputItemAttributes context.palette ++ [ consistentHeight, Element.width Element.fill ])
                            { text = remoteToInput <| getRemote rule
                            , placeholder = Nothing
                            , onChange =
                                \text ->
                                    case remoteType of
                                        IpPrefix ->
                                            GotRemote <| Just <| RemoteIpPrefix text

                                        GroupId ->
                                            GotRemote <| Just <| RemoteGroupUuid text

                                        Any ->
                                            GotRemote Nothing
                            , label = Input.labelAbove [] (VH.requiredLabel context.palette (Element.text <| remoteTypeToString <| remoteType))
                            }
                ]
            ]
        , Input.text
            (VH.inputItemAttributes context.palette ++ [ consistentHeight, Element.width Element.fill ])
            { text = Maybe.withDefault "" rule.description
            , placeholder = Just <| Input.placeholder [] (Element.text "Optional")
            , onChange = \text -> GotRuleUpdate { rule | description = Just text }
            , label = Input.labelAbove [] (Element.text "Description")
            }
        ]


view : View.Types.Context -> Model -> Element.Element Msg
view context model =
    Element.column [ Element.padding spacer.px8 ]
        [ form
            context
            model
        ]
