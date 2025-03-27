module Page.SecurityGroupRuleForm exposing (Model, Msg(..), PortInput, init, newBlankRule, update, view)

import Element
import Element.Input as Input
import Helpers.Cidr exposing (isValidCidr)
import Helpers.GetterSetters exposing (sortedSecurityGroups)
import Helpers.RemoteDataPlusPlus as RDPP
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
        , remoteToStringInput
        , stringToSecurityGroupRuleDirection
        , stringToSecurityGroupRuleEthertype
        , stringToSecurityGroupRuleProtocol
        )
import Style.Widgets.NumericTextInput.NumericTextInput as NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Style.Widgets.Select
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation exposing (FormInteraction(..))
import Types.Project exposing (Project)
import View.Helpers as VH exposing (directionOptions, etherTypeOptions, portRangeBoundsOptions, portRangeBoundsToString, protocolOptions, remoteOptions, remoteToRemoteType, remoteTypeToString, stringToPortRangeBounds, stringToRemoteType)
import View.Types exposing (PortRangeBounds(..), RemoteType(..))


type PortInput
    = StartingPort NumericTextInput
    | EndingPort NumericTextInput


type Msg
    = GotRuleUpdate SecurityGroupRule
    | GotPortRangeBounds PortRangeBounds
    | GotPortInput PortInput
    | GotRemoteTypeUpdate RemoteType
    | GotRemote (Maybe Remote)
    | NoOp


type alias Model =
    { rule : SecurityGroupRule
    , portRangeBounds : PortRangeBounds
    , startingPortInput : NumericTextInput
    , endingPortInput : NumericTextInput
    , remoteType : RemoteType
    , remoteTypeInputInteraction : FormInteraction
    }


init : SecurityGroupRule -> Model
init rule =
    { rule = rule
    , portRangeBounds =
        case ( rule.portRangeMin, rule.portRangeMax ) of
            ( Just _, Just _ ) ->
                if rule.portRangeMin == rule.portRangeMax then
                    PortRangeSingle

                else
                    PortRangeMinMax

            ( Nothing, Nothing ) ->
                PortRangeAny

            _ ->
                -- These are incomplete or unbounded ranges.
                PortRangeMinMax
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
    , remoteTypeInputInteraction = Pristine
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

        GotPortRangeBounds portRangeBounds ->
            ( { model
                | portRangeBounds =
                    portRangeBounds
                , startingPortInput =
                    if portRangeBounds == PortRangeAny then
                        BlankNumericTextInput

                    else
                        model.startingPortInput
                , endingPortInput =
                    if portRangeBounds == PortRangeMinMax then
                        model.endingPortInput

                    else
                        BlankNumericTextInput
                , rule =
                    let
                        rule =
                            model.rule
                    in
                    case portRangeBounds of
                        PortRangeAny ->
                            { rule
                                | portRangeMin = Nothing
                                , portRangeMax = Nothing
                            }

                        PortRangeSingle ->
                            { rule
                                | portRangeMin = NumericTextInput.toMaybe model.startingPortInput
                                , portRangeMax = NumericTextInput.toMaybe model.startingPortInput
                            }

                        PortRangeMinMax ->
                            { rule
                                | portRangeMin = NumericTextInput.toMaybe model.startingPortInput
                                , portRangeMax = NumericTextInput.toMaybe model.endingPortInput
                            }
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
                                , portRangeMax =
                                    case model.portRangeBounds of
                                        PortRangeSingle ->
                                            -- For single ports, start = end.
                                            NumericTextInput.toMaybe i

                                        _ ->
                                            rule.portRangeMax
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
                , remoteTypeInputInteraction = Pristine
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
                , remoteTypeInputInteraction = Touched
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


form :
    View.Types.Context
    -> Project
    -> Model
    -> Element.Element Msg
form context project model =
    let
        rule =
            model.rule

        consistentHeight =
            Element.height <| Element.px spacer.px48

        renderInvalidReason reason =
            case reason of
                Just r ->
                    r |> Validation.invalidMessage context.palette

                Nothing ->
                    Element.none
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
                [ Element.column [ Element.spacing spacer.px12 ]
                    [ Text.body "Port Range"
                    , Style.Widgets.Select.select
                        []
                        context.palette
                        { label = "Choose a port range"
                        , onChange =
                            \portRangeBounds ->
                                case portRangeBounds of
                                    Just portRangeBounds_ ->
                                        GotPortRangeBounds <| stringToPortRangeBounds portRangeBounds_

                                    Nothing ->
                                        NoOp
                        , options = portRangeBoundsOptions
                        , selected = Just <| portRangeBoundsToString model.portRangeBounds
                        }
                    ]
                , Element.column [ Element.spacing spacer.px12 ]
                    [ Element.row [ Element.spacing spacer.px12 ]
                        (let
                            startingPortInput bounds =
                                numericTextInput
                                    context.palette
                                    (VH.inputItemAttributes context.palette ++ [ consistentHeight, Element.width (Element.shrink |> Element.minimum 100) ])
                                    model.startingPortInput
                                    { labelText =
                                        if bounds == PortRangeSingle then
                                            "Port"

                                        else
                                            "Starting port"
                                    , minVal = Just 1
                                    , maxVal = Just 65535
                                    , defaultVal = Nothing
                                    , required = True
                                    }
                                    (\input -> GotPortInput <| StartingPort input)
                         in
                         case model.portRangeBounds of
                            PortRangeAny ->
                                []

                            PortRangeSingle ->
                                [ startingPortInput model.portRangeBounds ]

                            PortRangeMinMax ->
                                [ startingPortInput model.portRangeBounds
                                , -- endingPortInput
                                  numericTextInput
                                    context.palette
                                    (VH.inputItemAttributes context.palette ++ [ consistentHeight ])
                                    model.endingPortInput
                                    { labelText = "Ending port"
                                    , minVal = Just 1
                                    , maxVal = Just 65535
                                    , defaultVal = Nothing
                                    , required = True
                                    }
                                    (\input -> GotPortInput <| EndingPort input)
                                ]
                        )
                    , let
                        invalidReason =
                            if model.portRangeBounds == PortRangeMinMax then
                                let
                                    startingPortValue =
                                        Maybe.withDefault 0 <| NumericTextInput.toMaybe model.startingPortInput

                                    endingPortValue =
                                        Maybe.withDefault 65535 <| NumericTextInput.toMaybe model.endingPortInput
                                in
                                if startingPortValue > endingPortValue then
                                    Just "Starting port cannot be greater than ending port."

                                else
                                    Nothing

                            else
                                Nothing
                      in
                      renderInvalidReason invalidReason
                    ]
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

                    IpPrefix ->
                        Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
                            [ Input.text
                                (VH.inputItemAttributes context.palette ++ [ consistentHeight ])
                                { text = remoteToStringInput <| getRemote rule
                                , placeholder = Nothing
                                , onChange =
                                    \text ->
                                        GotRemote <| Just <| RemoteIpPrefix text
                                , label = Input.labelAbove [] (VH.requiredLabel context.palette (Element.text <| remoteTypeToString <| model.remoteType))
                                }
                            , let
                                invalidReason =
                                    -- If the input is pristine & blank, don't evaluate it.
                                    if model.remoteTypeInputInteraction == Pristine && String.isEmpty (remoteToStringInput <| getRemote rule) then
                                        Nothing

                                    else if not <| isValidCidr rule.ethertype (remoteToStringInput <| getRemote rule) then
                                        Just <| "Invalid CIDR for " ++ etherTypeToString rule.ethertype ++ " Prefix."

                                    else
                                        Nothing
                              in
                              renderInvalidReason invalidReason
                            ]

                    GroupId ->
                        Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
                            [ Text.body "Group ID"
                            , Style.Widgets.Select.select
                                []
                                context.palette
                                { onChange = \text -> GotRemote <| Just <| RemoteGroupUuid <| Maybe.withDefault "" text
                                , options = project.securityGroups |> RDPP.withDefault [] |> sortedSecurityGroups |> List.map (\sg -> ( sg.uuid, sg.name ))
                                , selected = Just <| remoteToStringInput <| getRemote rule
                                , label = "Group ID"
                                }
                            , let
                                invalidReason =
                                    -- If the input is pristine & blank, don't evaluate it.
                                    if model.remoteTypeInputInteraction == Pristine && String.isEmpty (remoteToStringInput <| getRemote rule) then
                                        Nothing

                                    else if String.isEmpty <| String.trim <| (remoteToStringInput <| getRemote rule) then
                                        Just "Group ID is required."

                                    else
                                        Nothing
                              in
                              renderInvalidReason invalidReason
                            ]
                ]
            ]
        , Input.text
            (VH.inputItemAttributes context.palette ++ [ consistentHeight, Element.width Element.fill ])
            { text = Maybe.withDefault "" rule.description
            , placeholder = Just <| Input.placeholder [] (Element.text "Optional")
            , onChange = \text -> GotRuleUpdate { rule | description = Just text }
            , label = Input.labelAbove [] (Element.text "Rule Description")
            }
        ]


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    Element.column [ Element.padding spacer.px8 ]
        [ form
            context
            project
            model
        ]
