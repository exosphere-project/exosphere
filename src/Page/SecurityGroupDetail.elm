module Page.SecurityGroupDetail exposing (Model, Msg(..), init, update, view)

import DateFormat.Relative
import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Time
import List exposing (sortBy)
import OpenStack.SecurityGroupRule exposing (SecurityGroupRule, SecurityGroupRuleProtocol(..), directionToString, etherTypeToString, portRangeToString, protocolToString)
import OpenStack.Types as OSTypes exposing (SecurityGroup, SecurityGroupUuid)
import Route
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Card
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Time
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { securityGroupUuid : OSTypes.SecurityGroupUuid
    }


type Msg
    = SharedMsg SharedMsg.SharedMsg


init : OSTypes.SecurityGroupUuid -> Model
init securityGroupUuid =
    { securityGroupUuid = securityGroupUuid
    }


update : Msg -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


popoverMsgMapper : PopoverId -> Msg
popoverMsgMapper popoverId =
    SharedMsg <| SharedMsg.TogglePopover popoverId


view : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Element.Element Msg
view context project currentTimeAndZone model =
    VH.renderRDPP context
        project.securityGroups
        context.localization.securityGroup
        (\_ ->
            {- Attempt to look up a given security group uuid; if a security group is found, call render. -}
            case GetterSetters.securityGroupLookup project model.securityGroupUuid of
                Just securityGroup ->
                    render context project currentTimeAndZone model securityGroup

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.securityGroup
                            , "found"
                            ]
        )


createdAgoEtc :
    View.Types.Context
    ->
        { ago : ( String, Element.Element msg )
        , rules : ( String, String )
        }
    -> Element.Element msg
createdAgoEtc context { ago, rules } =
    let
        ( agoWord, agoContents ) =
            ago

        ( numberOfRules, rulesUnit ) =
            rules

        subduedText =
            Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
    in
    Element.wrappedRow
        [ Element.width Element.fill ]
    <|
        [ Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| agoWord ++ " ")
            , agoContents
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| "·")
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.text numberOfRules
            , Element.el [ subduedText ] (Element.text <| " " ++ rulesUnit)
            ]
        ]


securityGroupNameView : SecurityGroup -> Element.Element Msg
securityGroupNameView securityGroup =
    let
        name_ =
            VH.resourceName (Just securityGroup.name) securityGroup.uuid
    in
    Element.row
        [ Element.spacing spacer.px8 ]
        [ Text.text Text.ExtraLarge [] name_ ]


header : String -> Element.Element msg
header text =
    Element.el [ Font.heavy ] <| Element.text text


scrollableCell : List (Element.Attribute msg) -> Element.Element msg -> Element.Element msg
scrollableCell attrs msg =
    Element.el
        ([ Element.scrollbarX, Element.clipY ]
            ++ attrs
        )
        (Element.el
            [ -- HACK: A width needs to be set so that the cell expands responsively while having a horizontal scrollbar to contain overflow.
              Element.width (Element.px 0)
            ]
            msg
        )


rulesTable : View.Types.Context -> ProjectIdentifier -> { rules : List SecurityGroupRule, securityGroupForUuid : SecurityGroupUuid -> Maybe SecurityGroup } -> Element.Element Msg
rulesTable context projectId { rules, securityGroupForUuid } =
    case List.length rules of
        0 ->
            Element.text "(none)"

        _ ->
            Element.table
                [ Element.spacing spacer.px16
                ]
                { data = sortBy (\item -> directionToString item.direction) rules
                , columns =
                    [ { header = header "Direction"
                      , width = Element.shrink
                      , view =
                            \item ->
                                Text.body <| directionToString <| item.direction
                      }
                    , { header = header "Ether Type"
                      , width = Element.shrink
                      , view =
                            \item ->
                                Text.body <| etherTypeToString <| item.ethertype
                      }
                    , { header = header "Protocol"
                      , width = Element.shrink
                      , view =
                            \item ->
                                let
                                    protocolString =
                                        case item.protocol of
                                            Just protocol ->
                                                protocolToString protocol

                                            -- A `null` protocol implies "any".
                                            Nothing ->
                                                protocolToString AnyProtocol
                                in
                                Text.body <| protocolString
                      }
                    , { header = header "Port Range"
                      , width = Element.shrink
                      , view =
                            \item ->
                                Text.body <| portRangeToString item
                      }
                    , { header = header "Remote"
                      , width = Element.shrink
                      , view =
                            \item ->
                                case ( item.remoteIpPrefix, item.remoteGroupUuid ) of
                                    -- Either IP prefix or remote security group.
                                    ( Just ipPrefix, _ ) ->
                                        Text.body ipPrefix

                                    ( _, Just remoteGroupUuid ) ->
                                        -- Look up a the remote security group locally.
                                        case securityGroupForUuid remoteGroupUuid of
                                            Just securityGroup ->
                                                Element.link []
                                                    { url =
                                                        Route.toUrl context.urlPathPrefix
                                                            (Route.ProjectRoute projectId <|
                                                                Route.SecurityGroupDetail securityGroup.uuid
                                                            )
                                                    , label =
                                                        Element.el
                                                            [ Font.color (SH.toElementColor context.palette.primary) ]
                                                            (Element.text <|
                                                                VH.extendedResourceName
                                                                    (Just securityGroup.name)
                                                                    securityGroup.uuid
                                                                    context.localization.securityGroup
                                                            )
                                                    }

                                            Nothing ->
                                                Text.body <|
                                                    VH.extendedResourceName
                                                        Nothing
                                                        remoteGroupUuid
                                                        context.localization.securityGroup

                                    ( Nothing, Nothing ) ->
                                        Text.body "-"
                      }
                    , { header = header "Description"
                      , width = Element.fill
                      , view =
                            \item ->
                                let
                                    description =
                                        Maybe.withDefault "-" item.description
                                in
                                scrollableCell
                                    []
                                    (Text.body <|
                                        if String.isEmpty description then
                                            "-"

                                        else
                                            description
                                    )
                      }
                    ]
                }


render : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> SecurityGroup -> Element.Element Msg
render context project ( currentTime, _ ) _ securityGroup =
    let
        whenCreated =
            let
                timeDistanceStr =
                    DateFormat.Relative.relativeTime currentTime securityGroup.createdAt

                createdTimeText =
                    let
                        createdTimeFormatted =
                            Helpers.Time.humanReadableDateAndTime securityGroup.createdAt
                    in
                    Element.text ("Created on: " ++ createdTimeFormatted)

                toggleTipContents =
                    Element.column [] [ createdTimeText ]
            in
            Element.row
                [ Element.spacing spacer.px4 ]
                [ Element.text timeDistanceStr
                , Style.Widgets.ToggleTip.toggleTip
                    context
                    popoverMsgMapper
                    (Helpers.String.hyphenate
                        [ "createdTimeTip"
                        , project.auth.project.uuid
                        , securityGroup.uuid
                        ]
                    )
                    toggleTipContents
                    ST.PositionBottomLeft
                ]

        numberOfRulesString =
            let
                locale =
                    context.locale
            in
            humanCount
                { locale | decimals = Exact 0 }
                (List.length securityGroup.rules)

        ruleWord =
            "rule"

        rulesUnit =
            case securityGroup.rules of
                [ _ ] ->
                    ruleWord

                _ ->
                    ruleWord |> Helpers.String.pluralize

        description =
            case securityGroup.description of
                Just str ->
                    if String.isEmpty str then
                        Element.none

                    else
                        Element.row [ Element.padding spacer.px8 ]
                            [ Element.paragraph [ Element.width Element.fill ] <|
                                [ Element.text <| str ]
                            ]

                Nothing ->
                    Element.none

        tile : List (Element.Element Msg) -> List (Element.Element Msg) -> Element.Element Msg
        tile headerContents contents =
            Style.Widgets.Card.exoCard context.palette
                (Element.column
                    [ Element.width Element.fill
                    , Element.padding spacer.px16
                    , Element.spacing spacer.px16
                    ]
                    (List.concat
                        [ [ Element.row
                                (Text.subheadingStyleAttrs context.palette
                                    ++ Text.typographyAttrs Text.Large
                                    ++ [ Border.width 0 ]
                                )
                                headerContents
                          ]
                        , contents
                        ]
                    )
                )

        rules =
            rulesTable
                context
                (GetterSetters.projectIdentifier project)
                { rules = securityGroup.rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }
    in
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.row (Text.headingStyleAttrs context.palette)
            [ FeatherIcons.shield |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Text.text Text.ExtraLarge
                []
                (context.localization.securityGroup
                    |> Helpers.String.toTitleCase
                )
            , securityGroupNameView securityGroup
            ]
        , tile
            [ FeatherIcons.list |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Element.text "Info"
            , Element.el
                [ Text.fontSize Text.Tiny
                , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                , Element.alignBottom
                ]
                (copyableText context.palette
                    [ Element.width (Element.shrink |> Element.minimum 240) ]
                    securityGroup.uuid
                )
            ]
            [ description
            , createdAgoEtc
                context
                { ago = ( "created", whenCreated )
                , rules = ( numberOfRulesString, rulesUnit )
                }
            ]
        , tile
            [ FeatherIcons.lock
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , ruleWord
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
                |> Element.text
            ]
            [ rules
            ]
        ]
