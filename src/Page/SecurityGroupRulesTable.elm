module Page.SecurityGroupRulesTable exposing (defaultRowStyle, rulesTable, rulesTableWithRowStyle, view)

import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import List exposing (sortBy)
import OpenStack.SecurityGroupRule
    exposing
        ( SecurityGroupRule
        , SecurityGroupRuleEthertype(..)
        , SecurityGroupRuleProtocol(..)
        , directionToString
        , etherTypeToString
        , portRangeToString
        , protocolToString
        )
import OpenStack.Types exposing (SecurityGroup, SecurityGroupUuid)
import Route
import Style.Helpers as SH
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.Project exposing (Project)
import View.Helpers as VH
import View.Types


rulesTable : View.Types.Context -> ProjectIdentifier -> { rules : List SecurityGroupRule, securityGroupForUuid : SecurityGroupUuid -> Maybe SecurityGroup } -> Element.Element msg
rulesTable context projectId { rules, securityGroupForUuid } =
    rulesTableWithRowStyle
        context
        projectId
        { rules = rules, securityGroupForUuid = securityGroupForUuid }
        (always defaultRowStyle)


defaultRowStyle : List (Element.Attribute msg)
defaultRowStyle =
    [ Element.padding spacer.px8 ]


rulesTableWithRowStyle : View.Types.Context -> ProjectIdentifier -> { rules : List SecurityGroupRule, securityGroupForUuid : SecurityGroupUuid -> Maybe SecurityGroup } -> (SecurityGroupRule -> List (Element.Attribute msg)) -> Element.Element msg
rulesTableWithRowStyle context projectId { rules, securityGroupForUuid } styleForRow =
    case List.length rules of
        0 ->
            Element.text "(none)"

        _ ->
            let
                header text =
                    Element.el (Font.heavy :: defaultRowStyle) <| Element.text text

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

                container rule =
                    Element.el (styleForRow rule)
            in
            Element.table
                [ Element.spacing 0
                ]
                { data = sortBy (\item -> directionToString item.direction) rules
                , columns =
                    [ { header = header "Direction"
                      , width = Element.shrink
                      , view =
                            \item ->
                                container item <| Text.body <| directionToString <| item.direction
                      }
                    , { header = header "Ether Type"
                      , width = Element.shrink
                      , view =
                            \item ->
                                container item <| Text.body <| etherTypeToString <| item.ethertype
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
                                container item <| Text.body <| protocolString
                      }
                    , { header = header "Port Range"
                      , width = Element.shrink
                      , view =
                            \item ->
                                container item <| Text.body <| portRangeToString item
                      }
                    , { header = header "Remote"
                      , width = Element.shrink
                      , view =
                            \item ->
                                let
                                    cell =
                                        case ( item.remoteIpPrefix, item.remoteGroupUuid ) of
                                            -- Either IP prefix or remote security group.
                                            ( Just ipPrefix, _ ) ->
                                                Text.body <| ipPrefix

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
                                                -- Assume 'any' address when neither remote group nor IP prefix are specified
                                                case item.ethertype of
                                                    Ipv4 ->
                                                        Text.body "0.0.0.0/0"

                                                    Ipv6 ->
                                                        Text.body "::/0"

                                                    _ ->
                                                        Text.body "-"
                                in
                                container item <| cell
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
                                    (styleForRow item)
                                    (Text.body <|
                                        if String.isEmpty description then
                                            "-"

                                        else
                                            description
                                    )
                      }
                    ]
                }


view : View.Types.Context -> Project -> SecurityGroupUuid -> Element.Element msg
view context project securityGroupUuid =
    VH.renderRDPP context
        project.securityGroups
        context.localization.securityGroup
        (\_ ->
            {- Attempt to look up a given security group uuid; if a security group is found, call render. -}
            case GetterSetters.securityGroupLookup project securityGroupUuid of
                Just securityGroup ->
                    rulesTable
                        context
                        (GetterSetters.projectIdentifier project)
                        { rules = securityGroup.rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.securityGroup
                            , "found"
                            ]
        )
