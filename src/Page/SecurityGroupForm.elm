module Page.SecurityGroupForm exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import List
import OpenStack.SecurityGroupRule
    exposing
        ( SecurityGroupRule
        , SecurityGroupRuleDirection(..)
        , SecurityGroupRuleEthertype(..)
        , SecurityGroupRuleProtocol(..)
        , SecurityGroupRuleUuid
        , directionToString
        , etherTypeToString
        , portRangeToString
        , protocolToString
        , stringToSecurityGroupRuleDirection
        )
import OpenStack.Types exposing (SecurityGroup, SecurityGroupUuid)
import Route
import Style.Helpers as SH
import Style.Widgets.Select
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.Project exposing (Project)
import View.Helpers as VH
import View.Types


type Msg
    = GotDirection SecurityGroupRuleUuid SecurityGroupRuleDirection
    | NoOp


type alias Model =
    { uuid : Maybe SecurityGroupUuid
    , name : String
    , description : Maybe String
    , rules : List SecurityGroupRule
    }


init : { name : String } -> Model
init { name } =
    -- TODO: Optionally init from an existing security group.
    { uuid = Nothing
    , name = name
    , description = Nothing
    , rules = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDirection ruleUuid direction ->
            ( { model
                | rules =
                    List.map
                        (\rule ->
                            if rule.uuid == ruleUuid then
                                { rule | direction = direction }

                            else
                                rule
                        )
                        model.rules
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


allDirections : List SecurityGroupRuleDirection
allDirections =
    [ Ingress, Egress ]


directionOptions : List ( SecurityGroupRuleDirection, String )
directionOptions =
    List.map (\direction -> ( direction, directionToString direction )) allDirections


rulesTable :
    View.Types.Context
    -> ProjectIdentifier
    -> { rules : List SecurityGroupRule, securityGroupForUuid : SecurityGroupUuid -> Maybe SecurityGroup }
    -> Element.Element Msg
rulesTable context projectId { rules, securityGroupForUuid } =
    case List.length rules of
        0 ->
            Element.text "(none)"

        _ ->
            let
                header text =
                    Element.el [ Font.heavy ] <| Element.text text

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
            in
            Element.table
                [ Element.spacing spacer.px16 ]
                { data =
                    rules |> GetterSetters.sortedSecurityGroupRules securityGroupForUuid
                , columns =
                    [ { header = header "Direction"
                      , width = Element.shrink
                      , view =
                            \item ->
                                -- Text.body <|
                                --     directionToString <|
                                --         item.direction
                                Style.Widgets.Select.select
                                    []
                                    context.palette
                                    { label = "Choose a direction"
                                    , onChange =
                                        \direction ->
                                            case direction of
                                                Just dir ->
                                                    GotDirection item.uuid (stringToSecurityGroupRuleDirection dir)

                                                Nothing ->
                                                    NoOp
                                    , options = List.map (\( dir, label ) -> ( directionToString dir, label )) directionOptions
                                    , selected = Just (directionToString Ingress)
                                    }
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


view : View.Types.Context -> Project -> SecurityGroup -> Element.Element Msg
view context project securityGroup =
    Element.column []
        [ rulesTable
            context
            (GetterSetters.projectIdentifier project)
            { rules = securityGroup.rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }
        ]
