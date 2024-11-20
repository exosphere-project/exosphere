module Page.SecurityGroupForm exposing (Model, Msg(..), init, update, view)

import Element
import Element.Border as Border
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import List
import List.Extra
import OpenStack.SecurityGroupRule
    exposing
        ( SecurityGroupRule
        , SecurityGroupRuleUuid
        )
import OpenStack.Types exposing (SecurityGroup, SecurityGroupUuid)
import Page.SecurityGroupRuleForm as SecurityGroupRuleForm
import Page.SecurityGroupRulesTable as SecurityGroupRulesTable
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Types.Project exposing (Project)
import View.Helpers as VH
import View.Types


type Msg
    = SecurityGroupRuleFormMsg SecurityGroupRuleForm.Msg
    | GotAddRule
    | GotDeleteRule SecurityGroupRuleUuid
    | GotDescription (Maybe String)
    | GotDoneEditingRule
    | GotEditRule SecurityGroupRuleUuid
    | GotName String
    | GotRequestCreateSecurityGroup


type alias Model =
    { uuid : Maybe SecurityGroupUuid
    , name : String
    , description : Maybe String
    , rules : List SecurityGroupRule
    , securityGroupRuleForm : Maybe SecurityGroupRuleForm.Model
    }


init : { name : String } -> Model
init { name } =
    -- TODO: Optionally init from an existing security group.
    { uuid = Nothing
    , name = name
    , description = Nothing
    , rules = []
    , securityGroupRuleForm = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SecurityGroupRuleFormMsg ruleFormMsg ->
            case model.securityGroupRuleForm of
                Just ruleForm ->
                    let
                        ( updatedRuleForm, ruleFormCmd ) =
                            SecurityGroupRuleForm.update ruleFormMsg ruleForm
                    in
                    ( { model
                        | securityGroupRuleForm = Just updatedRuleForm
                        , rules =
                            List.map
                                (\r ->
                                    if r.uuid == updatedRuleForm.rule.uuid then
                                        updatedRuleForm.rule

                                    else
                                        r
                                )
                                model.rules
                      }
                    , Cmd.map SecurityGroupRuleFormMsg ruleFormCmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotName name ->
            ( { model | name = name }, Cmd.none )

        GotDescription description ->
            ( { model | description = description }, Cmd.none )

        GotAddRule ->
            let
                newRule =
                    SecurityGroupRuleForm.newBlankRule (List.length model.rules)
            in
            ( { model
                | rules = model.rules ++ [ newRule ]
                , securityGroupRuleForm = Just <| SecurityGroupRuleForm.init newRule
              }
            , Cmd.none
            )

        GotDeleteRule ruleUuid ->
            ( { model
                | rules =
                    List.filter (\r -> r.uuid /= ruleUuid) model.rules
                , securityGroupRuleForm =
                    if Maybe.map (\f -> f.rule.uuid == ruleUuid) model.securityGroupRuleForm |> Maybe.withDefault False then
                        Nothing

                    else
                        model.securityGroupRuleForm
              }
            , Cmd.none
            )

        GotDoneEditingRule ->
            ( { model
                | securityGroupRuleForm = Nothing
              }
            , Cmd.none
            )

        GotEditRule ruleUuid ->
            ( { model
                | securityGroupRuleForm =
                    let
                        rule : Maybe.Maybe SecurityGroupRule
                        rule =
                            List.Extra.find (\r -> r.uuid == ruleUuid) model.rules
                    in
                    Just <|
                        SecurityGroupRuleForm.init <|
                            case rule of
                                Just r ->
                                    r

                                Nothing ->
                                    SecurityGroupRuleForm.newBlankRule (List.length model.rules)
              }
            , Cmd.none
            )

        GotRequestCreateSecurityGroup ->
            ( model
              -- TODO: Send a command to create the security group.
            , Cmd.none
            )


rulesList :
    View.Types.Context
    -> Project
    -> Model
    -> { rules : List SecurityGroupRule, securityGroupForUuid : SecurityGroupUuid -> Maybe SecurityGroup }
    -> Element.Element Msg
rulesList context project model { rules, securityGroupForUuid } =
    let
        customiser : SecurityGroupRulesTable.RulesTableRowCustomiser Msg
        customiser rule =
            let
                isRuleBeingEdited =
                    Maybe.map (\f -> f.rule.uuid == rule.uuid) model.securityGroupRuleForm |> Maybe.withDefault False
            in
            { leftElementForRow = Nothing
            , rightElementForRow =
                Just <|
                    Element.row [ Element.spacing spacer.px12 ]
                        [ if isRuleBeingEdited then
                            Button.button Button.Primary context.palette { text = "Done", onPress = Just <| GotDoneEditingRule }

                          else
                            Button.button Button.Secondary context.palette { text = "Edit", onPress = Just <| GotEditRule rule.uuid }
                        , Button.button Button.DangerSecondary context.palette { text = "Delete", onPress = Just <| GotDeleteRule rule.uuid }
                        ]
            , styleForRow = SecurityGroupRulesTable.defaultRowStyle ++ [ Element.centerY ]
            }
    in
    SecurityGroupRulesTable.rulesTableWithRowCustomiser
        context
        (GetterSetters.projectIdentifier project)
        { rules = rules, securityGroupForUuid = securityGroupForUuid }
        customiser


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.column [ Element.paddingXY spacer.px8 0, Element.spacing spacer.px12, Element.width Element.fill ]
            [ Input.text
                (VH.inputItemAttributes context.palette ++ [ Element.width <| Element.minimum 240 Element.fill ])
                { text = model.name
                , placeholder = Nothing
                , onChange = GotName
                , label = Input.labelLeft [] (Element.text "Name")
                }
            , Input.text
                (VH.inputItemAttributes context.palette ++ [ Element.width <| Element.minimum 240 Element.fill ])
                { text = model.description |> Maybe.withDefault ""
                , placeholder = Just <| Input.placeholder [] (Element.text "Optional")
                , onChange = GotDescription << Just
                , label = Input.labelLeft [] (Element.text "Description")
                }
            ]
        , rulesList
            context
            project
            model
            { rules = model.rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }
        , case model.securityGroupRuleForm of
            Just ruleForm ->
                let
                    -- Horizontal rule
                    hr =
                        Element.row [ Element.paddingEach { bottom = spacer.px12, left = 0, right = 0, top = 0 }, Element.width Element.fill ]
                            [ Element.row
                                [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                                , Border.color (context.palette.neutral.border |> SH.toElementColor)
                                , Element.width Element.fill
                                ]
                                [ Element.none ]
                            ]
                in
                Element.column [ Element.spacing spacer.px12 ]
                    [ hr
                    , SecurityGroupRuleForm.view
                        context
                        ruleForm
                        |> Element.map SecurityGroupRuleFormMsg
                    ]

            Nothing ->
                Element.none
        , Element.row [ Element.spaceEvenly, Element.width Element.fill ]
            [ let
                variant =
                    if List.length model.rules > 0 then
                        Button.Secondary

                    else
                        Button.Primary
              in
              Button.button variant context.palette { text = "Add Rule", onPress = Just GotAddRule }
            , let
                variant =
                    if List.length model.rules > 0 && model.securityGroupRuleForm == Nothing then
                        Button.Primary

                    else
                        Button.Secondary
              in
              Button.button variant
                context.palette
                { text =
                    String.join " "
                        [ "Create"
                        , context.localization.securityGroup
                            |> Helpers.String.toTitleCase
                        ]
                , onPress = Just GotRequestCreateSecurityGroup
                }
            ]
        ]
