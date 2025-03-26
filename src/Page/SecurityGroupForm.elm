module Page.SecurityGroupForm exposing (Model, Msg(..), init, initWithSecurityGroup, securityGroupTemplateFromForm, securityGroupUpdateFromForm, update, view)

import Element exposing (paddingEach)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Cidr exposing (isValidCidr)
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.String exposing (pluralizeCount, removeEmptiness)
import Json.Decode as Decode
import List
import List.Extra
import OpenStack.Error
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
        )
import OpenStack.Types exposing (SecurityGroup, SecurityGroupTemplate, SecurityGroupUpdate, SecurityGroupUuid, ServerUuid)
import Page.SecurityGroupRuleForm as SecurityGroupRuleForm
import Page.SecurityGroupRulesTable as SecurityGroupRulesTable
import Rest.Neutron
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Grid exposing (GridCell(..), GridRow(..), grid, scrollableCell)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Error as Error
import Types.Project exposing (Project)
import Types.SecurityGroupActions as SecurityGroupActions
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Forms as Forms
import View.Helpers as VH
import View.Types
import Widget


type Msg
    = SecurityGroupRuleFormMsg SecurityGroupRuleForm.Msg
    | GotAddRule
    | GotDeleteRule SecurityGroupRuleUuid
    | GotDescription (Maybe String)
    | GotDoneEditingRule
    | GotEditRule SecurityGroupRuleUuid
    | GotName String
    | GotCancel
    | GotRequestCreateSecurityGroup (Maybe ServerUuid)
    | GotCreateSecurityGroupResult SecurityGroupTemplate (Result Error.HttpErrorWithBody SecurityGroup)
    | GotRequestUpdateSecurityGroup SecurityGroupUuid


type alias Model =
    { uuid : Maybe SecurityGroupUuid
    , name : String
    , description : Maybe String
    , rules : List SecurityGroupRule
    , securityGroupRuleForm : Maybe SecurityGroupRuleForm.Model
    , submitted : Bool
    , creationInProgress : Bool
    , creationError : Maybe String
    }


init : { name : String } -> Model
init { name } =
    { uuid = Nothing
    , name = name
    , description = Nothing
    , rules = []
    , securityGroupRuleForm = Nothing
    , submitted = False
    , creationInProgress = False
    , creationError = Nothing
    }


initWithSecurityGroup : SecurityGroup -> Model
initWithSecurityGroup securityGroup =
    { uuid = Just securityGroup.uuid
    , name = securityGroup.name
    , description = securityGroup.description
    , rules = securityGroup.rules
    , securityGroupRuleForm = Nothing
    , submitted = False
    , creationInProgress = False
    , creationError = Nothing
    }


securityGroupUpdateFromForm : Model -> SecurityGroupUpdate
securityGroupUpdateFromForm model =
    { name = String.trim model.name
    , description = removeEmptiness model.description
    , rules = List.map OpenStack.SecurityGroupRule.securityGroupRuleToTemplate model.rules
    }


securityGroupTemplateFromForm : Model -> SecurityGroupTemplate
securityGroupTemplateFromForm model =
    { name = model.name
    , description = removeEmptiness model.description
    , regionId = Nothing
    , rules = List.map OpenStack.SecurityGroupRule.securityGroupRuleToTemplate model.rules
    }


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg { viewContext } project model =
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
                        , submitted = False
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
                    , SharedMsg.NoOp
                    )

                Nothing ->
                    ( model, Cmd.none, SharedMsg.NoOp )

        GotName name ->
            ( { model | name = name, submitted = False }, Cmd.none, SharedMsg.NoOp )

        GotDescription description ->
            ( { model | description = description, submitted = False }, Cmd.none, SharedMsg.NoOp )

        GotAddRule ->
            let
                newRule =
                    SecurityGroupRuleForm.newBlankRule (List.length model.rules)
            in
            ( { model
                | rules = model.rules ++ [ newRule ]
                , securityGroupRuleForm = Just <| SecurityGroupRuleForm.init newRule
                , submitted = False
              }
            , Cmd.none
            , SharedMsg.NoOp
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
                , submitted = False
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotDoneEditingRule ->
            ( { model | securityGroupRuleForm = Nothing }
            , Cmd.none
            , SharedMsg.NoOp
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
            , SharedMsg.NoOp
            )

        GotCancel ->
            -- Expect the form owner to clear the form's model.
            ( model, Cmd.none, SharedMsg.NoOp )

        GotRequestCreateSecurityGroup maybeServerUuid ->
            let
                newModel =
                    { model
                        | submitted = True
                        , creationInProgress = True
                        , creationError = Nothing
                    }
            in
            ( newModel
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestCreateSecurityGroup
                    (securityGroupTemplateFromForm <| newModel)
                    maybeServerUuid
            )

        GotCreateSecurityGroupResult template result ->
            -- Names ought to be unique within the project.
            if model.name == template.name then
                case result of
                    Ok securityGroup ->
                        let
                            newModel =
                                { model
                                  -- Update the form to use the newly available security group uuid.
                                  -- Retain the form state rules since those changes are probably still pending.
                                  -- (We expect that because new security groups are created with default rules.)
                                    | uuid = Just securityGroup.uuid
                                    , name = securityGroup.name
                                    , description = securityGroup.description
                                    , creationInProgress = False
                                    , creationError = Nothing
                                }
                        in
                        ( newModel
                        , Cmd.none
                        , SharedMsg.NoOp
                        )

                    Err httpError ->
                        let
                            error =
                                Decode.decodeString
                                    (Decode.field (OpenStack.Error.fieldForErrorDomain OpenStack.Error.NeutronError) Rest.Neutron.neutronErrorDecoder)
                                    httpError.body

                            errorMessage =
                                case error of
                                    Ok neutronError ->
                                        neutronError.message

                                    Err _ ->
                                        if String.isEmpty httpError.body then
                                            "An error occurred while creating the " ++ viewContext.localization.securityGroup ++ "."

                                        else
                                            httpError.body
                        in
                        ( { model
                            | creationInProgress = False
                            , creationError = Just errorMessage
                          }
                        , Cmd.none
                        , SharedMsg.NoOp
                        )

            else
                ( model, Cmd.none, SharedMsg.NoOp )

        GotRequestUpdateSecurityGroup securityGroupUuid ->
            case GetterSetters.securityGroupLookup project securityGroupUuid of
                Just existingSecurityGroup ->
                    let
                        newModel =
                            { model | submitted = True }
                    in
                    ( newModel
                    , Cmd.none
                    , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                        SharedMsg.RequestUpdateSecurityGroup existingSecurityGroup (securityGroupUpdateFromForm <| newModel)
                    )

                _ ->
                    ( model, Cmd.none, SharedMsg.NoOp )


isRuleValid : SecurityGroupRule -> Bool
isRuleValid { ethertype, direction, protocol, portRangeMin, portRangeMax, remoteIpPrefix, remoteGroupUuid } =
    let
        isEthertypeValid =
            case ethertype of
                UnsupportedEthertype _ ->
                    False

                _ ->
                    True

        isDirectionValid =
            case direction of
                UnsupportedDirection _ ->
                    False

                _ ->
                    True

        isProtocolValid =
            case protocol of
                Just (UnsupportedProtocol _) ->
                    False

                _ ->
                    True

        isPortRangeValid =
            case ( portRangeMin, portRangeMax ) of
                ( Just min, Just max ) ->
                    min <= max

                ( Just _, Nothing ) ->
                    False

                ( Nothing, Just _ ) ->
                    False

                ( Nothing, Nothing ) ->
                    True

        isRemoteValid =
            case ( remoteIpPrefix, remoteGroupUuid ) of
                ( Just _, Just _ ) ->
                    False

                ( Just ipPrefix, Nothing ) ->
                    isValidCidr ethertype ipPrefix

                ( Nothing, Just groupUuid ) ->
                    not <| String.isEmpty <| String.trim <| groupUuid

                ( Nothing, Nothing ) ->
                    True
    in
    isEthertypeValid
        && isDirectionValid
        && isProtocolValid
        && isPortRangeValid
        && isRemoteValid


rulesGrid :
    View.Types.Context
    -> Project
    -> Model
    -> List SecurityGroupRule
    -> Element.Element Msg
rulesGrid context project model rules =
    let
        dividerAttrs =
            [ Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <|
                SH.toElementColor context.palette.neutral.border
            , paddingEach { top = 0, bottom = spacer.px12, left = 0, right = 0 }
            ]

        headerRow =
            GridRow dividerAttrs
                [ GridCell [ Element.width <| Element.px 30 ] Element.none
                , GridCell [] (Element.el [ Font.heavy ] <| Element.text "Direction")
                , GridCell [] (Element.el [ Font.heavy ] <| Element.text "Ether Type")
                , GridCell [] (Element.el [ Font.heavy ] <| Element.text "Protocol")
                , GridCell [] (Element.el [ Font.heavy ] <| Element.text "Port Range")
                , GridCell [] (Element.el [ Font.heavy ] <| Element.text "Remote")
                , GridCell [ Element.width <| Element.fillPortion 1 ] (Element.el [ Font.heavy ] <| Element.text "Description")
                , GridCell [ Element.width <| Element.px 165 ] Element.none
                ]

        ruleRow rule =
            GridRow dividerAttrs
                [ GridCell [ Element.width <| Element.px 30 ]
                    (if isRuleValid rule then
                        Element.none

                     else
                        Validation.invalidIcon context.palette
                    )
                , GridCell [] (Text.body <| directionToString rule.direction)
                , GridCell [] (Text.body <| etherTypeToString rule.ethertype)
                , GridCell [] (Text.body <| protocolToString <| Maybe.withDefault AnyProtocol rule.protocol)
                , GridCell [] (scrollableCell [ Element.width Element.fill ] <| Text.body <| portRangeToString rule)
                , GridCell [] (scrollableCell [ Element.width Element.fill ] <| SecurityGroupRulesTable.renderRemote context (GetterSetters.projectIdentifier project) (GetterSetters.securityGroupLookup project) rule)
                , GridCell [ Element.width <| Element.fillPortion 1 ]
                    (let
                        description =
                            Maybe.withDefault "-" rule.description
                     in
                     scrollableCell [ Element.width Element.fill ] <|
                        Text.body <|
                            if String.isEmpty description then
                                "-"

                            else
                                description
                    )
                , GridCell [ Element.width <| Element.px 165 ]
                    (let
                        isRuleBeingEdited =
                            Maybe.map (\f -> f.rule.uuid == rule.uuid) model.securityGroupRuleForm |> Maybe.withDefault False
                     in
                     Element.row [ Element.spacing spacer.px12 ]
                        [ if isRuleBeingEdited then
                            Button.button Button.Primary context.palette { text = "Done", onPress = Just <| GotDoneEditingRule }

                          else
                            Button.button Button.Secondary context.palette { text = "Edit", onPress = Just <| GotEditRule rule.uuid }
                        , Button.button Button.DangerSecondary context.palette { text = "Delete", onPress = Just <| GotDeleteRule rule.uuid }
                        ]
                    )
                ]

        formRow =
            GridRow dividerAttrs
                [ GridCell []
                    (case model.securityGroupRuleForm of
                        Just ruleForm ->
                            Element.column [ Element.spacing spacer.px12 ]
                                [ SecurityGroupRuleForm.view
                                    context
                                    ruleForm
                                    |> Element.map SecurityGroupRuleFormMsg
                                , Element.el [ Element.alignRight, Element.paddingXY spacer.px12 0 ] <| Button.button Button.Primary context.palette { text = "Done", onPress = Just <| GotDoneEditingRule }
                                ]

                        Nothing ->
                            Element.none
                    )
                ]

        renderRow rule =
            let
                isRuleBeingEdited =
                    Maybe.map (\f -> f.rule.uuid == rule.uuid) model.securityGroupRuleForm |> Maybe.withDefault False
            in
            if isRuleBeingEdited then
                formRow

            else
                ruleRow rule
    in
    grid
        [ Element.spacing spacer.px12 ]
        (headerRow
            :: (if List.isEmpty rules then
                    [ GridRow dividerAttrs [ GridCell [] (Text.text Text.Body [ Element.centerX ] "(none)") ] ]

                else
                    List.map renderRow rules
               )
        )


warningSecurityGroupAffectsServers :
    View.Types.Context
    -> Project
    -> SecurityGroupUuid
    -> Maybe ServerUuid
    -> Element.Element msg
warningSecurityGroupAffectsServers context project securityGroupUuid maybeServerUuid =
    case Forms.securityGroupAffectsServersWarning context project securityGroupUuid maybeServerUuid "editing" of
        Just warning ->
            Element.el
                [ Element.width Element.shrink, Element.centerX ]
            <|
                Validation.warningMessage context.palette <|
                    warning

        Nothing ->
            Element.none


view : View.Types.Context -> Project -> Time.Posix -> Model -> Maybe ServerUuid -> Element.Element Msg
view context project currentTime model maybeServerUuid =
    let
        existingSecurityGroupName =
            model.uuid |> Maybe.andThen (GetterSetters.securityGroupLookup project) |> Maybe.map .name

        numberOfInvalidRules =
            List.length <| List.filter (not << isRuleValid) model.rules

        renderInvalidReason reason =
            case reason of
                Just r ->
                    r |> Validation.invalidMessage context.palette

                Nothing ->
                    Element.none

        isNameValid =
            String.isEmpty (String.trim model.name) == False

        invalidReason =
            if isNameValid then
                Nothing

            else
                Just "Name is required"

        isRuleSetValid =
            numberOfInvalidRules == 0

        formPadding =
            Element.paddingEach { top = 0, right = 0, bottom = 0, left = spacer.px64 + spacer.px12 }

        action =
            (case model.uuid of
                Just securityGroupUuid ->
                    GetterSetters.getSecurityGroupActions project (SecurityGroupActions.ExtantGroup securityGroupUuid)

                Nothing ->
                    GetterSetters.getSecurityGroupActions project (SecurityGroupActions.NewGroup model.name)
            )
                |> Maybe.withDefault SecurityGroupActions.initSecurityGroupAction

        isUpToDate =
            model.submitted
                && not model.creationInProgress
                && not action.pendingCreation
                && model.creationError
                == Nothing
                && action.pendingSecurityGroupChanges
                == SecurityGroupActions.initPendingSecurityGroupChanges
                && action.pendingRuleChanges
                == SecurityGroupActions.initPendingRulesChanges
    in
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.column [ Element.paddingXY spacer.px8 0, Element.spacing spacer.px12, Element.width Element.fill ] <|
            Input.text
                (VH.inputItemAttributes context.palette
                    ++ [ Element.width <| Element.minimum 240 Element.fill ]
                )
                { text = model.name
                , placeholder = Nothing
                , onChange = GotName
                , label =
                    Input.labelLeft []
                        (VH.requiredLabel context.palette (Element.text "Name"))
                }
                :: (if existingSecurityGroupName == Just (String.trim model.name) then
                        []

                    else
                        (Text.text Text.Small [ formPadding ] <|
                            String.join " "
                                [ "(Note:"
                                , Helpers.String.toTitleCase <| Helpers.String.pluralize context.localization.securityGroup
                                , "require a unique name.)"
                                ]
                        )
                            :: Forms.resourceNameAlreadyExists context project currentTime { resource = Forms.SecurityGroup model.name, onSuggestionPressed = \suggestion -> GotName suggestion }
                   )
                ++ [ Element.el [ formPadding ] <| renderInvalidReason invalidReason
                   , Input.text
                        (VH.inputItemAttributes context.palette ++ [ Element.width <| Element.minimum 240 Element.fill ])
                        { text = model.description |> Maybe.withDefault ""
                        , placeholder = Just <| Input.placeholder [] (Element.text "Optional")
                        , onChange = GotDescription << Just
                        , label = Input.labelLeft [] (Element.text "Description")
                        }
                   ]
        , rulesGrid
            context
            project
            model
            model.rules
        , case model.uuid of
            Just securityGroupUuid ->
                warningSecurityGroupAffectsServers context project securityGroupUuid maybeServerUuid

            _ ->
                Element.none
        , if isRuleSetValid then
            Element.none

          else
            let
                locale =
                    context.locale

                invalidRulesCount =
                    humanCount
                        { locale | decimals = Exact 0 }
                        numberOfInvalidRules
            in
            Element.el
                [ Element.width Element.shrink, Element.centerX ]
            <|
                Validation.invalidMessage context.palette <|
                    String.join " " [ "Please review", invalidRulesCount, "rule" |> pluralizeCount numberOfInvalidRules, "with problems." ]
        , Element.row [ Element.spaceEvenly, Element.width Element.fill ]
            [ let
                variant =
                    if List.length model.rules > 0 then
                        Button.Secondary

                    else
                        Button.Primary
              in
              Button.button variant context.palette { text = "Add Rule", onPress = Just GotAddRule }
            , -- Cancel
              if isUpToDate then
                Element.none

              else
                Element.el
                    [ Element.paddingXY spacer.px8 0, Element.width Element.shrink, Element.alignRight ]
                    (Button.button Button.DangerSecondary context.palette { text = "Cancel", onPress = Just GotCancel })
            , -- Done
              if isUpToDate then
                Button.button Button.Primary context.palette { text = "Done", onPress = Just GotCancel }

              else
                Element.none
            , -- Submit
              if isUpToDate then
                Element.none

              else
                let
                    variant =
                        if List.length model.rules > 0 && model.securityGroupRuleForm == Nothing then
                            Button.Primary

                        else
                            Button.Secondary
                in
                Button.button variant
                    context.palette
                    { text =
                        let
                            isExistingRule =
                                model.uuid /= Nothing
                        in
                        String.join " "
                            [ if isExistingRule then
                                "Update"

                              else
                                "Create"
                            , context.localization.securityGroup
                                |> Helpers.String.toTitleCase
                            ]
                    , onPress =
                        let
                            isFormValid =
                                isRuleSetValid && isNameValid
                        in
                        if isFormValid then
                            case model.uuid of
                                Just securityGroupUuid ->
                                    Just (GotRequestUpdateSecurityGroup securityGroupUuid)

                                Nothing ->
                                    Just <| GotRequestCreateSecurityGroup maybeServerUuid

                        else
                            Nothing
                    }
            ]
        , if action.pendingCreation then
            Element.row [ Element.spacing spacer.px16, Element.centerX ]
                [ Widget.circularProgressIndicator
                    (SH.materialStyle context.palette).progressIndicator
                    Nothing
                , Element.text <|
                    String.join " "
                        [ "Creating"
                        , context.localization.securityGroup
                            |> Helpers.String.toTitleCase
                        , "..."
                        ]
                ]

          else
            Element.none
        , let
            updates =
                action.pendingSecurityGroupChanges.updates
                    + action.pendingRuleChanges.creations
                    + action.pendingRuleChanges.deletions
          in
          if updates > 0 then
            Element.row [ Element.spacing spacer.px16, Element.centerX ]
                [ Widget.circularProgressIndicator
                    (SH.materialStyle context.palette).progressIndicator
                    Nothing
                , Element.text <|
                    String.join " "
                        [ updates |> String.fromInt
                        , "update" |> Helpers.String.pluralizeCount updates
                        , "remaining..."
                        ]
                ]

          else
            Element.none
        , let
            errors =
                action.pendingSecurityGroupChanges.errors
                    ++ action.pendingRuleChanges.errors
                    ++ (case model.creationError of
                            Just e ->
                                [ e ]

                            Nothing ->
                                []
                       )
          in
          if List.length errors > 0 then
            Element.el
                [ Element.width Element.shrink, Element.centerX ]
            <|
                Validation.invalidMessage context.palette <|
                    String.join ", " errors

          else
            Element.none
        , if isUpToDate then
            Element.el
                [ Element.width Element.shrink, Element.centerX ]
            <|
                Validation.validMessage context.palette <|
                    String.join " " [ Helpers.String.toTitleCase context.localization.securityGroup, "is up to date." ]

          else
            Element.none
        ]
