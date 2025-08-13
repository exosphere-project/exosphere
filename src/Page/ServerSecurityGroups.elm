module Page.ServerSecurityGroups exposing (DataDependent, Model, Msg(..), init, update, view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons exposing (edit2)
import Helpers.GetterSetters as GetterSetters exposing (isDefaultSecurityGroup, sortedSecurityGroups)
import Helpers.List exposing (uniqueBy)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String
import List.Extra
import OpenStack.SecurityGroupRule exposing (isRuleShadowed, matchRule)
import OpenStack.Types as OSTypes exposing (securityGroupExoTags, securityGroupTaggedAs)
import Page.SecurityGroupForm as SecurityGroupForm
import Page.SecurityGroupRulesTable as SecurityGroupRulesTable
import Route
import Set
import Set.Extra
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.Icon
import Style.Widgets.IconButton exposing (clickableIcon)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag exposing (tagInfo, tagNeutral, tagPositive)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.SecurityGroupActions as SecurityGroupActions
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , selectedSecurityGroups : DataDependent (Set.Set OSTypes.SecurityGroupUuid)
    , securityGroupForm : Maybe SecurityGroupForm.Model
    }


type Msg
    = GotApplyServerSecurityGroupUpdates (List OSTypes.ServerSecurityGroupUpdate)
    | GotCreateSecurityGroupForm
    | GotEditSecurityGroupForm OSTypes.SecurityGroupUuid
    | GotDone
    | GotServerSecurityGroups OSTypes.ServerUuid
    | ToggleSelectedGroup OSTypes.SecurityGroupUuid
    | SharedMsg SharedMsg.SharedMsg
    | SecurityGroupFormMsg SecurityGroupForm.Msg


type DataDependent a
    = Uninitialised
    | Ready a


init : Project -> OSTypes.ServerUuid -> Model
init project serverUuid =
    let
        serverSecurityGroupsRdpp =
            GetterSetters.getServerSecurityGroups project serverUuid

        serverSecurityGroups =
            case serverSecurityGroupsRdpp.data of
                RDPP.DoHave serverSecurityGroups_ _ ->
                    Just serverSecurityGroups_

                _ ->
                    Nothing

        maybeProjectSecurityGroups =
            case project.securityGroups.data of
                RDPP.DoHave projectSecurityGroups_ _ ->
                    Just projectSecurityGroups_

                _ ->
                    Nothing
    in
    { serverUuid = serverUuid
    , selectedSecurityGroups =
        case ( serverSecurityGroups, maybeProjectSecurityGroups ) of
            ( Just serverSecurityGroups_, Just projectSecurityGroups_ ) ->
                List.map .uuid serverSecurityGroups_
                    |> Set.fromList
                    |> Set.intersect (Set.fromList <| List.map .uuid projectSecurityGroups_)
                    |> Ready

            _ ->
                Uninitialised
    , securityGroupForm =
        Nothing
    }


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg sharedModel project model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        GotApplyServerSecurityGroupUpdates serverSecurityGroupUpdates ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg model.serverUuid <|
                    SharedMsg.RequestServerSecurityGroupUpdates serverSecurityGroupUpdates
            )

        GotDone ->
            ( model, Route.pushUrl sharedModel.viewContext (Route.ProjectRoute (GetterSetters.projectIdentifier project) (Route.ServerDetail model.serverUuid)), SharedMsg.NoOp )

        GotServerSecurityGroups serverUuid ->
            -- SharedModel just updated with new security groups for this server.
            if serverUuid == model.serverUuid then
                let
                    selectedSecurityGroups =
                        -- If we don't have anything selected yet, show those already applied.
                        if model.selectedSecurityGroups == Uninitialised then
                            Ready (appliedSecurityGroupsUuids project serverUuid)

                        else
                            model.selectedSecurityGroups
                in
                ( { model | selectedSecurityGroups = selectedSecurityGroups }, Cmd.none, SharedMsg.NoOp )

            else
                ( model, Cmd.none, SharedMsg.NoOp )

        ToggleSelectedGroup securityGroupUuid ->
            case model.selectedSecurityGroups of
                Ready selectedSecurityGroups ->
                    let
                        newSelectedSecurityGroups =
                            Set.Extra.toggle securityGroupUuid selectedSecurityGroups
                    in
                    ( { model | selectedSecurityGroups = Ready newSelectedSecurityGroups }, Cmd.none, SharedMsg.NoOp )

                Uninitialised ->
                    -- We should never be here, because this is already a `Ready` when there are security groups to select.
                    ( model, Cmd.none, SharedMsg.NoOp )

        SecurityGroupFormMsg securityGroupFormMsg ->
            case securityGroupFormMsg of
                SecurityGroupForm.GotCancel ->
                    ( { model | securityGroupForm = Nothing }
                    , Cmd.none
                    , SharedMsg.NoOp
                    )

                SecurityGroupForm.GotCreateSecurityGroupResult _ result ->
                    -- Listen for successful creation of a security group.
                    let
                        ( newModel, newCmd, newSharedMsg ) =
                            updateUnderlyingForm sharedModel project model securityGroupFormMsg
                    in
                    case result of
                        Ok securityGroup ->
                            let
                                actions =
                                    GetterSetters.getSecurityGroupActions project (SecurityGroupActions.ExtantGroup securityGroup.uuid)
                                        |> Maybe.withDefault SecurityGroupActions.initSecurityGroupAction

                                selectedSecurityGroups =
                                    case ( model.selectedSecurityGroups, actions.pendingServerLinkage ) of
                                        ( Ready selected, Just serverUuid ) ->
                                            -- If the server is the one we're working with, select the new security group.
                                            -- (It will already have been applied by this time.)
                                            if serverUuid == model.serverUuid then
                                                Ready <| Set.insert securityGroup.uuid selected

                                            else
                                                model.selectedSecurityGroups

                                        _ ->
                                            model.selectedSecurityGroups
                            in
                            ( { newModel | selectedSecurityGroups = selectedSecurityGroups }, newCmd, newSharedMsg )

                        Err _ ->
                            ( newModel, newCmd, newSharedMsg )

                _ ->
                    updateUnderlyingForm sharedModel project model securityGroupFormMsg

        GotCreateSecurityGroupForm ->
            let
                securityGroupForm =
                    SecurityGroupForm.init { name = newSecurityGroupName project model.serverUuid }
            in
            ( { model | securityGroupForm = Just <| securityGroupForm }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotEditSecurityGroupForm securityGroupUuid ->
            let
                securityGroupForm =
                    case GetterSetters.securityGroupLookup project securityGroupUuid of
                        Just securityGroup ->
                            SecurityGroupForm.initWithSecurityGroup securityGroup

                        Nothing ->
                            SecurityGroupForm.init { name = newSecurityGroupName project model.serverUuid }
            in
            ( { model | securityGroupForm = Just <| securityGroupForm }
            , Cmd.none
            , SharedMsg.NoOp
            )


updateUnderlyingForm : SharedModel -> Project -> Model -> SecurityGroupForm.Msg -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
updateUnderlyingForm sharedModel project model securityGroupFormMsg =
    let
        ( newSecurityGroupForm, securityGroupFormCmd, securityGroupFormSharedMsg ) =
            let
                securityGroupForm =
                    case model.securityGroupForm of
                        Just securityGroupForm_ ->
                            securityGroupForm_

                        Nothing ->
                            -- If we get a message without a form, initialise one.
                            SecurityGroupForm.init { name = newSecurityGroupName project model.serverUuid }
            in
            SecurityGroupForm.update securityGroupFormMsg sharedModel project securityGroupForm
    in
    ( { model | securityGroupForm = Just newSecurityGroupForm }
    , Cmd.map SecurityGroupFormMsg securityGroupFormCmd
    , securityGroupFormSharedMsg
    )


newSecurityGroupName : Project -> OSTypes.ServerUuid -> String
newSecurityGroupName project serverUuid =
    -- Set the security group name to the server name if it exists.
    Maybe.withDefault "" <| GetterSetters.serverNameLookup project serverUuid


appliedSecurityGroupsUuids : Project -> OSTypes.ServerUuid -> Set.Set OSTypes.SecurityGroupUuid
appliedSecurityGroupsUuids project serverUuid =
    let
        serverSecurityGroupsRdpp =
            GetterSetters.getServerSecurityGroups project serverUuid

        serverSecurityGroups =
            case serverSecurityGroupsRdpp.data of
                RDPP.DoHave serverSecurityGroups_ _ ->
                    Just serverSecurityGroups_

                _ ->
                    Nothing
    in
    serverSecurityGroupUuids (Maybe.withDefault [] serverSecurityGroups)


serverSecurityGroupUuids : List OSTypes.ServerSecurityGroup -> Set.Set OSTypes.SecurityGroupUuid
serverSecurityGroupUuids serverSecurityGroups =
    serverSecurityGroups
        |> List.map .uuid
        |> Set.fromList


isSecurityGroupApplied : Project -> OSTypes.ServerUuid -> OSTypes.SecurityGroupUuid -> Bool
isSecurityGroupApplied project serverUuid securityGroupUuid =
    Set.member securityGroupUuid (appliedSecurityGroupsUuids project serverUuid)


isSecurityGroupSelected : Model -> OSTypes.SecurityGroupUuid -> Bool
isSecurityGroupSelected model securityGroupUuid =
    case model.selectedSecurityGroups of
        Ready selectedSecurityGroups ->
            Set.member securityGroupUuid selectedSecurityGroups

        Uninitialised ->
            False


securityGroupRow : View.Types.Context -> Project -> Model -> OSTypes.SecurityGroup -> Element.Element Msg
securityGroupRow context project model securityGroup =
    let
        securityGroupUuid =
            securityGroup.uuid

        securityGroupName =
            VH.extendedResourceName
                (Just securityGroup.name)
                securityGroupUuid
                context.localization.securityGroup

        securityGroupTextButton msg =
            Element.el
                (Text.typographyAttrs Text.Emphasized
                    ++ [ Font.color <| SH.toElementColor <| context.palette.primary
                       , Element.pointer
                       , Events.onClick msg
                       , Element.width Element.fill
                       ]
                )
                (Element.text <| securityGroupName)

        preset =
            if securityGroupTaggedAs securityGroupExoTags.preset securityGroup then
                tagPositive context.palette "preset"

            else
                Element.none

        isDefault =
            isDefaultSecurityGroup context project securityGroup

        default =
            if isDefault then
                tagNeutral context.palette "default"

            else
                Element.none

        isBeingEdited =
            model.securityGroupForm
                |> Maybe.map (\form -> form.uuid == Just securityGroupUuid)
                |> Maybe.withDefault False

        editing =
            if isBeingEdited then
                tagInfo context.palette "editing"

            else
                Element.none

        tags =
            [ editing
            , preset
            , default
            ]

        edit msg =
            [ clickableIcon []
                { icon = edit2
                , accessibilityLabel = "edit " ++ securityGroupName
                , onClick = msg
                , color = context.palette.neutral.icon |> SH.toElementColor
                , hoverColor = context.palette.neutral.text.default |> SH.toElementColor
                }
            ]

        tooltip =
            [ Style.Widgets.ToggleTip.toggleTip
                context
                (SharedMsg << SharedMsg.TogglePopover)
                (Helpers.String.hyphenate
                    [ "securityGroupRulesId"
                    , project.auth.project.uuid
                    , securityGroup.uuid
                    ]
                )
                (SecurityGroupRulesTable.view context project securityGroupUuid)
                ST.PositionRight
            ]

        selected =
            isSecurityGroupSelected model securityGroupUuid

        applied =
            isSecurityGroupApplied project model.serverUuid securityGroupUuid

        highlight =
            case ( selected, applied ) of
                ( True, False ) ->
                    [ Background.color <| SH.toElementColorWithOpacity context.palette.success.textOnNeutralBG 0.1 ]

                ( False, True ) ->
                    [ Background.color <| SH.toElementColorWithOpacity context.palette.danger.textOnNeutralBG 0.1 ]

                _ ->
                    []

        rowStyle =
            [ Element.padding spacer.px16
            , Element.spacing spacer.px16
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <|
                SH.toElementColor context.palette.neutral.border
            , Element.width Element.fill
            ]
                ++ highlight
    in
    Element.column
        (listItemColumnAttribs context.palette ++ rowStyle)
        [ Element.row
            [ Element.spacing spacer.px16, Element.width Element.fill ]
            [ let
                selectWord =
                    if selected then
                        "Deselect"

                    else
                        "Select"
              in
              Input.checkbox
                [ Element.width Element.shrink ]
                { onChange = always (ToggleSelectedGroup securityGroupUuid)
                , icon = Input.defaultCheckbox
                , checked = selected
                , label = Input.labelHidden (selectWord ++ " " ++ securityGroupName)
                }
            , Element.column [ Element.spacing spacer.px12, Element.width Element.fill ]
                [ securityGroupTextButton (ToggleSelectedGroup securityGroupUuid)
                ]
            , Element.row [ Element.spacing spacer.px4, Element.alignRight, Element.alignTop ]
                (tags
                    ++ edit
                        (if isDefault || isBeingEdited then
                            -- You cannot edit the default security group.
                            Nothing

                         else
                            Just <| GotEditSecurityGroupForm securityGroupUuid
                        )
                    ++ tooltip
                )
            ]
        ]


renderList : List (Element.Attribute msg) -> { empty : Maybe (Element.Element msg) } -> (a -> Element.Element msg) -> List a -> Element.Element msg
renderList attrs { empty } rowForItem list =
    Element.column
        attrs
        (case List.length list of
            0 ->
                case empty of
                    Just emptyElement ->
                        [ emptyElement ]

                    Nothing ->
                        [ Element.none ]

            _ ->
                List.map rowForItem list
        )


renderSelectableSecurityGroupsList : View.Types.Context -> Project -> Model -> List OSTypes.SecurityGroup -> Element.Element Msg
renderSelectableSecurityGroupsList context project model securityGroups =
    renderList
        [ Element.alignTop
        , Element.width Element.shrink
        , Border.width 1
        , Border.color <| SH.toElementColor context.palette.neutral.border
        , Border.rounded 4
        , Background.color <| SH.toElementColor context.palette.neutral.background.frontLayer
        ]
        { empty = Just <| Element.text "(none)" }
        (securityGroupRow context project model)
        (sortedSecurityGroups securityGroups)


renderSecurityGroupListAndRules : View.Types.Context -> Project -> Time.Posix -> Model -> List OSTypes.SecurityGroup -> Element.Element Msg
renderSecurityGroupListAndRules context project currentTime model securityGroups =
    Element.wrappedRow [ Element.spacing spacer.px24 ]
        [ renderSelectableSecurityGroupsList context project model securityGroups
        , Element.column [ Element.alignTop, Element.spacing spacer.px24, Element.width Element.fill ]
            [ VH.tile
                context
                [ Element.text
                    (String.join " "
                        [ "Consolidated"
                        , context.localization.securityGroup
                            |> Helpers.String.toTitleCase
                        ]
                    )
                ]
                [ let
                    appliedSecurityGroupUuidsSet =
                        appliedSecurityGroupsUuids project model.serverUuid

                    appliedSecurityGroups : List OSTypes.SecurityGroup
                    appliedSecurityGroups =
                        securityGroups |> List.filter (\securityGroup -> Set.member securityGroup.uuid appliedSecurityGroupUuidsSet)

                    selectedSecurityGroups : List OSTypes.SecurityGroup
                    selectedSecurityGroups =
                        securityGroups
                            |> List.filter (\securityGroup -> isSecurityGroupSelected model securityGroup.uuid)

                    ( newRules, nextGroups ) =
                        case model.securityGroupForm of
                            -- With an active form, we have WIP rules.
                            Just securityGroupForm ->
                                case securityGroupForm.uuid of
                                    -- An existing security group being edited.
                                    Just uuid ->
                                        if isSecurityGroupSelected model uuid then
                                            -- If the security group is selected, use the rules from the form.
                                            ( securityGroupForm.rules
                                            , selectedSecurityGroups |> List.filter (\sg -> sg.uuid /= uuid)
                                            )

                                        else
                                            -- If not, the edits don't matter.
                                            ( []
                                            , selectedSecurityGroups
                                            )

                                    -- This is a new security group, which will be applied when created.
                                    Nothing ->
                                        ( securityGroupForm.rules
                                        , selectedSecurityGroups
                                        )

                            Nothing ->
                                ( []
                                , selectedSecurityGroups
                                )

                    customiser : SecurityGroupRulesTable.RulesTableRowCustomiser Msg
                    customiser rule =
                        let
                            selectedRules =
                                List.concatMap .rules nextGroups
                                    |> List.append newRules
                                    |> uniqueBy matchRule

                            selected =
                                selectedRules
                                    |> List.any (\r -> matchRule r rule)

                            applied =
                                List.concatMap .rules appliedSecurityGroups
                                    |> uniqueBy matchRule
                                    |> List.any (\r -> matchRule r rule)

                            editing =
                                case model.securityGroupForm of
                                    Just securityGroupForm ->
                                        case securityGroupForm.uuid of
                                            Just uuid ->
                                                -- This is an existing security group.
                                                -- If the rule belongs to a group that is selected or already applied, show the rule as being edited.
                                                (appliedSecurityGroups
                                                    ++ selectedSecurityGroups
                                                )
                                                    |> List.Extra.find (\sg -> sg.uuid == uuid)
                                                    |> Maybe.map (\sg -> List.any (\r -> matchRule r rule) sg.rules)
                                                    |> Maybe.withDefault False

                                            Nothing ->
                                                -- This is a new security group.
                                                -- If a rule is in the form, show it as being edited since it will be applied later.
                                                securityGroupForm.rules
                                                    |> List.any (\r -> matchRule r rule)

                                    Nothing ->
                                        False

                            highlight =
                                case ( selected, applied ) of
                                    ( True, False ) ->
                                        [ Background.color <| SH.toElementColorWithOpacity context.palette.success.textOnNeutralBG 0.1 ]

                                    ( False, True ) ->
                                        [ Background.color <| SH.toElementColorWithOpacity context.palette.danger.textOnNeutralBG 0.1 ]

                                    _ ->
                                        []

                            shadowed =
                                if isRuleShadowed rule selectedRules then
                                    [ Font.color <| SH.toElementColorWithOpacity context.palette.neutral.text.default 0.25 ]

                                else
                                    []

                            icon =
                                case ( selected, applied ) of
                                    ( True, False ) ->
                                        Just <| Style.Widgets.Icon.sizedFeatherIcon 16 FeatherIcons.plus

                                    ( False, True ) ->
                                        Just <| Style.Widgets.Icon.sizedFeatherIcon 16 FeatherIcons.minus

                                    _ ->
                                        -- Chosen for consistent spacing when no icon is present.
                                        Just <| Element.el [ Element.width <| Element.px 18 ] (Element.text "")

                            editingTag =
                                if editing then
                                    Just <| tagInfo context.palette "editing"

                                else
                                    -- Chosen for consistent spacing when no tag is present.
                                    Just <| Element.el [ Element.width <| Element.px 18 ] (Element.text "")
                        in
                        { leftElementForRow = icon
                        , rightElementForRow = editingTag
                        , styleForRow = SecurityGroupRulesTable.defaultRowStyle ++ highlight ++ shadowed ++ [ Element.height Element.fill ]
                        }

                    rules =
                        List.concatMap .rules (appliedSecurityGroups ++ nextGroups)
                            |> List.append newRules
                            |> uniqueBy matchRule
                  in
                  SecurityGroupRulesTable.rulesTableWithRowCustomiser
                    context
                    (GetterSetters.projectIdentifier project)
                    { rules = rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }
                    customiser
                ]
            , case model.securityGroupForm of
                Just securityGroupForm ->
                    let
                        isExistingSecurityGroup =
                            securityGroupForm.uuid /= Nothing
                    in
                    VH.tile
                        context
                        [ Element.text
                            (String.join " "
                                [ if isExistingSecurityGroup then
                                    "Edit"

                                  else
                                    "New"
                                , context.localization.securityGroup
                                    |> Helpers.String.toTitleCase
                                ]
                            )
                        ]
                        [ Element.column
                            [ Element.spacing spacer.px16, Element.width Element.fill ]
                            [ SecurityGroupForm.view
                                context
                                project
                                currentTime
                                securityGroupForm
                                (Just model.serverUuid)
                                |> Element.map SecurityGroupFormMsg
                            ]
                        , if isExistingSecurityGroup then
                            let
                                selected =
                                    securityGroupForm.uuid
                                        |> Maybe.map (\uuid -> isSecurityGroupSelected model uuid)
                                        |> Maybe.withDefault False

                                applied =
                                    securityGroupForm.uuid
                                        |> Maybe.map (isSecurityGroupApplied project model.serverUuid)
                                        |> Maybe.withDefault False

                                securityGroupWord =
                                    context.localization.securityGroup

                                serverWord =
                                    context.localization.virtualComputer

                                positioning =
                                    Element.el
                                        [ Element.width Element.shrink, Element.centerX, Element.paddingEach { top = spacer.px12, bottom = 0, left = 0, right = 0 } ]
                            in
                            case ( selected, applied ) of
                                ( False, _ ) ->
                                    positioning <| Validation.warningText context.palette <| "This " ++ securityGroupWord ++ " isn't currently selected for your " ++ serverWord ++ "."

                                ( _, False ) ->
                                    positioning <| Validation.warningText context.palette <| "This " ++ securityGroupWord ++ " isn't yet applied to your " ++ serverWord ++ "."

                                _ ->
                                    Element.none

                          else
                            Element.none
                        ]

                Nothing ->
                    VH.tile
                        context
                        []
                        [ Element.row
                            [ Element.spaceEvenly, Element.spacing spacer.px12, Element.width Element.fill ]
                            [ Text.p [] [ Text.body <| "You can create a " ++ context.localization.securityGroup ++ " for this " ++ context.localization.virtualComputer ++ " to capture additional rules." ]
                            , Button.button Button.Secondary
                                context.palette
                                { text =
                                    String.join " "
                                        [ "New"
                                        , context.localization.securityGroup
                                            |> Helpers.String.toTitleCase
                                        ]
                                , onPress = Just GotCreateSecurityGroupForm
                                }
                            ]
                        ]
            ]
        ]


renderSecurityGroupsList : View.Types.Context -> Project -> Time.Posix -> Model -> Server -> List OSTypes.SecurityGroup -> Element.Element Msg
renderSecurityGroupsList context project currentTime model server securityGroups =
    let
        serverSecurityGroups =
            GetterSetters.getServerSecurityGroups project server.osProps.uuid
    in
    VH.renderRDPP context
        serverSecurityGroups
        (context.localization.securityGroup
            |> Helpers.String.pluralize
        )
        (always <| renderSecurityGroupListAndRules context project currentTime model securityGroups)


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    VH.renderRDPP context
        project.servers
        context.localization.virtualComputer
        (\_ ->
            case GetterSetters.serverLookup project model.serverUuid of
                Just server ->
                    render context project currentTime model server

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.virtualComputer
                            , "found"
                            ]
        )


render : View.Types.Context -> Project -> Time.Posix -> Model -> Server -> Element.Element Msg
render context project currentTime model server =
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.wrappedRow (Text.headingStyleAttrs context.palette)
            [ FeatherIcons.shield |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Text.text Text.ExtraLarge
                []
                (String.join " "
                    [ context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    , context.localization.securityGroup
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "for"
                    , VH.resourceName (Just server.osProps.name) server.osProps.uuid
                    ]
                )
            , Element.row [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                [ Button.default
                    context.palette
                    { text = "Return to " ++ VH.resourceName (Just server.osProps.name) server.osProps.uuid
                    , onPress = Just GotDone
                    }
                , Button.primary
                    context.palette
                    { text = "Apply Changes"
                    , onPress =
                        let
                            serverSecurityGroupUpdates : List OSTypes.ServerSecurityGroupUpdate
                            serverSecurityGroupUpdates =
                                let
                                    updateIfNeeded : OSTypes.SecurityGroup -> Maybe OSTypes.ServerSecurityGroupUpdate
                                    updateIfNeeded securityGroup =
                                        let
                                            applied =
                                                isSecurityGroupApplied project model.serverUuid securityGroup.uuid

                                            selected =
                                                case model.selectedSecurityGroups of
                                                    Ready selectedSecurityGroups ->
                                                        Set.member securityGroup.uuid selectedSecurityGroups

                                                    Uninitialised ->
                                                        False

                                            serverSecurityGroup =
                                                { uuid = securityGroup.uuid, name = securityGroup.name }
                                        in
                                        case ( applied, selected ) of
                                            ( False, True ) ->
                                                Just (OSTypes.AddServerSecurityGroup serverSecurityGroup)

                                            ( True, False ) ->
                                                Just (OSTypes.RemoveServerSecurityGroup serverSecurityGroup)

                                            _ ->
                                                Nothing
                                in
                                List.filterMap updateIfNeeded
                                    (project.securityGroups |> RDPP.withDefault [])
                        in
                        case serverSecurityGroupUpdates of
                            [] ->
                                Nothing

                            _ ->
                                Just (GotApplyServerSecurityGroupUpdates serverSecurityGroupUpdates)
                    }
                ]
            ]
        , VH.renderRDPP
            context
            project.securityGroups
            (Helpers.String.pluralize context.localization.securityGroup)
            (renderSecurityGroupsList context project currentTime model server)
        ]
