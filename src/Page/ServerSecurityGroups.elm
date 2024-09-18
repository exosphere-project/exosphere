module Page.ServerSecurityGroups exposing (Model, Msg(..), init, update, view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters exposing (isDefaultSecurityGroup)
import Helpers.List exposing (uniqueBy)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String
import OpenStack.SecurityGroupRule exposing (isRuleShadowed, matchRule)
import OpenStack.Types as OSTypes exposing (securityGroupExoTags, securityGroupTaggedAs)
import Page.SecurityGroupRulesTable as SecurityGroupRulesTable
import Route
import Set
import Set.Extra
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.Card
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag exposing (tagNeutral, tagPositive)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , securityGroups : List OSTypes.SecurityGroup
    , appliedSecurityGroups : Set.Set OSTypes.SecurityGroupUuid
    , selectedSecurityGroups : Maybe.Maybe (Set.Set OSTypes.SecurityGroupUuid)
    }


type Msg
    = GotApplyServerSecurityGroupUpdates (List OSTypes.ServerSecurityGroupUpdate)
    | GotDone
    | GotServerSecurityGroups OSTypes.ServerUuid (List OSTypes.ServerSecurityGroup)
    | ToggleSelectedGroup OSTypes.SecurityGroupUuid
    | SharedMsg SharedMsg.SharedMsg


init : Project -> OSTypes.ServerUuid -> Model
init project serverUuid =
    let
        maybeServer =
            GetterSetters.serverLookup project serverUuid

        serverSecurityGroups =
            case maybeServer of
                Just server ->
                    case server.securityGroups.data of
                        RDPP.DoHave serverSecurityGroups_ _ ->
                            Just serverSecurityGroups_

                        _ ->
                            Nothing

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
    , securityGroups = Maybe.withDefault [] maybeProjectSecurityGroups
    , appliedSecurityGroups = appliedSecurityGroupUuids (Maybe.withDefault [] serverSecurityGroups)
    , selectedSecurityGroups =
        case ( serverSecurityGroups, maybeProjectSecurityGroups ) of
            ( Just serverSecurityGroups_, Just projectSecurityGroups_ ) ->
                List.map .uuid serverSecurityGroups_
                    |> Set.fromList
                    |> Set.intersect (Set.fromList <| List.map .uuid projectSecurityGroups_)
                    |> Just

            _ ->
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

        GotServerSecurityGroups serverUuid serverSecurityGroups ->
            -- SharedModel just updated with new security groups for this server.
            if serverUuid == model.serverUuid then
                let
                    appliedSecurityGroups =
                        appliedSecurityGroupUuids serverSecurityGroups

                    selectedSecurityGroups =
                        -- If we don't have anything selected yet, show those already applied.
                        if model.selectedSecurityGroups == Nothing then
                            Just appliedSecurityGroups

                        else
                            model.selectedSecurityGroups
                in
                ( { model | appliedSecurityGroups = appliedSecurityGroups, selectedSecurityGroups = selectedSecurityGroups }, Cmd.none, SharedMsg.NoOp )

            else
                ( model, Cmd.none, SharedMsg.NoOp )

        ToggleSelectedGroup securityGroupUuid ->
            case model.selectedSecurityGroups of
                Just selectedSecurityGroups ->
                    let
                        newSelectedSecurityGroups =
                            Set.Extra.toggle securityGroupUuid selectedSecurityGroups
                    in
                    ( { model | selectedSecurityGroups = Just newSelectedSecurityGroups }, Cmd.none, SharedMsg.NoOp )

                Nothing ->
                    -- We should never be here, because this is already a `Just` when there are security groups to select.
                    ( model, Cmd.none, SharedMsg.NoOp )


appliedSecurityGroupUuids : List OSTypes.ServerSecurityGroup -> Set.Set OSTypes.SecurityGroupUuid
appliedSecurityGroupUuids serverSecurityGroups =
    serverSecurityGroups
        |> List.map .uuid
        |> Set.fromList


isSecurityGroupApplied : Model -> OSTypes.SecurityGroupUuid -> Bool
isSecurityGroupApplied model securityGroupUuid =
    Set.member securityGroupUuid model.appliedSecurityGroups


isSecurityGroupSelected : Model -> OSTypes.SecurityGroupUuid -> Bool
isSecurityGroupSelected model securityGroupUuid =
    Set.member securityGroupUuid (Maybe.withDefault Set.empty model.selectedSecurityGroups)


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

        default =
            if isDefaultSecurityGroup context project securityGroup then
                tagNeutral context.palette "default"

            else
                Element.none

        tags =
            [ preset
            , default
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
            isSecurityGroupApplied model securityGroupUuid

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
                (tags ++ tooltip)
            ]
        ]


renderList : List (Element.Attribute Msg) -> (a -> Element.Element Msg) -> List a -> Element.Element Msg
renderList attrs rowForItem list =
    Element.column
        attrs
        (List.map rowForItem list)


renderSelectableSecurityGroupsList : View.Types.Context -> Project -> Model -> List OSTypes.SecurityGroup -> Element.Element Msg
renderSelectableSecurityGroupsList context project model securityGroups =
    renderList
        [ Element.alignTop
        , Element.width Element.fill
        , Border.width 1
        , Border.color <| SH.toElementColor context.palette.neutral.border
        , Border.rounded 4
        , Background.color <| SH.toElementColor context.palette.neutral.background.frontLayer
        ]
        (securityGroupRow context project model)
        securityGroups


renderSecurityGroupListAndRules : View.Types.Context -> Project -> Model -> List OSTypes.SecurityGroup -> List OSTypes.ServerSecurityGroup -> Element.Element Msg
renderSecurityGroupListAndRules context project model securityGroups serverSecurityGroups =
    let
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
    in
    Element.wrappedRow [ Element.spacing spacer.px24 ]
        [ renderSelectableSecurityGroupsList context project model securityGroups
        , tile
            [ Element.text
                (String.join " "
                    [ "Consolidated"
                    , context.localization.securityGroup
                        |> Helpers.String.toTitleCase
                    ]
                )
            ]
            [ let
                appliedSercurityGroupUuids_ =
                    appliedSecurityGroupUuids serverSecurityGroups

                appliedSecurityGroups : List OSTypes.SecurityGroup
                appliedSecurityGroups =
                    securityGroups |> List.filter (\securityGroup -> Set.member securityGroup.uuid appliedSercurityGroupUuids_)

                selectedSecurityGroups : List OSTypes.SecurityGroup
                selectedSecurityGroups =
                    securityGroups
                        |> List.filter (\securityGroup -> isSecurityGroupSelected model securityGroup.uuid)

                rowStyleForRule : OpenStack.SecurityGroupRule.SecurityGroupRule -> List (Element.Attribute msg)
                rowStyleForRule rule =
                    let
                        selectedRules =
                            List.concatMap .rules selectedSecurityGroups |> uniqueBy matchRule

                        selected =
                            selectedRules
                                |> List.any (\r -> matchRule r rule)

                        applied =
                            List.concatMap .rules appliedSecurityGroups
                                |> uniqueBy matchRule
                                |> List.any (\r -> matchRule r rule)

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
                    in
                    SecurityGroupRulesTable.defaultRowStyle ++ highlight ++ shadowed

                rules =
                    List.concatMap .rules (appliedSecurityGroups ++ selectedSecurityGroups) |> uniqueBy matchRule
              in
              SecurityGroupRulesTable.rulesTableWithRowStyle
                context
                (GetterSetters.projectIdentifier project)
                { rules = rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }
                rowStyleForRule
            ]
        ]


renderSecurityGroupsList : View.Types.Context -> Project -> Model -> Server -> List OSTypes.SecurityGroup -> Element.Element Msg
renderSecurityGroupsList context project model server securityGroups =
    VH.renderRDPP context
        server.securityGroups
        (context.localization.securityGroup
            |> Helpers.String.pluralize
        )
        (renderSecurityGroupListAndRules context project model securityGroups)


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    VH.renderRDPP context
        project.servers
        context.localization.virtualComputer
        (\_ ->
            case GetterSetters.serverLookup project model.serverUuid of
                Just server ->
                    render context project model server

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.virtualComputer
                            , "found"
                            ]
        )


render : View.Types.Context -> Project -> Model -> Server -> Element.Element Msg
render context project model server =
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.row (Text.headingStyleAttrs context.palette)
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
                                                Set.member securityGroup.uuid (server.securityGroups |> RDPP.withDefault [] |> List.map .uuid |> Set.fromList)

                                            selected =
                                                Set.member securityGroup.uuid (Maybe.withDefault Set.empty model.selectedSecurityGroups)

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
            (renderSecurityGroupsList context project model server)
        ]
