module Page.ServerSecurityGroups exposing (Model, Msg(..), init, update, view)

import Array
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters exposing (isDefaultSecurityGroup)
import Helpers.List exposing (uniqueBy)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String
import OpenStack.SecurityGroupRule exposing (matchRule)
import OpenStack.Types as OSTypes exposing (securityGroupExoTags, securityGroupTaggedAs)
import Page.SecurityGroupRulesTable as SecurityGroupRulesTable
import Route
import Set
import Set.Extra
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.Card
import Style.Widgets.DataList as DataList exposing (borderStyleForRow, defaultRowStyle)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Tag exposing (tagNeutral, tagPositive)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , dataListModel : DataList.Model
    , selectedSecurityGroups : Maybe.Maybe (Set.Set OSTypes.SecurityGroupUuid)
    }


type Msg
    = DataListMsg DataList.Msg
    | GotApplyServerSecurityGroupUpdates (List OSTypes.ServerSecurityGroupUpdate)
    | GotServerSecurityGroups OSTypes.ServerUuid (List OSTypes.ServerSecurityGroup)
    | ToggleSelectedGroup OSTypes.SecurityGroupUuid
    | SharedMsg SharedMsg.SharedMsg


init : Project -> OSTypes.ServerUuid -> Model
init project serverUuid =
    { serverUuid = serverUuid
    , dataListModel = DataList.init <| DataList.getDefaultFilterOptions []
    , selectedSecurityGroups =
        case GetterSetters.serverLookup project serverUuid of
            Just server ->
                case server.securityGroups.data of
                    RDPP.DoHave serverSecurityGroups _ ->
                        case project.securityGroups.data of
                            RDPP.DoHave securityGroups _ ->
                                List.map .uuid serverSecurityGroups
                                    |> Set.fromList
                                    |> Set.intersect (Set.fromList <| List.map .uuid securityGroups)
                                    |> Just

                            _ ->
                                Nothing

                    _ ->
                        Nothing

            _ ->
                Nothing
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        DataListMsg dataListMsg ->
            ( { model | dataListModel = DataList.update dataListMsg model.dataListModel }, Cmd.none, SharedMsg.NoOp )

        GotApplyServerSecurityGroupUpdates serverSecurityGroupUpdates ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg model.serverUuid <|
                    SharedMsg.RequestServerSecurityGroupUpdates serverSecurityGroupUpdates
            )

        GotServerSecurityGroups serverUuid serverSecurityGroups ->
            -- SharedModel just updated with new security groups for this server. If we don't have anything selected yet, show those already applied.
            if serverUuid == model.serverUuid && model.selectedSecurityGroups == Nothing then
                ( { model | selectedSecurityGroups = Just <| appliedSecurityGroupUuids serverSecurityGroups }, Cmd.none, SharedMsg.NoOp )

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


type alias SecurityGroupRecord =
    DataList.DataRecord
        { securityGroup : OSTypes.SecurityGroup
        , applied : Bool
        }


securityGroupRecords : List OSTypes.SecurityGroup -> List OSTypes.ServerSecurityGroup -> List SecurityGroupRecord
securityGroupRecords securityGroups serverSecurityGroups =
    List.map
        (\securityGroup ->
            { id = securityGroup.uuid
            , selectable = False -- DataList doesn't render checkboxes without bulk actions.
            , securityGroup = securityGroup
            , applied = appliedSecurityGroupUuids serverSecurityGroups |> Set.member securityGroup.uuid
            }
        )
        securityGroups


appliedSecurityGroupUuids : List OSTypes.ServerSecurityGroup -> Set.Set OSTypes.SecurityGroupUuid
appliedSecurityGroupUuids serverSecurityGroups =
    serverSecurityGroups
        |> List.map .uuid
        |> Set.fromList


isSecurityGroupSelected : Model -> OSTypes.SecurityGroupUuid -> Bool
isSecurityGroupSelected model securityGroupUuid =
    Set.member securityGroupUuid (Maybe.withDefault Set.empty model.selectedSecurityGroups)


securityGroupView : View.Types.Context -> Project -> Model -> List OSTypes.ServerSecurityGroup -> SecurityGroupRecord -> Element.Element Msg
securityGroupView context project model serverSecurityGroups securityGroupRecord =
    let
        securityGroup =
            securityGroupRecord.securityGroup

        securityGroupUuid =
            securityGroup.uuid

        securityGroupName =
            VH.extendedResourceName
                (Just securityGroup.name)
                securityGroupUuid
                context.localization.securityGroup

        securityGroupLink =
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.SecurityGroupDetail securityGroupUuid
                        )
                , label =
                    Element.el
                        (Text.typographyAttrs Text.Emphasized
                            ++ [ Font.color (SH.toElementColor context.palette.primary)
                               ]
                        )
                        (Element.text <| securityGroupName)
                }

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
                ST.PositionLeft
            ]
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row
            [ Element.spacing spacer.px16, Element.width Element.fill ]
            [ let
                selected =
                    isSecurityGroupSelected model securityGroupUuid

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
                [ securityGroupLink
                ]
            , Element.row [ Element.spacing spacer.px4, Element.alignRight, Element.alignTop ]
                (tags ++ tooltip)
            ]
        ]


renderSelectableSecurityGroupsList : View.Types.Context -> Project -> Model -> List OSTypes.SecurityGroup -> List OSTypes.ServerSecurityGroup -> Element.Element Msg
renderSelectableSecurityGroupsList context project model securityGroups serverSecurityGroups =
    DataList.viewWithCustomRowStyle
        (\filteredData i ->
            let
                securityGroupRecord =
                    Array.get i (Array.fromList filteredData)

                selected =
                    case securityGroupRecord of
                        Just record ->
                            isSecurityGroupSelected model record.securityGroup.uuid

                        Nothing ->
                            False

                applied =
                    case securityGroupRecord of
                        Just record ->
                            record.applied

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

                rowStyle =
                    defaultRowStyle context.palette
                        ++ [ Element.padding spacer.px16
                           , Element.spacing spacer.px16
                           ]
                        ++ highlight
            in
            borderStyleForRow rowStyle (List.length filteredData) i
        )
        context.localization.securityGroup
        model.dataListModel
        DataListMsg
        context
        [ Element.alignTop ]
        (securityGroupView context project model serverSecurityGroups)
        (securityGroupRecords securityGroups serverSecurityGroups)
        []
        Nothing
        Nothing


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

        appliedSercurityGroupUuids_ =
            appliedSecurityGroupUuids serverSecurityGroups

        appliedSecurityGroups : List OSTypes.SecurityGroup
        appliedSecurityGroups =
            securityGroups |> List.filter (\securityGroup -> Set.member securityGroup.uuid appliedSercurityGroupUuids_)

        selectedSecurityGroups : List OSTypes.SecurityGroup
        selectedSecurityGroups =
            securityGroups
                |> List.filter (\securityGroup -> isSecurityGroupSelected model securityGroup.uuid)

        appliedRules =
            List.concatMap .rules appliedSecurityGroups |> uniqueBy matchRule

        selectedRules =
            List.concatMap .rules selectedSecurityGroups |> uniqueBy matchRule

        rules =
            List.concatMap .rules (appliedSecurityGroups ++ selectedSecurityGroups) |> uniqueBy matchRule
    in
    Element.wrappedRow [ Element.spacing spacer.px24 ]
        [ renderSelectableSecurityGroupsList context project model securityGroups serverSecurityGroups
        , tile
            [ Element.text
                (String.join " "
                    [ "Consolidated"
                    , context.localization.securityGroup
                        |> Helpers.String.toTitleCase
                    ]
                )
            ]
            [ SecurityGroupRulesTable.rulesTableWithRowStyle
                context
                (GetterSetters.projectIdentifier project)
                { rules = rules, securityGroupForUuid = GetterSetters.securityGroupLookup project }
                (\rule ->
                    let
                        selected =
                            selectedRules |> List.any (\r -> matchRule r rule)

                        applied =
                            appliedRules |> List.any (\r -> matchRule r rule)

                        highlight =
                            case ( selected, applied ) of
                                ( True, False ) ->
                                    [ Background.color <| SH.toElementColorWithOpacity context.palette.success.textOnNeutralBG 0.1 ]

                                ( False, True ) ->
                                    [ Background.color <| SH.toElementColorWithOpacity context.palette.danger.textOnNeutralBG 0.1 ]

                                _ ->
                                    []
                    in
                    SecurityGroupRulesTable.defaultRowStyle ++ highlight
                )
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
                [ Button.primary
                    context.palette
                    { text = "Apply Changes"
                    , onPress =
                        let
                            serverSecurityGroupUpdates : List OSTypes.ServerSecurityGroupUpdate
                            serverSecurityGroupUpdates =
                                List.filterMap
                                    (\securityGroup ->
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
                                    )
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
