module View.View exposing (view)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import FeatherIcons as Icons
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Html
import Html.Attributes
import Page.FloatingIpAssign
import Page.FloatingIpCreate
import Page.FloatingIpList
import Page.GetSupport
import Page.HelpAbout
import Page.Home
import Page.ImageList
import Page.InstanceSourcePicker
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginOpenIdConnect
import Page.LoginOpenstack
import Page.LoginPicker
import Page.MessageLog
import Page.ProjectOverview
import Page.SecurityGroupDetail
import Page.SecurityGroupList
import Page.SelectProjectRegions
import Page.SelectProjects
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.ServerResize
import Page.Settings
import Page.ShareCreate
import Page.ShareDetail
import Page.ShareList
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Page.VolumeMountInstructions
import Route
import Style.Helpers as SH exposing (shadowDefaults)
import Style.Types as ST
import Style.Widgets.DeleteButton
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Popover.Popover exposing (dropdownItemStyle, popover)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text exposing (FontFamily(..), TextVariant(..))
import Style.Widgets.Toast as Toast
import Toasty
import Types.Error exposing (AppError)
import Types.HelperTypes exposing (WindowSize)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg exposing (SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Breadcrumb
import View.Helpers as VH
import View.Nav
import View.PageTitle
import View.Types
import Widget


view : Result AppError OuterModel -> Browser.Document OuterMsg
view resultModel =
    case resultModel of
        Err appError ->
            viewInvalid appError

        Ok model ->
            viewValid model


viewValid : OuterModel -> Browser.Document OuterMsg
viewValid outerModel =
    { title =
        View.PageTitle.pageTitle outerModel
    , body =
        [ viewWrapper outerModel ]
    }


viewInvalid : AppError -> Browser.Document OuterMsg
viewInvalid appError =
    { title = "Error"
    , body = [ Text.body appError.error |> Element.layout [] ]
    }


viewWrapper : OuterModel -> Html.Html OuterMsg
viewWrapper outerModel =
    let
        { viewContext } =
            outerModel.sharedModel
    in
    Element.layout
        [ Text.fontSize Body
        , Text.fontFamily Default
        , Font.color <| SH.toElementColor <| viewContext.palette.neutral.text.default
        , Background.color <| SH.toElementColor viewContext.palette.neutral.background.backLayer
        ]
        (appView viewContext.windowSize outerModel viewContext)


appView : WindowSize -> OuterModel -> View.Types.Context -> Element.Element OuterMsg
appView windowSize outerModel context =
    let
        ( header, content ) =
            case outerModel.viewState of
                NonProjectView viewConstructor ->
                    nonProjectViews outerModel.sharedModel context viewConstructor

                ProjectView projectName viewConstructor ->
                    case GetterSetters.projectLookup outerModel.sharedModel projectName of
                        Nothing ->
                            ( Nothing
                            , Text.body <|
                                String.join " "
                                    [ "Oops!"
                                    , context.localization.unitOfTenancy
                                        |> Helpers.String.toTitleCase
                                    , "not found"
                                    ]
                            )

                        Just project ->
                            ( Just <| projectHeaderView context project
                            , projectContentView
                                outerModel.sharedModel
                                context
                                project
                                viewConstructor
                            )

        contentContainerAttrs =
            [ Element.padding spacer.px24
            , Element.width Element.fill
            ]

        mainContainerView =
            Element.column
                [ Element.alignTop
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbars
                , Element.spacing spacer.px8
                ]
                [ case header of
                    Just header_ ->
                        let
                            headerContainerAttrs =
                                [ Background.color <|
                                    SH.toElementColor context.palette.neutral.background.frontLayer
                                , Border.widthXY 0 1
                                , Border.color <|
                                    SH.toElementColor context.palette.neutral.border
                                , Element.width Element.fill
                                , Element.paddingEach
                                    { top = spacer.px12
                                    , right = spacer.px24
                                    , bottom = spacer.px16
                                    , left = spacer.px24
                                    }
                                , Element.spacing spacer.px12
                                ]
                        in
                        Element.column
                            headerContainerAttrs
                            [ View.Breadcrumb.breadcrumb outerModel context
                                |> Element.map SharedMsg
                            , header_
                            ]

                    Nothing ->
                        Element.none
                , Element.column contentContainerAttrs
                    [ content
                    , Element.html
                        (Toasty.view Toast.config
                            (Toast.view context outerModel.sharedModel)
                            (\m -> SharedMsg <| ToastMsg m)
                            outerModel.sharedModel.toasties
                        )
                    ]
                ]
    in
    Element.column
        [ Element.padding 0
        , Element.spacing 0
        , Element.width Element.fill
        , Element.height <|
            Element.px windowSize.height
        ]
        [ Element.el
            [ Border.shadow shadowDefaults
            , Element.width Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "z-index" "1"
            ]
            (View.Nav.navBar outerModel context)
        , Element.el
            [ Element.padding 0
            , Element.spacing 0
            , Element.width Element.fill
            , Element.height <|
                Element.px (windowSize.height - View.Nav.navBarHeight)
            ]
            mainContainerView
        ]


nonProjectViews :
    SharedModel
    -> View.Types.Context
    -> Types.View.NonProjectViewConstructor
    -> ( Maybe (Element.Element OuterMsg), Element.Element OuterMsg )
nonProjectViews model context viewConstructor =
    case viewConstructor of
        GetSupport pageModel ->
            ( Just <| Page.GetSupport.headerView context model
            , Page.GetSupport.view context model pageModel
                |> Element.map GetSupportMsg
            )

        HelpAbout ->
            ( Just <| Page.HelpAbout.headerView model context
            , Page.HelpAbout.view model context
            )

        Home pageModel ->
            ( Just (Page.Home.headerView context model |> Element.map HomeMsg)
            , Page.Home.view context model pageModel
                |> Element.map HomeMsg
            )

        LoadingUnscopedProjects ->
            ( Nothing
              -- TODO put a fidget spinner here
            , Text.body <|
                String.join " "
                    [ "Loading"
                    , context.localization.unitOfTenancy
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    ]
            )

        Login loginView ->
            case loginView of
                LoginOpenstack pageModel ->
                    ( Just <| Page.LoginOpenstack.headerView context
                    , Page.LoginOpenstack.view context model pageModel
                        |> Element.map LoginOpenstackMsg
                    )

                LoginOpenIdConnect pageModel ->
                    ( Just <| Page.LoginOpenIdConnect.headerView context pageModel
                    , Page.LoginOpenIdConnect.view context model pageModel
                        |> Element.map SharedMsg
                    )

        LoginPicker ->
            ( Just <| Page.LoginPicker.headerView context
            , Page.LoginPicker.view context model
                |> Element.map LoginPickerMsg
            )

        MessageLog pageModel ->
            ( Just <| Page.MessageLog.headerView context
            , Page.MessageLog.view context model pageModel
                |> Element.map MessageLogMsg
            )

        PageNotFound ->
            ( Nothing
            , Text.body "Error: page not found. Perhaps you are trying to reach an invalid URL."
            )

        SelectProjectRegions pageModel ->
            Page.SelectProjectRegions.views context model pageModel
                |> Tuple.mapSecond (Element.map SelectProjectRegionsMsg)

        SelectProjects pageModel ->
            Page.SelectProjects.views context model pageModel
                |> Tuple.mapSecond (Element.map SelectProjectsMsg)

        Settings pageModel ->
            ( Just <| Page.Settings.headerView context
            , Page.Settings.view context model pageModel
                |> Element.map SettingsMsg
            )


projectContentView :
    SharedModel
    -> View.Types.Context
    -> Project
    -> Types.View.ProjectViewConstructor
    -> Element.Element OuterMsg
projectContentView model context p viewConstructor =
    case viewConstructor of
        ProjectOverview pageModel ->
            Page.ProjectOverview.view context p model.clientCurrentTime pageModel
                |> Element.map ProjectOverviewMsg

        FloatingIpAssign pageModel ->
            Page.FloatingIpAssign.view context p pageModel
                |> Element.map FloatingIpAssignMsg

        FloatingIpList pageModel ->
            Page.FloatingIpList.view context p pageModel
                |> Element.map FloatingIpListMsg

        ImageList pageModel ->
            Page.ImageList.view context p pageModel
                |> Element.map ImageListMsg

        InstanceSourcePicker pageModel ->
            Page.InstanceSourcePicker.view context p pageModel
                |> Element.map InstanceSourcePickerMsg

        FloatingIpCreate pageModel ->
            Page.FloatingIpCreate.view context p model.clientCurrentTime pageModel
                |> Element.map FloatingIpCreateMsg

        KeypairCreate pageModel ->
            Page.KeypairCreate.view context p model.clientCurrentTime pageModel
                |> Element.map KeypairCreateMsg

        KeypairList pageModel ->
            Page.KeypairList.view context p pageModel
                |> Element.map KeypairListMsg

        SecurityGroupDetail pageModel ->
            Page.SecurityGroupDetail.view context p ( model.clientCurrentTime, model.timeZone ) pageModel
                |> Element.map SecurityGroupDetailMsg

        SecurityGroupList pageModel ->
            Page.SecurityGroupList.view context p model.clientCurrentTime pageModel
                |> Element.map SecurityGroupListMsg

        ServerCreate pageModel ->
            Page.ServerCreate.view context p model.clientCurrentTime pageModel
                |> Element.map ServerCreateMsg

        ServerCreateImage pageModel ->
            Page.ServerCreateImage.view context pageModel
                |> Element.map ServerCreateImageMsg

        ServerDetail pageModel ->
            Page.ServerDetail.view context p ( model.clientCurrentTime, model.timeZone ) pageModel
                |> Element.map ServerDetailMsg

        ServerList pageModel ->
            Page.ServerList.view context p model.clientCurrentTime pageModel
                |> Element.map ServerListMsg

        ServerResize pageModel ->
            Page.ServerResize.view context p pageModel
                |> Element.map ServerResizeMsg

        ShareCreate pageModel ->
            Page.ShareCreate.view context p model.clientCurrentTime pageModel
                |> Element.map ShareCreateMsg

        ShareDetail pageModel ->
            Page.ShareDetail.view context p ( model.clientCurrentTime, model.timeZone ) pageModel
                |> Element.map ShareDetailMsg

        ShareList pageModel ->
            Page.ShareList.view context p model.clientCurrentTime pageModel
                |> Element.map ShareListMsg

        VolumeAttach pageModel ->
            Page.VolumeAttach.view context p pageModel
                |> Element.map VolumeAttachMsg

        VolumeCreate pageModel ->
            Page.VolumeCreate.view context p model.clientCurrentTime pageModel
                |> Element.map VolumeCreateMsg

        VolumeDetail pageModel ->
            Page.VolumeDetail.view context p pageModel
                |> Element.map VolumeDetailMsg

        VolumeList pageModel ->
            Page.VolumeList.view context p model.clientCurrentTime pageModel
                |> Element.map VolumeListMsg

        VolumeMountInstructions pageModel ->
            Page.VolumeMountInstructions.view context p pageModel
                |> Element.map VolumeMountInstructionsMsg


projectHeaderView : View.Types.Context -> Project -> Element.Element OuterMsg
projectHeaderView context p =
    let
        removeText =
            String.join " "
                [ "Remove"
                , Helpers.String.toTitleCase context.localization.unitOfTenancy
                ]

        deletePopconfirmId =
            String.join " "
                [ "Remove"
                , context.localization.unitOfTenancy
                , p.auth.project.uuid
                , Maybe.withDefault
                    ("without " ++ Helpers.String.indefiniteArticle context.localization.openstackSharingKeystoneWithAnother ++ " " ++ context.localization.openstackSharingKeystoneWithAnother)
                    (Maybe.map (\region -> "in " ++ context.localization.openstackSharingKeystoneWithAnother ++ " " ++ region.id) p.region)
                ]
    in
    Element.row [ Element.width Element.fill, Element.spacing spacer.px12 ]
        [ Text.text Text.Large
            [ Font.regular, Region.heading 2 ]
            (VH.friendlyCloudName
                context
                p
                ++ " - "
                ++ p.auth.project.name
            )
        , Text.p
            [ Text.fontSize Text.Small
            , Element.alpha 0.75
            ]
            [ Element.text "(logged in as "
            , Text.strong p.auth.user.name
            , Element.text ")"
            ]
        , Style.Widgets.DeleteButton.deletePopconfirm context
            (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
            deletePopconfirmId
            { confirmation =
                Element.column
                    [ Element.spacing spacer.px8
                    , Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
                    ]
                    [ Text.body <|
                        "Are you sure you want to remove this "
                            ++ context.localization.unitOfTenancy
                            ++ "?"
                    , Text.text Text.Small [] <|
                        "Nothing will be deleted on the "
                            ++ context.localization.openstackWithOwnKeystone
                            ++ ", only removed from your browser until you log in again"
                    ]
            , buttonText = Just "Remove"
            , onConfirm = Just <| SharedMsg <| SharedMsg.ProjectMsg (GetterSetters.projectIdentifier p) SharedMsg.RemoveProject
            , onCancel = Just <| SharedMsg <| SharedMsg.NoOp
            }
            ST.PositionLeftTop
            (\toggle _ ->
                Widget.iconButton
                    (SH.materialStyle context.palette).button
                    { icon =
                        Element.row [ Element.spacing spacer.px8 ]
                            [ Element.text removeText
                            , sizedFeatherIcon 18 Icons.logOut
                            ]
                    , text = removeText
                    , onPress = Just toggle
                    }
            )
            |> Element.el
                [ Element.alignRight ]
        , Element.el
            [ Element.alignRight ]
            (createProjectResourcesButton context p)
        ]


createProjectResourcesButton : View.Types.Context -> Project -> Element.Element OuterMsg
createProjectResourcesButton context project =
    let
        projectId =
            GetterSetters.projectIdentifier project

        renderButton : Element.Element Never -> String -> Route.Route -> Element.Attribute OuterMsg -> Element.Element OuterMsg
        renderButton icon text route closeDropdown =
            Element.link
                [ Element.width Element.fill
                , closeDropdown
                ]
                { url = Route.toUrl context.urlPathPrefix route
                , label =
                    Widget.button
                        (dropdownItemStyle context.palette)
                        { icon =
                            Element.el [] icon
                        , text =
                            text
                        , onPress =
                            Just <| SharedMsg <| SharedMsg.NoOp
                        }
                }

        dropdownContent closeDropdown =
            Element.column
                []
                [ renderButton
                    (sizedFeatherIcon 18 Icons.server)
                    (context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    )
                    (Route.ProjectRoute projectId <| Route.InstanceSourcePicker)
                    closeDropdown
                , renderButton
                    (sizedFeatherIcon 18 Icons.hardDrive)
                    (context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    )
                    (Route.ProjectRoute projectId <| Route.VolumeCreate)
                    closeDropdown
                , case ( context.experimentalFeaturesEnabled, project.endpoints.manila ) of
                    ( True, Just _ ) ->
                        renderButton
                            (sizedFeatherIcon 18 Icons.share2)
                            (context.localization.share
                                |> Helpers.String.toTitleCase
                            )
                            (Route.ProjectRoute projectId <| Route.ShareCreate)
                            closeDropdown

                    _ ->
                        Element.none
                , renderButton
                    (sizedFeatherIcon 18 Icons.key)
                    (context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.toTitleCase
                    )
                    (Route.ProjectRoute projectId <| Route.KeypairCreate)
                    closeDropdown
                , renderButton
                    (Style.Widgets.Icon.ipAddress
                        (SH.toElementColor context.palette.primary)
                        18
                    )
                    (context.localization.floatingIpAddress
                        |> Helpers.String.toTitleCase
                    )
                    (Route.ProjectRoute projectId <| Route.FloatingIpCreate Nothing)
                    closeDropdown
                ]

        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).primaryButton
                { text = "Create"
                , icon =
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text "Create"
                        , sizedFeatherIcon 18 <|
                            if dropdownIsShown then
                                Icons.chevronUp

                            else
                                Icons.chevronDown
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
    popover context
        (\createBtnDropdownId -> SharedMsg <| SharedMsg.TogglePopover createBtnDropdownId)
        { id = Helpers.String.hyphenate [ "createBtnDropdown", projectId.projectUuid ]
        , content = dropdownContent
        , contentStyleAttrs = []
        , position = ST.PositionBottomRight
        , distanceToTarget = Nothing
        , target = dropdownTarget
        , targetStyleAttrs = []
        }
