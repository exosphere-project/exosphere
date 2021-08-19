module View.Nav exposing (navBar, navBarHeight, navMenu, navMenuWidth)

import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FeatherIcons
import Helpers.String
import Page.AllResources
import Page.MessageLog
import State.ViewState
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Style.Widgets.MenuItem as MenuItem
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Helpers as VH
import View.Types


navMenuWidth : Int
navMenuWidth =
    180


navBarHeight : Int
navBarHeight =
    70


navMenu : OuterModel -> View.Types.Context -> Element.Element OuterMsg
navMenu outerModel context =
    let
        projectMenuItem : Project -> Element.Element OuterMsg
        projectMenuItem project =
            let
                projectTitle =
                    VH.friendlyProjectTitle outerModel.sharedModel project

                status =
                    case outerModel.viewState of
                        ProjectView p _ _ ->
                            if p == project.auth.project.uuid then
                                MenuItem.Active

                            else
                                MenuItem.Inactive

                        _ ->
                            MenuItem.Inactive
            in
            MenuItem.menuItem
                context.palette
                status
                (FeatherIcons.cloud |> FeatherIcons.toHtml [] |> Element.html |> Element.el [] |> Just)
                projectTitle
                (Just
                    (SetProjectView project.auth.project.uuid <| AllResources Page.AllResources.init)
                )

        projectMenuItems : List Project -> List (Element.Element OuterMsg)
        projectMenuItems projects =
            List.map projectMenuItem projects

        addProjectMenuItem =
            let
                active =
                    case outerModel.viewState of
                        NonProjectView LoginPicker ->
                            MenuItem.Active

                        NonProjectView (Login _) ->
                            MenuItem.Active

                        _ ->
                            MenuItem.Inactive

                destination =
                    SetNonProjectView <| State.ViewState.defaultLoginViewState outerModel.sharedModel.style.defaultLoginView
            in
            MenuItem.menuItem context.palette
                active
                (FeatherIcons.plusCircle |> FeatherIcons.toHtml [] |> Element.html |> Element.el [] |> Just)
                ("Add " ++ Helpers.String.toTitleCase context.localization.unitOfTenancy)
                (Just destination)
    in
    Element.column
        [ Background.color (SH.toElementColor context.palette.menu.background)
        , Font.color (SH.toElementColor context.palette.menu.on.background)
        , Element.width (Element.px navMenuWidth)
        , Element.height Element.shrink
        , Element.scrollbarY
        , Element.height Element.fill
        ]
        (projectMenuItems outerModel.sharedModel.projects
            ++ [ addProjectMenuItem ]
        )


navBar : OuterModel -> View.Types.Context -> Element.Element OuterMsg
navBar outerModel context =
    let
        navBarContainerAttributes =
            [ Background.color (SH.toElementColor context.palette.menu.secondary)
            , Element.width Element.fill
            , Element.height (Element.px navBarHeight)
            ]

        -- TODO: Responsiveness - Depending on how wide the screen is, return Element.column for navBarContainerElement.
        -- https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#responsiveness
        navBarContainerElement =
            Element.row

        navBarBrand =
            Element.row
                [ Element.padding 5
                , Element.spacing 20
                ]
                [ Element.image [ Element.height (Element.px 50) ] { src = outerModel.sharedModel.style.logo, description = "" }
                , if outerModel.sharedModel.style.topBarShowAppTitle then
                    Element.el
                        [ Region.heading 1
                        , Font.bold
                        , Font.size 26
                        , Font.color (SH.toElementColor context.palette.menu.on.surface)
                        ]
                        (Element.text outerModel.sharedModel.style.appTitle)

                  else
                    Element.none
                ]

        navBarRight =
            Element.row
                [ Element.alignRight, Element.paddingXY 20 0, Element.spacing 15 ]
                [ Element.el
                    [ Font.color (SH.toElementColor context.palette.menu.on.surface)
                    ]
                    (Element.text "")
                , Element.el
                    [ Font.color (SH.toElementColor context.palette.menu.on.surface)
                    ]
                    (Input.button
                        []
                        { onPress = Just (SetNonProjectView <| MessageLog Page.MessageLog.init)
                        , label =
                            Element.row
                                (VH.exoRowAttributes ++ [ Element.spacing 8 ])
                                [ Icon.bell (SH.toElementColor context.palette.menu.on.surface) 20
                                , Element.text "Messages"
                                ]
                        }
                    )
                , Element.el
                    [ Font.color (SH.toElementColor context.palette.menu.on.surface)
                    ]
                    (Input.button
                        []
                        { onPress = Just (SetNonProjectView Settings)
                        , label =
                            Element.row
                                (VH.exoRowAttributes ++ [ Element.spacing 8 ])
                                [ FeatherIcons.settings
                                    |> FeatherIcons.toHtml []
                                    |> Element.html
                                    |> Element.el []
                                , Element.text "Settings"
                                ]
                        }
                    )
                , Element.el
                    [ Font.color (SH.toElementColor context.palette.menu.on.surface)
                    ]
                    (Input.button
                        []
                        { onPress =
                            Just
                                (SharedMsg <|
                                    SharedMsg.NavigateToView <|
                                        SharedMsg.GetSupport
                                            (State.ViewState.viewStateToSupportableItem outerModel.viewState)
                                )
                        , label =
                            Element.row
                                (VH.exoRowAttributes ++ [ Element.spacing 8 ])
                                [ FeatherIcons.helpCircle
                                    |> FeatherIcons.toHtml []
                                    |> Element.html
                                    |> Element.el []
                                , Element.text "Get Support"
                                ]
                        }
                    )
                , Element.el
                    [ Font.color (SH.toElementColor context.palette.menu.on.surface)
                    ]
                    (Input.button
                        []
                        { onPress = Just <| SharedMsg <| SharedMsg.NavigateToView <| SharedMsg.HelpAbout
                        , label =
                            Element.row
                                (VH.exoRowAttributes ++ [ Element.spacing 8 ])
                                [ FeatherIcons.info
                                    |> FeatherIcons.toHtml []
                                    |> Element.html
                                    |> Element.el []
                                , Element.text "About"
                                ]
                        }
                    )

                -- This is where the right-hand side menu would go
                ]

        navBarHeaderView =
            Element.row
                [ Element.padding 10
                , Element.spacing 10
                , Element.height (Element.px navBarHeight)
                , Element.width Element.fill
                ]
                [ navBarBrand
                , navBarRight
                ]
    in
    navBarContainerElement
        navBarContainerAttributes
        [ navBarHeaderView ]
