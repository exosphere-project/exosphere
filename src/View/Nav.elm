module View.Nav exposing (navBar, navBarHeight, navMenu, navMenuWidth)

import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FeatherIcons
import Helpers.String
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Style.Widgets.MenuItem as MenuItem
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import View.GetSupport
import View.Helpers as VH
import View.Types


navMenuWidth : Int
navMenuWidth =
    180


navBarHeight : Int
navBarHeight =
    70


navMenu : Model -> View.Types.Context -> Element.Element Msg
navMenu model context =
    let
        projectMenuItem : Project -> Element.Element Msg
        projectMenuItem project =
            let
                projectTitle =
                    VH.friendlyProjectTitle model project

                status =
                    case model.viewState of
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
                projectTitle
                (Just
                    (ProjectMsg project.auth.project.uuid
                        (SetProjectView <|
                            ListProjectServers
                                Defaults.serverListViewParams
                        )
                    )
                )

        projectMenuItems : List Project -> List (Element.Element Msg)
        projectMenuItems projects =
            List.map projectMenuItem projects

        addProjectMenuItem =
            let
                active =
                    case model.viewState of
                        NonProjectView LoginPicker ->
                            MenuItem.Active

                        NonProjectView (Login _) ->
                            MenuItem.Active

                        _ ->
                            MenuItem.Inactive

                destination =
                    model.style.defaultLoginView
                        |> Maybe.map (\loginView -> SetNonProjectView (Login loginView))
                        |> Maybe.withDefault (SetNonProjectView LoginPicker)
            in
            MenuItem.menuItem context.palette
                active
                ("Add " ++ Helpers.String.capitalizeString context.localization.unitOfTenancy)
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
        (projectMenuItems model.projects
            ++ [ addProjectMenuItem ]
        )


navBar : Model -> View.Types.Context -> Element.Element Msg
navBar model context =
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
                [ Element.padding 10
                , Element.spacing 20
                ]
                [ Element.el
                    [ Region.heading 1
                    , Font.bold
                    , Font.size 26
                    , Font.color (SH.toElementColor context.palette.menu.on.surface)
                    ]
                    (Element.text model.style.appTitle)
                , Element.image [ Element.height (Element.px 40) ] { src = model.style.logo, description = "" }
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
                        { onPress = Just (SetNonProjectView MessageLog)
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
                                (SetNonProjectView <|
                                    GetSupport
                                        (View.GetSupport.viewStateToSupportableItem model.viewState)
                                        ""
                                        False
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
                        { onPress = Just (SetNonProjectView HelpAbout)
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
