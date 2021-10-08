module View.Nav exposing (navBar, navBarHeight, navMenu, navMenuWidth)

import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.String
import Route
import State.ViewState
import Style.Helpers as SH
import Style.Widgets.HomeLogo exposing (homeLogo)
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
                (Route.toUrl context.urlPathPrefix (Route.ProjectRoute project.auth.project.uuid Route.AllResourcesList))

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

                destUrl =
                    Route.toUrl context.urlPathPrefix
                        (Route.defaultLoginPage
                            outerModel.sharedModel.style.defaultLoginView
                        )
            in
            MenuItem.menuItem context.palette
                active
                (FeatherIcons.plusCircle |> FeatherIcons.toHtml [] |> Element.html |> Element.el [] |> Just)
                ("Add " ++ Helpers.String.toTitleCase context.localization.unitOfTenancy)
                destUrl
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
        { style } =
            outerModel.sharedModel

        navBarContainerAttributes =
            [ Background.color (SH.toElementColor context.palette.menu.secondary)
            , Element.width Element.fill
            , Element.height (Element.px navBarHeight)
            ]

        -- TODO: Responsiveness - Depending on how wide the screen is, return Element.column for navBarContainerElement.
        -- https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#responsiveness
        navBarContainerElement =
            Element.row

        navBarRight =
            let
                renderButton : Route.Route -> List (Element.Element OuterMsg) -> Element.Element OuterMsg
                renderButton route buttonLabelRowContents =
                    Element.link
                        [ Font.color (SH.toElementColor context.palette.menu.on.surface) ]
                        { url = Route.toUrl context.urlPathPrefix route
                        , label =
                            Input.button []
                                { onPress = Just (SharedMsg <| SharedMsg.NoOp)
                                , label =
                                    Element.row
                                        (VH.exoRowAttributes ++ [ Element.spacing 8 ])
                                        buttonLabelRowContents
                                }
                        }
            in
            Element.row
                [ Element.alignRight, Element.paddingXY 20 0, Element.spacing 15 ]
                [ Element.el
                    [ Font.color (SH.toElementColor context.palette.menu.on.surface) ]
                    (Element.text "")
                , renderButton (Route.MessageLog False)
                    [ Icon.bell (SH.toElementColor context.palette.menu.on.surface) 20
                    , Element.text "Messages"
                    ]
                , renderButton Route.Settings
                    [ FeatherIcons.settings |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                    , Element.text "Settings"
                    ]
                , renderButton (Route.GetSupport (State.ViewState.viewStateToSupportableItem outerModel.viewState))
                    [ FeatherIcons.helpCircle |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                    , Element.text "Get Support"
                    ]
                , renderButton Route.HelpAbout
                    [ FeatherIcons.info |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                    , Element.text "About"
                    ]

                -- This is where the right-hand side menu would go
                ]

        navBarHeaderView =
            Element.row
                [ Element.padding 10
                , Element.spacing 10
                , Element.height (Element.px navBarHeight)
                , Element.width Element.fill
                ]
                [ homeLogo context { logoUrl = style.logo, title = style.appTitle }
                    |> VH.renderIf style.topBarShowAppTitle
                , navBarRight
                ]
    in
    navBarContainerElement
        navBarContainerAttributes
        [ navBarHeaderView ]
