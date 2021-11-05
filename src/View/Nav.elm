module View.Nav exposing (navBar, navBarHeight, navMenu, navMenuWidth)

import Element
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Helpers.Boolean as HB
import Helpers.String
import Route
import State.ViewState
import Style.Helpers as SH
import Style.Widgets.HomeLogo exposing (homeLogo)
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton exposing (FlowOrder(..))
import Style.Widgets.MenuItem as MenuItem
import Style.Widgets.NavButton exposing (navButton)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
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
                (Route.toUrl context.urlPathPrefix (Route.ProjectRoute project.auth.project.uuid Route.ProjectOverview))

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

        navBarHeaderView =
            Element.row
                [ Element.padding 10
                , Element.spacing 10
                , Element.height (Element.px navBarHeight)
                , Element.width Element.fill
                ]
                [ homeLogo context
                    { logoUrl = style.logo
                    , title = HB.toMaybe style.appTitle style.topBarShowAppTitle
                    }
                , Element.row
                    [ Element.alignRight, Element.paddingXY 20 0, Element.spacing 15 ]
                    [ navButton context
                        []
                        { icon = Icon.Bell
                        , label = "Messages"
                        , route = Route.MessageLog False
                        }
                    , navButton context
                        []
                        { icon = Icon.Settings
                        , label = "Settings"
                        , route = Route.Settings
                        }
                    , navButton context
                        []
                        { icon = Icon.HelpCircle
                        , label = "Get Support"
                        , route = Route.GetSupport (State.ViewState.viewStateToSupportableItem outerModel.viewState)
                        }
                    , navButton context
                        []
                        { icon = Icon.Info
                        , label = "About"
                        , route = Route.HelpAbout
                        }
                    ]
                ]
    in
    navBarContainerElement
        navBarContainerAttributes
        [ navBarHeaderView ]
