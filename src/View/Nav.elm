module View.Nav exposing (navBar, navBarHeight, navMenuWidth)

import Element
import Element.Background as Background
import Helpers.Boolean as HB
import Route
import State.ViewState
import Style.Helpers as SH exposing (spacer)
import Style.Widgets.HomeLogo exposing (homeLogo)
import Style.Widgets.Icon as Icon
import Style.Widgets.NavButton exposing (navButton)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import View.Types


navMenuWidth : Int
navMenuWidth =
    180


navBarHeight : Int
navBarHeight =
    70


navBar : OuterModel -> View.Types.Context -> Element.Element OuterMsg
navBar outerModel context =
    let
        { style } =
            outerModel.sharedModel

        navBarContainerAttributes =
            [ Background.color (SH.toElementColor context.palette.menu.background)
            , Element.width Element.fill
            , Element.height (Element.px navBarHeight)
            ]

        -- TODO: Responsiveness - Depending on how wide the screen is, return Element.column for navBarContainerElement.
        -- https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#responsiveness
        navBarContainerElement =
            Element.row

        navBarHeaderView =
            Element.row
                [ Element.padding spacer.px12
                , Element.spacing spacer.px12
                , Element.height (Element.px navBarHeight)
                , Element.width Element.fill
                ]
                [ homeLogo context
                    { logoUrl = style.logo
                    , title = HB.toMaybe style.appTitle style.topBarShowAppTitle
                    }
                , Element.row
                    [ Element.alignRight
                    , Element.spacing spacer.px16
                    ]
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
