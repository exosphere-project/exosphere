module View.Nav exposing (navBar, navBarHeight)

import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FeatherIcons
import Route
import State.ViewState
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


navBarHeight : Int
navBarHeight =
    70


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
            Element.link []
                { url = Route.toUrl context.urlPathPrefix Route.Home
                , label =
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
                }

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
                [ navBarBrand
                , navBarRight
                ]
    in
    navBarContainerElement
        navBarContainerAttributes
        [ navBarHeaderView ]
