module Page.LoginPicker exposing (Msg(..), headerView, update, view)

import Browser
import Color
import Element
import Element.Background as Background
import Element.Border as Border
import Route
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type Msg
    = SharedMsg SharedMsg.SharedMsg


type alias LoginMethod =
    { logo : Element.Element Msg
    , button : Element.Element Msg
    , description : String
    }


update : Msg -> ( (), Cmd Msg, SharedMsg.SharedMsg )
update msg =
    case msg of
        SharedMsg sharedMsg ->
            ( (), Cmd.none, sharedMsg )


headerView : View.Types.Context -> Element.Element msg
headerView context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        "Choose a Login Method"


view : View.Types.Context -> SharedModel -> Element.Element Msg
view context sharedModel =
    let
        renderLinkButton route text =
            Element.link []
                { url = Route.toUrl context.urlPathPrefix route
                , label =
                    Button.primary
                        context.palette
                        { text = text
                        , onPress =
                            Just <| SharedMsg <| SharedMsg.NoOp
                        }
                }

        defaultLoginMethods =
            [ { logo =
                    Element.image [ Element.centerX, Element.width (Element.px 180), Element.height (Element.px 100) ] { src = "assets/img/openstack-logo.svg", description = "" }
              , button = renderLinkButton (Route.LoginOpenstack Nothing) "Add OpenStack Account"
              , description =
                    ""
              }
            ]

        oidcLoginMethod oidcLoginConfig =
            { logo =
                Element.image
                    [ Element.centerX
                    , Element.centerY
                    ]
                    { src = oidcLoginConfig.oidcLoginIcon
                    , description = oidcLoginConfig.oidcLoginButtonDescription
                    }
            , button =
                Button.default
                    context.palette
                    { text = oidcLoginConfig.oidcLoginButtonLabel
                    , onPress =
                        let
                            url =
                                oidcLoginConfig.webssoUrl
                        in
                        Just <| SharedMsg <| SharedMsg.LinkClicked <| Browser.External url
                    }
            , description =
                oidcLoginConfig.oidcLoginButtonDescription
            }

        loginMethods =
            List.append
                defaultLoginMethods
                (case sharedModel.openIdConnectLoginConfig of
                    Just oidcLoginConfig ->
                        [ oidcLoginMethod oidcLoginConfig ]

                    Nothing ->
                        []
                )

        renderLoginMethod : LoginMethod -> Element.Element Msg
        renderLoginMethod loginMethod =
            Element.el
                [ Element.width <| Element.px 380
                ]
            <|
                Widget.column
                    (SH.materialStyle context.palette).cardColumn
                    [ Element.column
                        [ Element.width <| Element.px 300
                        , Element.centerX
                        , Element.paddingXY spacer.px12 spacer.px16
                        , Element.spacing spacer.px16
                        ]
                        [ Element.el
                            -- Yes, a hard-coded color when we've otherwise removed them from the app. These logos need a light background to look right.
                            [ Background.color <| SH.toElementColor <| Color.rgb255 255 255 255
                            , Element.centerX
                            , Element.paddingXY spacer.px16 0
                            , Border.rounded 10
                            , Element.height <| Element.px 100
                            ]
                            loginMethod.logo
                        , Element.el [ Element.centerX ] loginMethod.button
                        , Element.paragraph [ Element.height <| Element.minimum 50 Element.shrink ] [ Element.text loginMethod.description ]
                        ]
                    ]
    in
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing spacer.px24
        ]
        (List.map renderLoginMethod loginMethods)
