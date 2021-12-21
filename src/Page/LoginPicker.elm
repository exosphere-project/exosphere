module Page.LoginPicker exposing (Msg(..), update, view)

import Browser
import Color
import Element
import Element.Background as Background
import Element.Border as Border
import Route
import Style.Helpers as SH
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


view : View.Types.Context -> SharedModel -> Element.Element Msg
view context sharedModel =
    let
        renderLinkButton route text =
            Element.link []
                { url = Route.toUrl context.urlPathPrefix route
                , label =
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
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
            , { logo =
                    Element.image [ Element.centerX, Element.width (Element.px 150), Element.height (Element.px 100) ] { src = "assets/img/jetstream-logo.svg", description = "" }
              , button = renderLinkButton (Route.LoginJetstream Nothing) "Add Jetstream1 Account"
              , description =
                    "Recommended login method for Jetstream1"
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
                Widget.textButton
                    (SH.materialStyle context.palette).button
                    { text = oidcLoginConfig.oidcLoginButtonLabel
                    , onPress =
                        let
                            url =
                                oidcLoginConfig.keystoneAuthUrl ++ oidcLoginConfig.webssoKeystoneEndpoint
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
                    [ Element.column [ Element.width <| Element.px 300, Element.centerX, Element.paddingXY 10 15, Element.spacing 15 ]
                        [ Element.el
                            -- Yes, a hard-coded color when we've otherwise removed them from the app. These logos need a light background to look right.
                            [ Background.color <| SH.toElementColor <| Color.rgb255 255 255 255
                            , Element.centerX
                            , Element.paddingXY 15 0
                            , Border.rounded 10
                            , Element.height <| Element.px 100
                            ]
                            loginMethod.logo
                        , Element.el [ Element.centerX ] loginMethod.button
                        , Element.paragraph [ Element.height <| Element.minimum 50 Element.shrink ] [ Element.text loginMethod.description ]
                        ]
                    ]
    in
    Element.column VH.contentContainer
        [ Element.row (VH.heading2 context.palette) [ Element.text "Choose a Login Method" ]
        , Element.wrappedRow
            (VH.exoRowAttributes
                ++ [ Element.width Element.fill
                   , Element.spacing 40
                   ]
            )
            (List.map renderLoginMethod loginMethods)
        ]
