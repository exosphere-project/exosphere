module View.LoginPicker exposing (loginPicker)

import Color
import Element
import Element.Background as Background
import Element.Border as Border
import Style.Helpers as SH
import Types.HelperTypes exposing (JetstreamCreds, JetstreamProvider(..))
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedMsg exposing (SharedMsg(..))
import Types.Types exposing (OpenIdConnectLoginConfig)
import Types.View
    exposing
        ( LoginView(..)
        , NonProjectViewConstructor(..)
        )
import View.Helpers as VH
import View.LoginOpenstack
import View.Types
import Widget


type alias LoginMethod =
    { logo : Element.Element OuterMsg
    , button : Element.Element OuterMsg
    , description : String
    }


loginPicker : View.Types.Context -> Maybe OpenIdConnectLoginConfig -> Element.Element OuterMsg
loginPicker context maybeOpenIdConnectLoginConfig =
    let
        defaultLoginMethods =
            [ { logo =
                    Element.image [ Element.centerX, Element.width (Element.px 180), Element.height (Element.px 100) ] { src = "assets/img/openstack-logo.svg", description = "" }
              , button =
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Add OpenStack Account"
                        , onPress =
                            Just <|
                                SetNonProjectView <|
                                    Login <|
                                        LoginOpenstack <|
                                            View.LoginOpenstack.init
                        }
              , description =
                    ""
              }
            , { logo =
                    Element.image [ Element.centerX, Element.width (Element.px 150), Element.height (Element.px 100) ] { src = "assets/img/jetstream-logo.svg", description = "" }
              , button =
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Add Jetstream Account"
                        , onPress =
                            Just <|
                                SetNonProjectView <|
                                    Login <|
                                        LoginJetstream <|
                                            JetstreamCreds BothJetstreamClouds "" ""
                        }
              , description =
                    "Recommended login method for Jetstream Cloud"
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
                        Just <| SharedMsg <| NavigateToUrl url
                    }
            , description =
                oidcLoginConfig.oidcLoginButtonDescription
            }

        loginMethods =
            List.append
                defaultLoginMethods
                (case maybeOpenIdConnectLoginConfig of
                    Just oidcLoginConfig ->
                        [ oidcLoginMethod oidcLoginConfig ]

                    Nothing ->
                        []
                )

        renderLoginMethod : LoginMethod -> Element.Element OuterMsg
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
