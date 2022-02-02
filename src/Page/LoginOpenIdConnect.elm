module Page.LoginOpenIdConnect exposing (Model, init, view)

import Browser
import Color
import Element
import Element.Background as Background
import Element.Border as Border
import Style.Helpers as SH
import Types.HelperTypes as HelperTypes
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    HelperTypes.OpenIdConnectLoginConfig


init : HelperTypes.OpenIdConnectLoginConfig -> Model
init oidcLoginConfig =
    oidcLoginConfig


view : View.Types.Context -> SharedModel -> Model -> Element.Element SharedMsg.SharedMsg
view context _ model =
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette)
            (Element.text model.oidcLoginButtonLabel)
        , Element.column VH.contentContainer
            [ Element.el
                [ Element.width <| Element.px 380
                , Element.centerX
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
                            (Element.image
                                [ Element.centerX
                                , Element.centerY
                                ]
                                { src = model.oidcLoginIcon
                                , description = model.oidcLoginButtonDescription
                                }
                            )
                        , Element.el [ Element.centerX ]
                            (Widget.textButton
                                (SH.materialStyle context.palette).primaryButton
                                { text = model.oidcLoginButtonLabel
                                , onPress =
                                    let
                                        url =
                                            model.keystoneAuthUrl ++ model.webssoKeystoneEndpoint
                                    in
                                    Just <| SharedMsg.LinkClicked <| Browser.External url
                                }
                            )
                        , Element.paragraph
                            [ Element.height <| Element.minimum 50 Element.shrink ]
                            [ Element.text model.oidcLoginButtonDescription ]
                        ]
                    ]
            , Element.row [ Element.width Element.fill ]
                [ Element.el [] (VH.loginPickerButton context)
                ]
            ]
        ]