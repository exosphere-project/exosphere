module View.Login exposing (viewLoginJetstream, viewLoginOpenstack, viewLoginPicker)

import Color
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( JetstreamCreds
        , JetstreamProvider(..)
        , LoginView(..)
        , Msg(..)
        , NonProjectViewConstructor(..)
        , OpenIdConnectLoginConfig
        )
import View.Helpers as VH
import View.Types
import Widget
import Widget.Style.Material


type alias LoginMethod =
    { logo : Element.Element Msg
    , button : Element.Element Msg
    , description : String
    }


viewLoginPicker : View.Types.ViewContext -> Maybe OpenIdConnectLoginConfig -> Element.Element Msg
viewLoginPicker context maybeOpenIdConnectLoginConfig =
    let
        defaultLoginMethods =
            [ { logo =
                    Element.image [ Element.centerX, Element.width (Element.px 180), Element.height (Element.px 100) ] { src = "assets/img/openstack-logo.svg", description = "" }
              , button =
                    Widget.textButton
                        (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                        { text = "Add OpenStack Account"
                        , onPress =
                            Just <|
                                SetNonProjectView <|
                                    Login <|
                                        LoginOpenstack <|
                                            Defaults.openstackCreds
                        }
              , description =
                    ""
              }
            , { logo =
                    Element.image [ Element.centerX, Element.width (Element.px 150), Element.height (Element.px 100) ] { src = "assets/img/jetstream-logo.svg", description = "" }
              , button =
                    Widget.textButton
                        (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                        { text = "Add Jetstream Account"
                        , onPress =
                            Just <|
                                SetNonProjectView <|
                                    Login <|
                                        LoginJetstream <|
                                            JetstreamCreds BothJetstreamClouds "" "" ""
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
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                    { text = oidcLoginConfig.oidcLoginButtonLabel
                    , onPress =
                        let
                            url =
                                oidcLoginConfig.keystoneAuthUrl ++ oidcLoginConfig.webssoKeystoneEndpoint
                        in
                        Just <| NavigateToUrl url
                    }
            , description =
                oidcLoginConfig.oidcLoginButtonDescription
            }

        loginMethods =
            List.append
                defaultLoginMethods
                (case ( maybeOpenIdConnectLoginConfig, context.isElectron ) of
                    ( Just oidcLoginConfig, False ) ->
                        [ oidcLoginMethod oidcLoginConfig ]

                    ( _, _ ) ->
                        []
                )

        renderLoginMethod : LoginMethod -> Element.Element Msg
        renderLoginMethod loginMethod =
            Element.column VH.exoColumnAttributes
                [ Element.el
                    -- Yes, a hard-coded color when we've otherwise removed them from the app. These logos need a light background to look right.
                    [ Background.color <| SH.toElementColor <| Color.rgb255 255 255 255
                    , Element.centerX
                    , Element.paddingXY 15 0
                    , Border.rounded 10
                    , Element.height <| Element.px 100
                    ]
                    loginMethod.logo
                , loginMethod.button
                , Element.paragraph [ Element.height <| Element.minimum 60 Element.shrink ] [ Element.text loginMethod.description ]
                ]
    in
    Element.column VH.exoColumnAttributes
        [ Element.text "Choose a login method"
        , Element.row
            (VH.exoRowAttributes ++ [ Element.spacing 30 ])
            (List.map renderLoginMethod loginMethods)
        ]


loginPickerButton : View.Types.ViewContext -> Element.Element Msg
loginPickerButton context =
    Widget.textButton
        (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
        { text = "See Other Login Methods"
        , onPress =
            Just <| SetNonProjectView <| LoginPicker
        }


viewLoginOpenstack : View.Types.ViewContext -> OSTypes.OpenstackLogin -> Element.Element Msg
viewLoginOpenstack context openstackCreds =
    Element.column VH.exoColumnAttributes
        [ Element.el
            VH.heading2
            (Element.text "Add an OpenStack Account")
        , Element.wrappedRow
            VH.exoRowAttributes
            [ loginOpenstackCredsEntry context openstackCreds
            , loginOpenstackOpenRcEntry context openstackCreds
            ]
        , Element.row (VH.exoRowAttributes ++ [ Element.width Element.fill ])
            [ Element.el [] (loginPickerButton context)
            , Element.el (VH.exoPaddingSpacingAttributes ++ [ Element.alignRight ])
                (Widget.textButton
                    (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                    { text = "Log In"
                    , onPress =
                        Just <| RequestUnscopedToken openstackCreds
                    }
                )
            ]
        ]


loginOpenstackCredsEntry : View.Types.ViewContext -> OSTypes.OpenstackLogin -> Element.Element Msg
loginOpenstackCredsEntry context openstackCreds =
    let
        updateCreds : OSTypes.OpenstackLogin -> Msg
        updateCreds newCreds =
            SetNonProjectView <| Login <| LoginOpenstack newCreds

        textField text placeholderText onChange labelText =
            Input.text
                (VH.inputItemAttributes context.palette.background)
                { text = text
                , placeholder = Just (Input.placeholder [] (Element.text placeholderText))
                , onChange = onChange
                , label = Input.labelAbove [ Font.size 14 ] (Element.text labelText)
                }
    in
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.width (Element.px 500)
               , Element.alignTop
               ]
        )
        [ Element.el [] (Element.text "Either enter your credentials...")
        , textField
            openstackCreds.authUrl
            "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"
            (\u -> updateCreds { openstackCreds | authUrl = u })
            "Keystone auth URL"
        , textField
            openstackCreds.userDomain
            "User domain e.g. default"
            (\d -> updateCreds { openstackCreds | userDomain = d })
            "User Domain (name or ID)"
        , textField
            openstackCreds.username
            "User name e.g. demo"
            (\u -> updateCreds { openstackCreds | username = u })
            "User Name"
        , Input.currentPassword
            (VH.inputItemAttributes context.palette.surface)
            { text = openstackCreds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = \p -> updateCreds { openstackCreds | password = p }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Password")
            }
        ]


loginOpenstackOpenRcEntry : View.Types.ViewContext -> OSTypes.OpenstackLogin -> Element.Element Msg
loginOpenstackOpenRcEntry context openstackCreds =
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.spacing 15
               , Element.height (Element.fill |> Element.minimum 250)
               ]
        )
        [ Element.paragraph []
            [ Element.text "...or paste an "
            , VH.browserLink
                context
                "https://docs.openstack.org/newton/install-guide-rdo/keystone-openrc.html"
              <|
                View.Types.BrowserLinkTextLabel "OpenRC"
            , Element.text " file"
            ]
        , Input.multiline
            (VH.inputItemAttributes context.palette.background
                ++ [ Element.width (Element.px 300)
                   , Element.height Element.fill
                   , Font.size 12
                   ]
            )
            { onChange = \o -> InputOpenRc openstackCreds o
            , text = ""
            , placeholder = Nothing
            , label = Input.labelLeft [] Element.none
            , spellcheck = False
            }
        ]


viewLoginJetstream : View.Types.ViewContext -> JetstreamCreds -> Element.Element Msg
viewLoginJetstream context jetstreamCreds =
    let
        updateCreds : JetstreamCreds -> Msg
        updateCreds newCreds =
            SetNonProjectView <| Login <| LoginJetstream newCreds
    in
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2
            (Element.text "Add a Jetstream Cloud Account")
        , jetstreamLoginText context
        , Element.column VH.exoColumnAttributes
            [ Input.text
                (VH.inputItemAttributes context.palette.background)
                { text = jetstreamCreds.taccUsername
                , placeholder = Just (Input.placeholder [] (Element.text "tg******"))
                , onChange = \un -> updateCreds { jetstreamCreds | taccUsername = un }
                , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Username")
                }
            , Input.currentPassword
                (VH.inputItemAttributes context.palette.background)
                { text = jetstreamCreds.taccPassword
                , placeholder = Nothing
                , onChange = \pw -> updateCreds { jetstreamCreds | taccPassword = pw }
                , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Password")
                , show = False
                }
            , Input.radio []
                { label = Input.labelAbove [] (Element.text "Provider")
                , onChange = \x -> updateCreds { jetstreamCreds | jetstreamProviderChoice = x }
                , options =
                    [ Input.option IUCloud (Element.text "IU Cloud")
                    , Input.option TACCCloud (Element.text "TACC Cloud")
                    , Input.option BothJetstreamClouds (Element.text "Both Clouds")
                    ]
                , selected = Just jetstreamCreds.jetstreamProviderChoice
                }
            ]
        , Element.row (VH.exoRowAttributes ++ [ Element.width Element.fill ])
            [ Element.el [] (loginPickerButton context)
            , Element.el (VH.exoPaddingSpacingAttributes ++ [ Element.alignRight ])
                (Widget.textButton
                    (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                    { text = "Log In"
                    , onPress =
                        Just (JetstreamLogin jetstreamCreds)
                    }
                )
            ]
        ]


jetstreamLoginText : View.Types.ViewContext -> Element.Element Msg
jetstreamLoginText context =
    Element.column VH.exoColumnAttributes
        [ Element.paragraph
            []
            [ Element.text "To use Exosphere with "
            , VH.browserLink
                context
                "https://jetstream-cloud.org"
              <|
                View.Types.BrowserLinkTextLabel "Jetstream Cloud"
            , Element.text ", you need access to a Jetstream allocation. Possible ways to get this:"
            ]
        , Element.paragraph
            []
            [ Element.text "- Request access to the Exosphere Trial Allocation; please create an account on "
            , VH.browserLink
                context
                "https://portal.xsede.org"
              <|
                View.Types.BrowserLinkTextLabel "XSEDE User Portal"
            , Element.text ", then "
            , VH.browserLink
                context
                "https://gitlab.com/exosphere/exosphere/issues/new"
              <|
                View.Types.BrowserLinkTextLabel "create an issue"
            , Element.text " asking for access and providing your XSEDE username."
            ]
        , Element.paragraph
            []
            [ Element.text "- If you know someone else who already has an allocation, they can add you to it. (See \"How do I let other XSEDE accounts use my allocation?\" on "
            , VH.browserLink
                context
                "https://iujetstream.atlassian.net/wiki/spaces/JWT/pages/537460937/Jetstream+Allocations+FAQ"
              <|
                View.Types.BrowserLinkTextLabel "this FAQ"
            , Element.text ")"
            ]
        , Element.paragraph
            []
            [ Element.text "- "
            , VH.browserLink
                context
                "https://iujetstream.atlassian.net/wiki/spaces/JWT/pages/49184781/Jetstream+Allocations"
              <|
                View.Types.BrowserLinkTextLabel "Apply for your own Startup Allocation"
            ]
        , Element.paragraph [] []
        , Element.paragraph
            []
            [ Element.text "Once you have access to an allocation, collect these things:"
            ]
        , Element.paragraph
            []
            [ Element.text "1. TACC username (usually looks like 'tg******'); "
            , VH.browserLink
                context
                "https://portal.tacc.utexas.edu/password-reset/-/password/forgot-username"
              <|
                View.Types.BrowserLinkTextLabel "look up your TACC username"
            ]
        , Element.paragraph
            []
            [ Element.text "2. TACC password; "
            , VH.browserLink
                context
                "https://portal.tacc.utexas.edu/password-reset/-/password/request-reset"
              <|
                View.Types.BrowserLinkTextLabel "set your TACC password"
            ]
        ]
