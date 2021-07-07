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
        , OpenstackLoginFormEntryType(..)
        , OpenstackLoginViewParams
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


viewLoginPicker : View.Types.Context -> Maybe OpenIdConnectLoginConfig -> Element.Element Msg
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
                                            Defaults.openStackLoginViewParams
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
                (case maybeOpenIdConnectLoginConfig of
                    Just oidcLoginConfig ->
                        [ oidcLoginMethod oidcLoginConfig ]

                    Nothing ->
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


loginPickerButton : View.Types.Context -> Element.Element Msg
loginPickerButton context =
    Widget.textButton
        (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
        { text = "Other Login Methods"
        , onPress =
            Just <| SetNonProjectView <| LoginPicker
        }


viewLoginOpenstack : View.Types.Context -> OpenstackLoginViewParams -> Element.Element Msg
viewLoginOpenstack context viewParams =
    let
        allCredsEntered =
            -- These fields must be populated before login can be attempted
            [ viewParams.creds.authUrl
            , viewParams.creds.userDomain
            , viewParams.creds.username
            , viewParams.creds.password
            ]
                |> List.any (\x -> String.isEmpty x)
                |> not
    in
    Element.column VH.exoColumnAttributes
        [ Element.el
            (VH.heading2 context.palette)
            (Element.text "Add an OpenStack Account")
        , Element.el
            VH.exoElementAttributes
            (case viewParams.formEntryType of
                LoginViewCredsEntry ->
                    loginOpenstackCredsEntry context viewParams allCredsEntered

                LoginViewOpenRcEntry ->
                    loginOpenstackOpenRcEntry context viewParams
            )
        , Element.row (VH.exoRowAttributes ++ [ Element.width Element.fill ])
            (case viewParams.formEntryType of
                LoginViewCredsEntry ->
                    [ Element.el [] (loginPickerButton context)
                    , Widget.textButton
                        (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                        { text = "Use OpenRC File"
                        , onPress = Just <| SetNonProjectView <| Login <| LoginOpenstack { viewParams | formEntryType = LoginViewOpenRcEntry }
                        }
                    , Element.el [ Element.alignRight ]
                        (Widget.textButton
                            (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                            { text = "Log In"
                            , onPress =
                                if allCredsEntered then
                                    Just <| RequestUnscopedToken viewParams.creds

                                else
                                    Nothing
                            }
                        )
                    ]

                LoginViewOpenRcEntry ->
                    [ Element.el VH.exoPaddingSpacingAttributes
                        (Widget.textButton
                            (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                            { text = "Cancel"
                            , onPress = Just <| SetNonProjectView <| Login <| LoginOpenstack { viewParams | formEntryType = LoginViewCredsEntry }
                            }
                        )
                    , Element.el (VH.exoPaddingSpacingAttributes ++ [ Element.alignRight ])
                        (Widget.textButton
                            (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                            { text = "Submit"
                            , onPress = Just <| SubmitOpenRc viewParams.creds viewParams.openRc
                            }
                        )
                    ]
            )
        ]


loginOpenstackCredsEntry : View.Types.Context -> OpenstackLoginViewParams -> Bool -> Element.Element Msg
loginOpenstackCredsEntry context viewParams allCredsEntered =
    let
        creds =
            viewParams.creds

        updateCreds : OSTypes.OpenstackLogin -> Msg
        updateCreds newCreds =
            SetNonProjectView <| Login <| LoginOpenstack { viewParams | creds = newCreds }

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
        [ Element.el [] (Element.text "Enter your credentials")
        , textField
            creds.authUrl
            "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"
            (\u -> updateCreds { creds | authUrl = u })
            "Keystone auth URL"
        , textField
            creds.userDomain
            "User domain e.g. default"
            (\d -> updateCreds { creds | userDomain = d })
            "User Domain (name or ID)"
        , textField
            creds.username
            "User name e.g. demo"
            (\u -> updateCreds { creds | username = u })
            "User Name"
        , Input.currentPassword
            (VH.inputItemAttributes context.palette.surface)
            { text = creds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = \p -> updateCreds { creds | password = p }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Password")
            }
        , if allCredsEntered then
            Element.none

          else
            Element.el
                (VH.exoElementAttributes
                    ++ [ Element.alignRight
                       , Font.color (context.palette.error |> SH.toElementColor)
                       ]
                )
                (Element.text "All fields are required.")
        ]


loginOpenstackOpenRcEntry : View.Types.Context -> OpenstackLoginViewParams -> Element.Element Msg
loginOpenstackOpenRcEntry context viewParams =
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.spacing 15
               , Element.height (Element.fill |> Element.minimum 250)
               ]
        )
        [ Element.paragraph []
            [ Element.text "Paste an "
            , VH.browserLink
                context
                "https://docs.openstack.org/newton/install-guide-rdo/keystone-openrc.html"
              <|
                View.Types.BrowserLinkTextLabel "OpenRC"
            , Element.text " file"
            ]
        , Input.multiline
            (VH.inputItemAttributes context.palette.background
                ++ [ Element.width (Element.px 500)
                   , Element.height Element.fill
                   , Font.size 12
                   ]
            )
            { onChange = \o -> SetNonProjectView <| Login <| LoginOpenstack { viewParams | openRc = o }
            , text = viewParams.openRc
            , placeholder = Nothing
            , label = Input.labelLeft [] Element.none
            , spellcheck = False
            }
        ]


viewLoginJetstream : View.Types.Context -> JetstreamCreds -> Element.Element Msg
viewLoginJetstream context jetstreamCreds =
    let
        updateCreds : JetstreamCreds -> Msg
        updateCreds newCreds =
            SetNonProjectView <| Login <| LoginJetstream newCreds
    in
    Element.column VH.exoColumnAttributes
        [ Element.el (VH.heading2 context.palette)
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


jetstreamLoginText : View.Types.Context -> Element.Element Msg
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
