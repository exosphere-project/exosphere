module View.Login exposing (viewLoginJetstream, viewLoginOpenstack, viewLoginPicker)

import Element
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Modifier as Modifier
import OpenStack.Types as OSTypes
import Types.Types exposing (JetstreamCreds, JetstreamProvider(..), Model, Msg(..), NonProjectViewConstructor(..))
import View.Helpers as VH
import View.Types


viewLoginPicker : Element.Element Msg
viewLoginPicker =
    Element.column VH.exoColumnAttributes
        [ Element.text "Choose a login method"
        , Element.row VH.exoRowAttributes
            [ Element.column VH.exoColumnAttributes
                [ Element.image [ Element.centerX, Element.width (Element.px 180), Element.height (Element.px 100) ] { src = "assets/img/openstack-logo.svg", description = "" }
                , Button.button
                    []
                    (Just
                        (SetNonProjectView
                            (LoginOpenstack
                                (OSTypes.OpenstackLogin "" "" "" "" "" "")
                            )
                        )
                    )
                    "Add OpenStack Account"
                ]
            , Element.column VH.exoColumnAttributes
                [ Element.image [ Element.centerX, Element.width (Element.px 150), Element.height (Element.px 100) ] { src = "assets/img/jetstream-logo.svg", description = "" }
                , Button.button
                    []
                    (Just
                        (SetNonProjectView
                            (LoginJetstream
                                (JetstreamCreds TACCCloud "" "" "")
                            )
                        )
                    )
                    "Add Jetstream Cloud Account"
                ]
            ]
        ]


viewLoginOpenstack : Model -> OSTypes.OpenstackLogin -> Element.Element Msg
viewLoginOpenstack model openstackCreds =
    Element.column VH.exoColumnAttributes
        [ Element.el
            VH.heading2
            (Element.text "Add an OpenStack Account")
        , Element.wrappedRow
            VH.exoRowAttributes
            [ loginOpenstackCredsEntry model openstackCreds
            , loginOpenstackOpenRcEntry model openstackCreds
            ]
        , Element.el (VH.exoPaddingSpacingAttributes ++ [ Element.alignRight ])
            (Button.button
                [ Modifier.Primary ]
                (Just <| RequestUnscopedToken openstackCreds)
                "Log In"
            )
        ]


loginOpenstackCredsEntry : Model -> OSTypes.OpenstackLogin -> Element.Element Msg
loginOpenstackCredsEntry _ openstackCreds =
    let
        updateCreds : OSTypes.OpenstackLogin -> Msg
        updateCreds newCreds =
            SetNonProjectView <| LoginOpenstack newCreds
    in
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.width (Element.px 500)
               , Element.alignTop
               ]
        )
        [ Element.el [] (Element.text "Either enter your credentials...")
        , Input.text
            [ Element.spacing 12
            ]
            { text = openstackCreds.authUrl
            , placeholder = Just (Input.placeholder [] (Element.text "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"))
            , onChange = \u -> updateCreds { openstackCreds | authUrl = u }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Keystone auth URL")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = openstackCreds.userDomain
            , placeholder = Just (Input.placeholder [] (Element.text "User domain e.g. default"))
            , onChange = \d -> updateCreds { openstackCreds | userDomain = d }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Domain (name or ID)")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = openstackCreds.username
            , placeholder = Just (Input.placeholder [] (Element.text "User name e.g. demo"))
            , onChange = \u -> updateCreds { openstackCreds | username = u }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Name")
            }
        , Input.currentPassword
            [ Element.spacing 12
            ]
            { text = openstackCreds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = \p -> updateCreds { openstackCreds | password = p }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Password")
            }
        ]


loginOpenstackOpenRcEntry : Model -> OSTypes.OpenstackLogin -> Element.Element Msg
loginOpenstackOpenRcEntry model openstackCreds =
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.spacing 15
               , Element.height (Element.fill |> Element.minimum 250)
               ]
        )
        [ Element.paragraph []
            [ Element.text "...or paste an "
            , VH.browserLink model.isElectron "https://docs.openstack.org/newton/install-guide-rdo/keystone-openrc.html" <| View.Types.BrowserLinkTextLabel "OpenRC"
            , Element.text " file"
            ]
        , Input.multiline
            [ Element.width (Element.px 300)
            , Element.height Element.fill
            , Font.size 12
            ]
            { onChange = \o -> InputOpenRc openstackCreds o
            , text = ""
            , placeholder = Nothing
            , label = Input.labelLeft [] Element.none
            , spellcheck = False
            }
        ]


viewLoginJetstream : Model -> JetstreamCreds -> Element.Element Msg
viewLoginJetstream model jetstreamCreds =
    let
        updateCreds : JetstreamCreds -> Msg
        updateCreds newCreds =
            SetNonProjectView <| LoginJetstream newCreds
    in
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2
            (Element.text "Add a Jetstream Cloud Account")
        , jetstreamLoginText model
        , Element.column VH.exoColumnAttributes
            [ Input.text
                [ Element.spacing 12
                ]
                { text = jetstreamCreds.taccUsername
                , placeholder = Just (Input.placeholder [] (Element.text "tg******"))
                , onChange = \un -> updateCreds { jetstreamCreds | taccUsername = un }
                , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Username")
                }
            , Input.currentPassword
                [ Element.spacing 12
                ]
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
                    ]
                , selected = Just jetstreamCreds.jetstreamProviderChoice
                }
            , Element.el (VH.exoPaddingSpacingAttributes ++ [ Element.alignRight ])
                (Button.button
                    [ Modifier.Primary ]
                    (Just (JetstreamLogin jetstreamCreds))
                    "Log In"
                )
            ]
        ]


jetstreamLoginText : Model -> Element.Element Msg
jetstreamLoginText model =
    Element.column VH.exoColumnAttributes
        [ Element.paragraph
            []
            [ Element.text "To use Exosphere with "
            , VH.browserLink model.isElectron "https://jetstream-cloud.org" <| View.Types.BrowserLinkTextLabel "Jetstream Cloud"
            , Element.text ", you need access to a Jetstream allocation. Possible ways to get this:"
            ]
        , Element.paragraph
            []
            [ Element.text "- Request access to the Exosphere Trial Allocation; please create an account on "
            , VH.browserLink model.isElectron "https://portal.xsede.org" <| View.Types.BrowserLinkTextLabel "XSEDE User Portal"
            , Element.text ", then "
            , VH.browserLink model.isElectron "https://gitlab.com/exosphere/exosphere/issues/new" <| View.Types.BrowserLinkTextLabel "create an issue"
            , Element.text " asking for access and providing your XSEDE username."
            ]
        , Element.paragraph
            []
            [ Element.text "- If you know someone else who already has an allocation, they can add you to it. (See \"How do I let other XSEDE accounts use my allocation?\" on "
            , VH.browserLink model.isElectron "https://iujetstream.atlassian.net/wiki/spaces/JWT/pages/537460937/Jetstream+Allocations+FAQ" <| View.Types.BrowserLinkTextLabel "this FAQ"
            , Element.text ")"
            ]
        , Element.paragraph
            []
            [ Element.text "- "
            , VH.browserLink model.isElectron "https://iujetstream.atlassian.net/wiki/spaces/JWT/pages/49184781/Jetstream+Allocations" <| View.Types.BrowserLinkTextLabel "Apply for your own Startup Allocation"
            ]
        , Element.paragraph [] []
        , Element.paragraph
            []
            [ Element.text "Once you have access to an allocation, collect these things:"
            ]
        , Element.paragraph
            []
            [ Element.text "1. TACC username (usually looks like 'tg******'); "
            , VH.browserLink model.isElectron "https://portal.tacc.utexas.edu/password-reset/-/password/forgot-username" <| View.Types.BrowserLinkTextLabel "look up your TACC username"
            ]
        , Element.paragraph
            []
            [ Element.text "2. TACC password; "
            , VH.browserLink model.isElectron "https://portal.tacc.utexas.edu/password-reset/-/password/request-reset" <| View.Types.BrowserLinkTextLabel "set your TACC password"
            ]
        ]
