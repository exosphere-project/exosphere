module View.Login exposing (viewLoginJetstream, viewLoginOpenstack, viewLoginPicker)

import Element
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Modifier as Modifier
import OpenStack.Types as OSTypes
import Types.Types exposing (JetstreamCreds, JetstreamLoginField(..), JetstreamProvider(..), Model, Msg(..), NonProjectViewConstructor(..), OpenstackLoginField(..))
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
                (Just <| RequestNewProjectToken openstackCreds)
                "Log In"
            )
        ]


loginOpenstackCredsEntry : Model -> OSTypes.OpenstackLogin -> Element.Element Msg
loginOpenstackCredsEntry _ openstackCreds =
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
            , onChange = \u -> InputOpenstackLoginField openstackCreds (AuthUrl u)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Keystone auth URL")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = openstackCreds.projectDomain
            , placeholder = Just (Input.placeholder [] (Element.text "OS_PROJECT_DOMAIN_ID e.g. default"))
            , onChange = \d -> InputOpenstackLoginField openstackCreds (ProjectDomain d)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Project Domain (name or ID)")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = openstackCreds.projectName
            , placeholder = Just (Input.placeholder [] (Element.text "Project name e.g. demo"))
            , onChange = \pn -> InputOpenstackLoginField openstackCreds (ProjectName pn)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Project Name")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = openstackCreds.userDomain
            , placeholder = Just (Input.placeholder [] (Element.text "User domain e.g. default"))
            , onChange = \d -> InputOpenstackLoginField openstackCreds (UserDomain d)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Domain (name or ID)")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = openstackCreds.username
            , placeholder = Just (Input.placeholder [] (Element.text "User name e.g. demo"))
            , onChange = \u -> InputOpenstackLoginField openstackCreds (Username u)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Name")
            }
        , Input.currentPassword
            [ Element.spacing 12
            ]
            { text = openstackCreds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = \p -> InputOpenstackLoginField openstackCreds (Password p)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Password")
            }
        ]


loginOpenstackOpenRcEntry : Model -> OSTypes.OpenstackLogin -> Element.Element Msg
loginOpenstackOpenRcEntry _ openstackCreds =
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.spacing 15
               , Element.height (Element.fill |> Element.minimum 250)
               ]
        )
        [ Element.paragraph []
            [ Element.text "...or paste an "

            {-
               Todo this link opens in Electron, should open in user's browser
               https://github.com/electron/electron/blob/master/docs/api/shell.md#shellopenexternalurl-options-callback
            -}
            , Element.link []
                { url = "https://docs.openstack.org/newton/install-guide-rdo/keystone-openrc.html"
                , label = Element.text "OpenRC"
                }
            , Element.text " file"
            ]
        , Input.multiline
            [ Element.width (Element.px 300)
            , Element.height Element.fill
            , Font.size 12
            ]
            { onChange = \o -> InputOpenstackLoginField openstackCreds (OpenRc o)
            , text = ""
            , placeholder = Nothing
            , label = Input.labelLeft [] Element.none
            , spellcheck = False
            }
        ]


viewLoginJetstream : Model -> JetstreamCreds -> Element.Element Msg
viewLoginJetstream model jetstreamCreds =
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2
            (Element.text "Add a Jetstream Cloud Account")
        , jetstreamLoginText model
        , Element.column VH.exoColumnAttributes
            [ Input.text
                [ Element.spacing 12
                ]
                { text = jetstreamCreds.jetstreamProjectName
                , placeholder = Just (Input.placeholder [] (Element.text "TG-******"))
                , onChange = \pn -> InputJetstreamLoginField jetstreamCreds (JetstreamProjectName pn)
                , label = Input.labelAbove [ Font.size 14 ] (Element.text "Allocation Name")
                }
            , Input.text
                [ Element.spacing 12
                ]
                { text = jetstreamCreds.taccUsername
                , placeholder = Just (Input.placeholder [] (Element.text "tg******"))
                , onChange = \un -> InputJetstreamLoginField jetstreamCreds (TaccUsername un)
                , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Username")
                }
            , Input.currentPassword
                [ Element.spacing 12
                ]
                { text = jetstreamCreds.taccPassword
                , placeholder = Nothing
                , onChange = \pw -> InputJetstreamLoginField jetstreamCreds (TaccPassword pw)
                , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Password")
                , show = False
                }
            , Input.radio []
                { label = Input.labelAbove [] (Element.text "Provider")
                , onChange = \x -> InputJetstreamLoginField jetstreamCreds (JetstreamProviderChoice x)
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
            [ Element.text "1. Your allocation name (begins with `TG-`); log into "
            , VH.browserLink model.isElectron "https://portal.xsede.org" <| View.Types.BrowserLinkTextLabel "XSEDE User Portal"
            , Element.text " and see it in your "
            , VH.browserLink model.isElectron "https://portal.xsede.org/group/xup/allocations/usage" <| View.Types.BrowserLinkTextLabel "allocations"
            ]
        , Element.paragraph
            []
            [ Element.text "2. TACC username (usually looks like 'tg******'); "
            , VH.browserLink model.isElectron "https://portal.tacc.utexas.edu/password-reset/-/password/forgot-username" <| View.Types.BrowserLinkTextLabel "look up your TACC username"
            ]
        , Element.paragraph
            []
            [ Element.text "3. TACC password; "
            , VH.browserLink model.isElectron "https://portal.tacc.utexas.edu/password-reset/-/password/request-reset" <| View.Types.BrowserLinkTextLabel "set your TACC password"
            ]
        ]
