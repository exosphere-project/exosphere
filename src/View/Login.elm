module View.Login exposing (viewLoginJetstream, viewLoginOpenstack, viewLoginPicker)

import Element
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Modifier as Modifier
import Types.Types exposing (JetstreamCreds, JetstreamLoginField(..), JetstreamProvider(..), Model, Msg(..), NonProjectViewConstructor(..), OpenstackCreds, OpenstackLoginField(..))
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
                                (OpenstackCreds "" "" "" "" "" "")
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


viewLoginOpenstack : Model -> OpenstackCreds -> Element.Element Msg
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


loginOpenstackCredsEntry : Model -> OpenstackCreds -> Element.Element Msg
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


loginOpenstackOpenRcEntry : Model -> OpenstackCreds -> Element.Element Msg
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
        , Element.column VH.exoColumnAttributes
            [ Input.text
                [ Element.spacing 12
                ]
                { text = jetstreamCreds.jetstreamProjectName
                , placeholder = Just (Input.placeholder [] (Element.text "TG-whatever"))
                , onChange = \pn -> InputJetstreamLoginField jetstreamCreds (JetstreamProjectName pn)
                , label = Input.labelAbove [ Font.size 14 ] (Element.text "Project/Allocation Name")
                }
            , Input.text
                [ Element.spacing 12
                ]
                { text = jetstreamCreds.taccUsername
                , placeholder = Nothing
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
