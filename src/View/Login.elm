module View.Login exposing (viewLogin)

import Element
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Modifier as Modifier
import Types.Types exposing (..)
import View.Helpers as VH


viewLogin : Model -> Element.Element Msg
viewLogin model =
    Element.column VH.exoColumnAttributes
        [ Element.el
            VH.heading2
            (Element.text "Add an OpenStack Account")
        , Element.wrappedRow
            VH.exoRowAttributes
            [ viewLoginCredsEntry model
            , viewLoginOpenRcEntry model
            ]
        , Element.el (VH.exoPaddingSpacingAttributes ++ [ Element.alignRight ])
            (Button.button
                [ Modifier.Primary ]
                (Just RequestNewProjectToken)
                "Log In"
            )
        ]


viewLoginCredsEntry : Model -> Element.Element Msg
viewLoginCredsEntry model =
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
            { text = model.creds.authUrl
            , placeholder = Just (Input.placeholder [] (Element.text "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"))
            , onChange = \u -> InputLoginField (AuthUrl u)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Keystone auth URL")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.projectDomain
            , placeholder = Just (Input.placeholder [] (Element.text "OS_PROJECT_DOMAIN_ID e.g. default"))
            , onChange = \d -> InputLoginField (ProjectDomain d)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Project Domain (name or ID)")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.projectName
            , placeholder = Just (Input.placeholder [] (Element.text "Project name e.g. demo"))
            , onChange = \pn -> InputLoginField (ProjectName pn)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Project Name")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.userDomain
            , placeholder = Just (Input.placeholder [] (Element.text "User domain e.g. default"))
            , onChange = \d -> InputLoginField (UserDomain d)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Domain (name or ID)")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.username
            , placeholder = Just (Input.placeholder [] (Element.text "User name e.g. demo"))
            , onChange = \u -> InputLoginField (Username u)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Name")
            }
        , Input.currentPassword
            [ Element.spacing 12
            ]
            { text = model.creds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = \p -> InputLoginField (Password p)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Password")
            }
        ]


viewLoginOpenRcEntry : Model -> Element.Element Msg
viewLoginOpenRcEntry model =
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
            { onChange = \o -> InputLoginField (OpenRc o)
            , text = ""
            , placeholder = Nothing
            , label = Input.labelLeft [] Element.none
            , spellcheck = False
            }
        ]
