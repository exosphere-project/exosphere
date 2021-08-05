module View.LoginOpenstack exposing (viewLoginOpenstack)

import Element
import Element.Font as Font
import Element.Input as Input
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Types.Msg exposing (SharedMsg(..))
import Types.View
    exposing
        ( JetstreamProvider(..)
        , LoginView(..)
        , NonProjectViewConstructor(..)
        , OpenstackLoginFormEntryType(..)
        , OpenstackLoginViewParams
        )
import View.Helpers as VH
import View.Login exposing (loginPickerButton)
import View.Types
import Widget


viewLoginOpenstack : View.Types.Context -> OpenstackLoginViewParams -> Element.Element SharedMsg
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
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el
            (VH.heading2 context.palette)
            (Element.text "Add an OpenStack Account")
        , Element.column VH.formContainer
            [ case viewParams.formEntryType of
                LoginViewCredsEntry ->
                    loginOpenstackCredsEntry context viewParams allCredsEntered

                LoginViewOpenRcEntry ->
                    loginOpenstackOpenRcEntry context viewParams
            , Element.row (VH.exoRowAttributes ++ [ Element.width Element.fill ])
                (case viewParams.formEntryType of
                    LoginViewCredsEntry ->
                        [ Element.el [] (loginPickerButton context)
                        , Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Use OpenRC File"
                            , onPress = Just <| SetNonProjectView <| Login <| LoginOpenstack { viewParams | formEntryType = LoginViewOpenRcEntry }
                            }
                        , Element.el [ Element.alignRight ]
                            (Widget.textButton
                                (SH.materialStyle context.palette).primaryButton
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
                                (SH.materialStyle context.palette).button
                                { text = "Cancel"
                                , onPress = Just <| SetNonProjectView <| Login <| LoginOpenstack { viewParams | formEntryType = LoginViewCredsEntry }
                                }
                            )
                        , Element.el (VH.exoPaddingSpacingAttributes ++ [ Element.alignRight ])
                            (Widget.textButton
                                (SH.materialStyle context.palette).primaryButton
                                { text = "Submit"
                                , onPress = Just <| SubmitOpenRc viewParams.creds viewParams.openRc
                                }
                            )
                        ]
                )
            ]
        ]


loginOpenstackCredsEntry : View.Types.Context -> OpenstackLoginViewParams -> Bool -> Element.Element SharedMsg
loginOpenstackCredsEntry context viewParams allCredsEntered =
    let
        creds =
            viewParams.creds

        updateCreds : OSTypes.OpenstackLogin -> SharedMsg
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
        VH.formContainer
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


loginOpenstackOpenRcEntry : View.Types.Context -> OpenstackLoginViewParams -> Element.Element SharedMsg
loginOpenstackOpenRcEntry context viewParams =
    Element.column
        VH.formContainer
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
                ++ [ Element.width Element.fill
                   , Element.height (Element.px 250)
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
