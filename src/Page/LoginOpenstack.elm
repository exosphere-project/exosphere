module Page.LoginOpenstack exposing (EntryType, Model, Msg, defaultCreds, headerView, init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.String
import OpenStack.OpenRc
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Link as Link
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { creds : OSTypes.OpenstackLogin
    , appCredentialAuthUrl : OSTypes.KeystoneUrl
    , appCredential : OSTypes.ApplicationCredential
    , openRc : String
    , entryType : EntryType
    }


type EntryType
    = CredsEntry
    | AppCredEntry
    | OpenRcEntry


type Msg
    = GotAuthUrl String
    | GotAppCredAuthUrl String
    | GotUserDomain String
    | GotUsername String
    | GotPassword String
    | GotAppCredentialId String
    | GotAppCredentialSecret String
    | GotOpenRc String
    | GotSelectOpenRcInput
    | GotSelectAppCredInput
    | GotSelectCredsInput
    | GotProcessOpenRc
    | SharedMsg SharedMsg.SharedMsg


init : Maybe OSTypes.OpenstackLogin -> Model
init maybeCreds =
    let
        creds =
            Maybe.withDefault defaultCreds maybeCreds
    in
    { creds = creds
    , appCredentialAuthUrl = creds.authUrl
    , appCredential = defaultAppCredential
    , openRc = ""
    , entryType = CredsEntry
    }


defaultCreds : OSTypes.OpenstackLogin
defaultCreds =
    { authUrl = ""
    , userDomain = ""
    , username = ""
    , password = ""
    }


defaultAppCredential : OSTypes.ApplicationCredential
defaultAppCredential =
    { uuid = ""
    , secret = ""
    }


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    let
        oldCreds =
            model.creds

        updateCreds : Model -> OSTypes.OpenstackLogin -> Model
        updateCreds model_ newCreds =
            { model_ | creds = newCreds }
    in
    case msg of
        GotAuthUrl authUrl ->
            ( { model
                | creds = { oldCreds | authUrl = authUrl }
                , appCredentialAuthUrl = authUrl
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotAppCredAuthUrl authUrl ->
            ( { model
                | creds = { oldCreds | authUrl = authUrl }
                , appCredentialAuthUrl = authUrl
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotUserDomain userDomain ->
            ( updateCreds model { oldCreds | userDomain = userDomain }, Cmd.none, SharedMsg.NoOp )

        GotUsername username ->
            ( updateCreds model { oldCreds | username = username }, Cmd.none, SharedMsg.NoOp )

        GotPassword password ->
            ( updateCreds model { oldCreds | password = password }, Cmd.none, SharedMsg.NoOp )

        GotAppCredentialId appCredId ->
            ( { model
                | appCredential =
                    { uuid = appCredId
                    , secret = model.appCredential.secret
                    }
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotAppCredentialSecret appCredSecret ->
            ( { model
                | appCredential =
                    { uuid = model.appCredential.uuid
                    , secret = appCredSecret
                    }
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotOpenRc openRc ->
            ( { model | openRc = openRc }, Cmd.none, SharedMsg.NoOp )

        GotSelectOpenRcInput ->
            ( { model | entryType = OpenRcEntry }, Cmd.none, SharedMsg.NoOp )

        GotSelectAppCredInput ->
            ( { model | entryType = AppCredEntry }, Cmd.none, SharedMsg.NoOp )

        GotSelectCredsInput ->
            ( { model | entryType = CredsEntry }, Cmd.none, SharedMsg.NoOp )

        GotProcessOpenRc ->
            let
                newCreds =
                    OpenStack.OpenRc.processOpenRc model.creds model.openRc
            in
            ( { model
                | creds = newCreds
                , appCredentialAuthUrl = newCreds.authUrl
                , entryType = CredsEntry
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


headerView : View.Types.Context -> Element.Element msg
headerView context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        "Add an OpenStack Account"


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context _ model =
    let
        allCredsEntered =
            -- These fields must be populated before login can be attempted
            [ model.creds.authUrl
            , model.creds.userDomain
            , model.creds.username
            , model.creds.password
            ]
                |> List.any (\x -> String.isEmpty x)
                |> not

        allAppCredentialFieldsEntered =
            [ model.appCredentialAuthUrl
            , model.appCredential.uuid
            , model.appCredential.secret
            ]
                |> List.any String.isEmpty
                |> not
    in
    Element.column (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ case model.entryType of
            CredsEntry ->
                loginOpenstackCredsEntry context model allCredsEntered

            AppCredEntry ->
                loginOpenstackAppCredEntry context model allAppCredentialFieldsEntered

            OpenRcEntry ->
                loginOpenstackOpenRcEntry context model
        , Element.row
            [ Element.width Element.fill
            , Element.paddingXY 0 spacer.px16 -- so that it looks separate from form fields
            , Element.spacing spacer.px12
            ]
            (case model.entryType of
                CredsEntry ->
                    [ Element.el []
                        (VH.loginPickerButton context
                            |> Element.map SharedMsg
                        )
                    , Button.default
                        context.palette
                        { text = "Use OpenRC File"
                        , onPress = Just GotSelectOpenRcInput
                        }
                    , Button.default
                        context.palette
                        { text = "Use Application Credential"
                        , onPress = Just GotSelectAppCredInput
                        }
                    , Element.el [ Element.alignRight ]
                        (Button.primary
                            context.palette
                            { text = "Log In"
                            , onPress =
                                if allCredsEntered then
                                    Just (SharedMsg <| SharedMsg.RequestUnscopedToken model.creds)

                                else
                                    Nothing
                            }
                        )
                    ]

                AppCredEntry ->
                    [ Element.el []
                        (VH.loginPickerButton context
                            |> Element.map SharedMsg
                        )
                    , Button.default
                        context.palette
                        { text = "Use Username and Password"
                        , onPress = Just GotSelectCredsInput
                        }
                    , Button.default
                        context.palette
                        { text = "Use OpenRC File"
                        , onPress = Just GotSelectOpenRcInput
                        }
                    , Element.el [ Element.alignRight ]
                        (Button.primary
                            context.palette
                            { text = "Log In"
                            , onPress =
                                if allAppCredentialFieldsEntered then
                                    Just
                                        (SharedMsg <|
                                            SharedMsg.RequestProjectScopedTokenWithAppCredential
                                                model.appCredentialAuthUrl
                                                model.appCredential
                                        )

                                else
                                    Nothing
                            }
                        )
                    ]

                OpenRcEntry ->
                    [ Element.el []
                        (Button.default
                            context.palette
                            { text = "Cancel"
                            , onPress = Just GotSelectCredsInput
                            }
                        )
                    , Element.el [ Element.alignRight ]
                        (Button.primary
                            context.palette
                            { text = "Submit"
                            , onPress = Just GotProcessOpenRc
                            }
                        )
                    ]
            )
        ]


loginOpenstackCredsEntry : View.Types.Context -> Model -> Bool -> Element.Element Msg
loginOpenstackCredsEntry context model allCredsEntered =
    let
        creds =
            model.creds

        textField text placeholderText onChange labelText =
            Input.text
                (VH.inputItemAttributes context.palette)
                { text = text
                , placeholder = Just (Input.placeholder [] (Element.text placeholderText))
                , onChange = onChange
                , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text labelText)
                }
    in
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ Element.el [] (Element.text <| "Enter your " ++ Helpers.String.pluralize context.localization.credential)
        , textField
            creds.authUrl
            "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"
            GotAuthUrl
            "Keystone auth URL"
        , textField
            creds.userDomain
            "User domain e.g. default"
            GotUserDomain
            "User Domain (name or ID)"
        , textField
            creds.username
            "User name e.g. demo"
            GotUsername
            "User Name"
        , Input.currentPassword
            (VH.inputItemAttributes context.palette)
            { text = creds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = GotPassword
            , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text "Password")
            }
        , if allCredsEntered then
            Element.none

          else
            Element.el
                [ Element.alignRight
                , Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor)
                ]
                (Element.text "All fields are required.")
        ]


loginOpenstackAppCredEntry : View.Types.Context -> Model -> Bool -> Element.Element Msg
loginOpenstackAppCredEntry context model allAppCredentialFieldsEntered =
    let
        textField text placeholderText onChange labelText =
            Input.text
                (VH.inputItemAttributes context.palette)
                { text = text
                , placeholder = Just (Input.placeholder [] (Element.text placeholderText))
                , onChange = onChange
                , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text labelText)
                }
    in
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ Element.el [] (Element.text "Enter your application credential.")
        , textField
            model.appCredentialAuthUrl
            "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"
            GotAppCredAuthUrl
            "Keystone auth URL"
        , textField
            model.appCredential.uuid
            "Application credential ID"
            GotAppCredentialId
            "Application Credential ID"
        , Input.currentPassword
            (VH.inputItemAttributes context.palette)
            { text = model.appCredential.secret
            , placeholder = Just (Input.placeholder [] (Element.text "Application credential secret"))
            , show = False
            , onChange = GotAppCredentialSecret
            , label = Input.labelAbove [ Text.fontSize Text.Small ] (Element.text "Application Credential Secret")
            }
        , if allAppCredentialFieldsEntered then
            Element.none

          else
            Element.el
                [ Element.alignRight
                , Font.color (context.palette.danger.textOnNeutralBG |> SH.toElementColor)
                ]
                (Element.text "All fields are required.")
        ]


loginOpenstackOpenRcEntry : View.Types.Context -> Model -> Element.Element Msg
loginOpenstackOpenRcEntry context model =
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px12 ])
        [ Element.paragraph []
            [ Element.text "Paste an "
            , Link.externalLink
                context.palette
                "https://docs.openstack.org/newton/install-guide-rdo/keystone-openrc.html"
                "OpenRC"
            , Element.text " file"
            ]
        , Input.multiline
            (VH.inputItemAttributes context.palette
                ++ [ Element.width Element.fill
                   , Element.height (Element.px 250)
                   , Text.fontSize Text.Tiny
                   ]
            )
            { onChange = GotOpenRc
            , text = model.openRc
            , placeholder = Nothing
            , label = Input.labelHidden "Paste an OpenRC file"
            , spellcheck = False
            }
        ]
