module Page.LoginOpenstack exposing (EntryType, Model, Msg, defaultCreds, headerView, init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import OpenStack.OpenRc
import OpenStack.Types as OSTypes
import Style.Helpers as SH exposing (spacer)
import Style.Widgets.Button as Button
import Style.Widgets.Link as Link
import Style.Widgets.Text as Text
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { creds : OSTypes.OpenstackLogin
    , openRc : String
    , entryType : EntryType
    }


type EntryType
    = CredsEntry
    | OpenRcEntry


type Msg
    = GotAuthUrl String
    | GotUserDomain String
    | GotUsername String
    | GotPassword String
    | GotOpenRc String
    | GotSelectOpenRcInput
    | GotSelectCredsInput
    | GotProcessOpenRc
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Maybe OSTypes.OpenstackLogin -> Model
init maybeCreds =
    { creds =
        Maybe.withDefault defaultCreds maybeCreds
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
            ( updateCreds model { oldCreds | authUrl = authUrl }, Cmd.none, SharedMsg.NoOp )

        GotUserDomain userDomain ->
            ( updateCreds model { oldCreds | userDomain = userDomain }, Cmd.none, SharedMsg.NoOp )

        GotUsername username ->
            ( updateCreds model { oldCreds | username = username }, Cmd.none, SharedMsg.NoOp )

        GotPassword password ->
            ( updateCreds model { oldCreds | password = password }, Cmd.none, SharedMsg.NoOp )

        GotOpenRc openRc ->
            ( { model | openRc = openRc }, Cmd.none, SharedMsg.NoOp )

        GotSelectOpenRcInput ->
            ( { model | entryType = OpenRcEntry }, Cmd.none, SharedMsg.NoOp )

        GotSelectCredsInput ->
            ( { model | entryType = CredsEntry }, Cmd.none, SharedMsg.NoOp )

        GotProcessOpenRc ->
            let
                newCreds =
                    OpenStack.OpenRc.processOpenRc model.creds model.openRc
            in
            ( { model
                | creds = newCreds
                , entryType = CredsEntry
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


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
    in
    Element.column (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ case model.entryType of
            CredsEntry ->
                loginOpenstackCredsEntry context model allCredsEntered

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
                , label = Input.labelAbove [ Font.size 14 ] (Element.text labelText)
                }
    in
    Element.column
        (VH.formContainer ++ [ Element.spacing spacer.px16 ])
        [ Element.el [] (Element.text "Enter your credentials")
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
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Password")
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
