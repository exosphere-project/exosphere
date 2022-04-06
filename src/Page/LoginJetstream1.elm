module Page.LoginJetstream1 exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Style.Helpers as SH
import Style.Widgets.Link as Link
import Style.Widgets.Text as Text
import Types.HelperTypes exposing (Jetstream1Creds, Jetstream1Provider(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    Jetstream1Creds


type Msg
    = GotUsername String
    | GotPassword String
    | GotProviderChoice Jetstream1Provider
    | SharedMsg SharedMsg.SharedMsg


init : Maybe Jetstream1Creds -> Model
init maybeCreds =
    Maybe.withDefault
        defaultJetstream1Creds
        maybeCreds


defaultJetstream1Creds : Jetstream1Creds
defaultJetstream1Creds =
    { jetstream1ProviderChoice = BothJetstream1Clouds
    , taccUsername = ""
    , taccPassword = ""
    }


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        GotUsername username ->
            ( { model | taccUsername = username }, Cmd.none, SharedMsg.NoOp )

        GotPassword password ->
            ( { model | taccPassword = password }, Cmd.none, SharedMsg.NoOp )

        GotProviderChoice choice ->
            ( { model | jetstream1ProviderChoice = choice }, Cmd.none, SharedMsg.NoOp )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context _ model =
    let
        renderInvalidReasons value inputName =
            if String.isEmpty value then
                VH.invalidInputHelperText context.palette (inputName ++ " is required")

            else
                Element.none
    in
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Text.heading context.palette [] Element.none "Add a Jetstream1 Account"
        , Element.column VH.contentContainer
            [ helpText context
            , Element.column VH.formContainer
                [ Input.text
                    (VH.inputItemAttributes context.palette.background)
                    { text = model.taccUsername
                    , placeholder = Just (Input.placeholder [] (Element.text "tg******"))
                    , onChange = GotUsername
                    , label = Input.labelAbove [ Font.size 14 ] (VH.requiredLabel context.palette (Element.text "TACC Username"))
                    }
                , renderInvalidReasons model.taccUsername "TACC Username"
                , Input.currentPassword
                    (VH.inputItemAttributes context.palette.background)
                    { text = model.taccPassword
                    , placeholder = Nothing
                    , onChange = GotPassword
                    , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Password")
                    , show = False
                    }
                , renderInvalidReasons model.taccPassword "TACC Password"
                , Input.radio []
                    { label = Input.labelAbove [] (Element.text "Provider")
                    , onChange = GotProviderChoice
                    , options =
                        [ Input.option IUCloud (Element.text "IU Cloud")
                        , Input.option TACCCloud (Element.text "TACC Cloud")
                        , Input.option BothJetstream1Clouds (Element.text "Both Clouds")
                        ]
                    , selected = Just model.jetstream1ProviderChoice
                    }
                , Element.row [ Element.width Element.fill ]
                    [ Element.el []
                        (VH.loginPickerButton context
                            |> Element.map SharedMsg
                        )
                    , let
                        onPress =
                            if String.isEmpty model.taccUsername || String.isEmpty model.taccPassword then
                                Nothing

                            else
                                Just <| SharedMsg <| SharedMsg.Jetstream1Login model
                      in
                      Element.el [ Element.alignRight ]
                        (Widget.textButton
                            (SH.materialStyle context.palette).primaryButton
                            { text = "Log In"
                            , onPress = onPress
                            }
                        )
                    ]
                ]
            ]
        ]


helpText : View.Types.Context -> Element.Element Msg
helpText context =
    Element.column VH.exoColumnAttributes
        [ Element.paragraph
            []
            [ Element.text "To use Exosphere with "
            , Link.externalLink
                context.palette
                "https://jetstream-cloud.org"
                "Jetstream1"
            , Element.text ", you need access to a Jetstream1 allocation. Possible ways to get this:"
            ]
        , Element.paragraph
            []
            [ Element.text "- Request access to the Exosphere Trial Allocation; please create an account on "
            , Link.externalLink
                context.palette
                "https://portal.xsede.org"
                "XSEDE User Portal"
            , Element.text ", then "
            , Link.externalLink
                context.palette
                "https://gitlab.com/exosphere/exosphere/issues/new"
                "create an issue"
            , Element.text " asking for access and providing your XSEDE username."
            ]
        , Element.paragraph
            []
            [ Element.text "- If you know someone else who already has an allocation, they can add you to it. (See \"How do I let other XSEDE accounts use my allocation?\" on "
            , Link.externalLink
                context.palette
                "https://iujetstream.atlassian.net/wiki/spaces/JWT/pages/537460937/Jetstream+Allocations+FAQ"
                "this FAQ"
            , Element.text ")"
            ]
        , Element.paragraph
            []
            [ Element.text "- "
            , Link.externalLink
                context.palette
                "https://iujetstream.atlassian.net/wiki/spaces/JWT/pages/49184781/Jetstream+Allocations"
                "Apply for your own Startup Allocation"
            ]
        , Element.paragraph [] []
        , Element.paragraph
            []
            [ Element.text "Once you have access to an allocation, collect these things:"
            ]
        , Element.paragraph
            []
            [ Element.text "1. TACC username (usually looks like 'tg******'); "
            , Link.externalLink
                context.palette
                "https://portal.tacc.utexas.edu/password-reset/-/password/forgot-username"
                "look up your TACC username"
            ]
        , Element.paragraph
            []
            [ Element.text "2. TACC password; "
            , Link.externalLink
                context.palette
                "https://portal.tacc.utexas.edu/password-reset/-/password/request-reset"
                "set your TACC password"
            ]
        ]
