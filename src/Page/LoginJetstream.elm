module Page.LoginJetstream exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Route
import Style.Helpers as SH
import Types.HelperTypes exposing (JetstreamCreds, JetstreamProvider(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    JetstreamCreds


type Msg
    = GotUsername String
    | GotPassword String
    | GotProviderChoice JetstreamProvider
    | SharedMsg SharedMsg.SharedMsg


init : Model
init =
    defaultJetstreamCreds


defaultJetstreamCreds : JetstreamCreds
defaultJetstreamCreds =
    { jetstreamProviderChoice = BothJetstreamClouds
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
            ( { model | jetstreamProviderChoice = choice }, Cmd.none, SharedMsg.NoOp )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context _ model =
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette)
            (Element.text "Add a Jetstream Cloud Account")
        , Element.column VH.contentContainer
            [ helpText context
            , Element.column VH.formContainer
                [ Input.text
                    (VH.inputItemAttributes context.palette.background)
                    { text = model.taccUsername
                    , placeholder = Just (Input.placeholder [] (Element.text "tg******"))
                    , onChange = GotUsername
                    , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Username")
                    }
                , Input.currentPassword
                    (VH.inputItemAttributes context.palette.background)
                    { text = model.taccPassword
                    , placeholder = Nothing
                    , onChange = GotPassword
                    , label = Input.labelAbove [ Font.size 14 ] (Element.text "TACC Password")
                    , show = False
                    }
                , Input.radio []
                    { label = Input.labelAbove [] (Element.text "Provider")
                    , onChange = GotProviderChoice
                    , options =
                        [ Input.option IUCloud (Element.text "IU Cloud")
                        , Input.option TACCCloud (Element.text "TACC Cloud")
                        , Input.option BothJetstreamClouds (Element.text "Both Clouds")
                        ]
                    , selected = Just model.jetstreamProviderChoice
                    }
                , Element.row [ Element.width Element.fill ]
                    [ Element.el [] (loginPickerButton context)
                    , Element.el [ Element.alignRight ]
                        (Widget.textButton
                            (SH.materialStyle context.palette).primaryButton
                            { text = "Log In"
                            , onPress =
                                Just <| SharedMsg <| SharedMsg.JetstreamLogin model
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


loginPickerButton : View.Types.Context -> Element.Element Msg
loginPickerButton context =
    Widget.textButton
        (SH.materialStyle context.palette).button
        { text = "Other Login Methods"
        , onPress =
            Just <| SharedMsg <| SharedMsg.NavigateToView Route.LoginPicker
        }
