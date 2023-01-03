module Page.KeypairCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.SshKeyTypeGuesser
import Helpers.String
import Html.Attributes
import String exposing (trim)
import Style.Helpers as SH exposing (spacer)
import Style.Widgets.Button as Button
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { name : String
    , publicKey : String
    }


type Msg
    = GotName String
    | GotPublicKey String
    | GotSubmit


init : Model
init =
    Model "" ""


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotName name ->
            ( { model | name = name }, Cmd.none, SharedMsg.NoOp )

        GotPublicKey publicKey ->
            ( { model | publicKey = publicKey }, Cmd.none, SharedMsg.NoOp )

        GotSubmit ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestCreateKeypair (trim model.name) model.publicKey
            )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        nameExists =
            VH.sshKeyNameExists project model.name

        renderNameExists =
            if nameExists then
                [ Validation.warningMessage context.palette (VH.sshKeyNameExistsMessage context) ]

            else
                []

        nameSuggestionButtons =
            let
                suggestedNames =
                    VH.resourceNameSuggestions currentTime project model.name
                        |> List.filter (\n -> not (VH.sshKeyNameExists project n))

                suggestionButtons =
                    suggestedNames
                        |> List.map
                            (\name ->
                                Button.default
                                    context.palette
                                    { text = name
                                    , onPress = Just (GotName name)
                                    }
                            )
            in
            if nameExists then
                [ Element.row
                    [ Element.spacing spacer.px8 ]
                    suggestionButtons
                ]

            else
                [ Element.none ]

        renderInvalidReason reason =
            case reason of
                Just r ->
                    r |> Validation.invalidMessage context.palette

                Nothing ->
                    Element.none

        invalidNameReason =
            if String.isEmpty model.name then
                Just "Name is required"

            else if String.left 1 model.name == " " then
                Just "Name cannot start with a space"

            else
                Nothing

        keyTypeGuess =
            Helpers.SshKeyTypeGuesser.guessKeyType model.publicKey

        invalidValueReason =
            if String.isEmpty model.publicKey then
                Just "Public key is required"

            else if keyTypeGuess == Helpers.SshKeyTypeGuesser.PrivateKey then
                Just "Private key detected! Enter a public key instead. Public keys are usually found in a .pub file"

            else
                Nothing
    in
    Element.column
        VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join " "
                [ "Upload"
                , context.localization.pkiPublicKeyForSsh
                    |> Helpers.String.toTitleCase
                ]
            )
        , Element.column [ Element.spacing spacer.px16, Element.width Element.fill ]
            ([ Input.text
                (VH.inputItemAttributes context.palette)
                { text = model.name
                , placeholder =
                    Just
                        (Input.placeholder []
                            (Element.text <|
                                String.join " "
                                    [ "My", context.localization.pkiPublicKeyForSsh ]
                            )
                        )
                , onChange = GotName
                , label =
                    Input.labelAbove []
                        (Element.text <|
                            String.join " "
                                [ context.localization.pkiPublicKeyForSsh, "name" ]
                        )
                }
             , renderInvalidReason invalidNameReason
             ]
                ++ renderNameExists
                ++ nameSuggestionButtons
            )
        , Input.multiline
            (VH.inputItemAttributes context.palette
                ++ [ Element.width Element.fill
                   , Element.height (Element.px 300)
                   , Element.padding spacer.px8
                   , Element.spacing 0
                   , Html.Attributes.style "word-break" "break-all" |> Element.htmlAttribute
                   , Font.family [ Font.monospace ]
                   , Font.size 12
                   ]
            )
            { text = model.publicKey
            , placeholder = Just (Input.placeholder [] (Element.text "ssh-rsa ..."))
            , onChange = GotPublicKey
            , label =
                Input.labelAbove
                    [ Element.paddingXY 0 spacer.px12
                    , Font.family [ Font.sansSerif ]
                    , Font.size 17
                    ]
                    (Element.text <|
                        String.join " "
                            [ context.localization.pkiPublicKeyForSsh, "value" ]
                    )
            , spellcheck = False
            }
        , renderInvalidReason invalidValueReason
        , let
            ( createKey, keyWarnText ) =
                case ( invalidNameReason, invalidValueReason ) of
                    ( Nothing, Nothing ) ->
                        ( Just GotSubmit
                        , Nothing
                        )

                    _ ->
                        ( Nothing
                        , Just <| "All fields are required"
                        )
          in
          Element.row [ Element.width Element.fill ]
            [ case keyWarnText of
                Just text ->
                    Element.el [ Font.color <| SH.toElementColor context.palette.danger.textOnNeutralBG ] <| Element.text text

                Nothing ->
                    Element.none
            , Element.el [ Element.alignRight ] <|
                Button.primary
                    context.palette
                    { text = "Create"
                    , onPress = createKey
                    }
            ]
        ]
