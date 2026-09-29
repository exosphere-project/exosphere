module Page.KeypairCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.SshKeyTypeGuesser exposing (KeyTypeGuess(..))
import Helpers.String
import Html.Attributes
import OpenStack.KeypairNameValidator exposing (keypairNameValidator)
import String exposing (trim)
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Forms as Forms exposing (Resource(..))
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
                SharedMsg.RequestCreateKeypair (trim model.name) (trim model.publicKey)
            )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        invalidNameReasons =
            keypairNameValidator model.name

        ( publicKeyFeedback, isPublicKeyAcceptable ) =
            if String.isEmpty (trim model.publicKey) then
                ( Validation.invalidMessage context.palette "Public key is required", False )

            else
                -- Only a private key blocks submission. Nova rejects any public key it cannot load, so a guess that
                -- lags behind a newly supported key type must not stop the upload.
                case Helpers.SshKeyTypeGuesser.guessKeyType model.publicKey of
                    PrivateKey ->
                        ( Validation.invalidMessage context.palette "Private key detected! Enter a public key instead. Public keys are usually found in a .pub file"
                        , False
                        )

                    PublicKey keyType ->
                        let
                            looksValid =
                                String.concat [ "Looks like a valid ", context.localization.pkiPublicKeyForSsh, " (", keyType, ")." ]
                        in
                        -- Nova accepts only the key types its installed pyca/cryptography can load, so support for
                        -- security key types varies by cloud (Jetstream2 has rejected them: "failed to generate fingerprint").
                        if String.startsWith "sk-" keyType then
                            ( Validation.warningMessage context.palette <|
                                String.join " "
                                    [ looksValid
                                    , "Some"
                                    , Helpers.String.pluralize context.localization.openstackWithOwnKeystone
                                    , "might not support security key (sk-) types."
                                    ]
                            , True
                            )

                        else
                            ( Validation.validMessage context.palette looksValid, True )

                    Rfc4716PublicKey ->
                        ( Validation.warningMessage context.palette "This key is in SSH2 (RFC 4716) format, as exported by PuTTY. Convert it to OpenSSH format with: ssh-keygen -i -f <file>"
                        , True
                        )

                    PemPublicKey ->
                        ( Validation.warningMessage context.palette "This key is in PEM format. Convert it to OpenSSH format with: ssh-keygen -i -m PKCS8 -f <file> (use -m PEM for a BEGIN RSA PUBLIC KEY)"
                        , True
                        )

                    Unknown ->
                        ( Validation.warningMessage context.palette "This doesn't look like an OpenSSH public key. Expected a single line starting with the key type, such as ssh-ed25519, as found in a .pub file"
                        , True
                        )
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
             , Text.text Text.Small [] "Use only letters, numbers, spaces, hyphens and underscores."
             ]
                ++ List.map (Validation.invalidMessage context.palette) invalidNameReasons
                ++ Forms.resourceNameAlreadyExists context
                    project
                    currentTime
                    { resource = Keypair model.name, onSuggestionPressed = \suggestion -> GotName suggestion, errorLevel = Validation.Warning }
            )
        , Input.multiline
            (VH.inputItemAttributes context.palette
                ++ [ Element.width Element.fill
                   , Element.height (Element.px 300)
                   , Element.padding spacer.px8
                   , Element.spacing 0
                   , Html.Attributes.style "word-break" "break-all" |> Element.htmlAttribute
                   , Font.family [ Font.monospace ]
                   , Text.fontSize Text.Tiny
                   ]
            )
            { text = model.publicKey
            , placeholder = Just (Input.placeholder [] (Element.text "ssh-rsa ..."))
            , onChange = GotPublicKey
            , label =
                Input.labelAbove
                    [ Element.paddingXY 0 spacer.px12
                    , Font.family [ Font.sansSerif ]
                    , Text.fontSize Text.Body
                    ]
                    (Element.text <|
                        String.join " "
                            [ context.localization.pkiPublicKeyForSsh, "value" ]
                    )
            , spellcheck = False
            }
        , publicKeyFeedback
        , let
            ( createKey, keyWarnText ) =
                case ( invalidNameReasons, isPublicKeyAcceptable ) of
                    ( [], True ) ->
                        ( Just GotSubmit
                        , Nothing
                        )

                    _ ->
                        ( Nothing
                        , Just <| "Please correct the problems above"
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
