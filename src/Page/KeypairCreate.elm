module Page.KeypairCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Html.Attributes
import List
import Style.Helpers as SH exposing (spacer)
import Style.Widgets.Button as Button
import Style.Widgets.Text as Text
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
                SharedMsg.RequestCreateKeypair model.name model.publicKey
            )


view : View.Types.Context -> Model -> Element.Element Msg
view context model =
    let
        uppercasePublicKey : String
        uppercasePublicKey =
            String.toUpper model.publicKey

        pemPrivateKeyHeaders : List String
        pemPrivateKeyHeaders =
            [ "-----BEGIN OPENSSH PRIVATE KEY-----"
            , "-----END OPENSSH PRIVATE KEY-----"
            , "-----BEGIN PRIVATE KEY-----"
            , "-----END PRIVATE KEY-----"
            ]

        renderInvalidReasonsFunction reason condition =
            reason |> VH.invalidInputHelperText context.palette |> VH.renderIf condition

        ( renderInvalidKeyNameReason, isKeyNameValid ) =
            if String.isEmpty model.name then
                ( renderInvalidReasonsFunction "Name is required" True, False )

            else if String.left 1 model.name == " " then
                ( renderInvalidReasonsFunction "Name cannot start with a space" True, False )

            else if String.right 1 model.name == " " then
                ( renderInvalidReasonsFunction "Name cannot end with a space" True, False )

            else
                ( Element.none, True )

        ( renderInvalidKeyValueReason, isKeyValueValid ) =
            if String.isEmpty model.publicKey then
                ( renderInvalidReasonsFunction "Public key is required" True, False )

            else if List.map (\s -> String.contains s uppercasePublicKey) pemPrivateKeyHeaders |> List.any (\n -> n && True) then
                ( renderInvalidReasonsFunction "Private key detected! Enter a public key instead. Public keys are usually found in a .pub file" True, False )

            else
                ( Element.none, True )
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
            [ Input.text
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
            , renderInvalidKeyNameReason
            ]
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
        , renderInvalidKeyValueReason
        , let
            ( createKey, keyWarnText ) =
                if isKeyNameValid && isKeyValueValid then
                    ( Just GotSubmit
                    , Nothing
                    )

                else
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
