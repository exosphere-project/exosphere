module Page.KeypairCreate exposing (Model, Msg(..), createKeyPairButton, init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Html.Attributes
import Style.Widgets.Button as Button
import Style.Widgets.FormValidation as FormValidation
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
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette) <|
            Element.text <|
                String.join " "
                    [ "Upload"
                    , context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.toTitleCase
                    ]
        , Element.column VH.formContainer
            ([ Input.text
                (VH.inputItemAttributes context.palette.background)
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
                , label = Input.labelAbove [] (Element.text "Name")
                }
             , Input.multiline
                (VH.inputItemAttributes context.palette.background
                    ++ [ Element.width Element.fill
                       , Element.height (Element.px 300)
                       , Element.padding 7
                       , Element.spacing 5
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
                        [ Element.paddingXY 0 10
                        , Font.family [ Font.sansSerif ]
                        , Font.size 17
                        ]
                        (Element.text "Public Key Value")
                , spellcheck = False
                }
             ]
                ++ createKeyPairButton context model
            )
        ]


createKeyPairButton : View.Types.Context -> Model -> List (Element.Element Msg)
createKeyPairButton context model =
    let
        isValid =
            List.all
                identity
                [ String.length model.name > 0
                , String.length model.publicKey > 0
                ]

        ( maybeCmd, validation ) =
            if isValid then
                ( Just GotSubmit
                , Element.none
                )

            else
                ( Nothing
                , FormValidation.renderValidationError context "All fields are required"
                )
    in
    [ Element.el [ Element.alignRight ] <|
        Button.primary
            context.palette
            { text = "Create"
            , onPress = maybeCmd
            }
    , Element.el [ Element.alignRight ] <|
        validation
    ]
