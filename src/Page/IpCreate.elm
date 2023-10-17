module Page.IpCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { name : String
    }


type Msg
    = GotName String
    | GotSubmit


init : Model
init =
    Model ""


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        GotName name ->
            ( { model | name = name }, Cmd.none, SharedMsg.NoOp )

        GotSubmit ->
            ( model
            , Cmd.none
            , SharedMsg.NoOp
            )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context _ _ model =
    let
        renderInvalidReason reason =
            case reason of
                Just r ->
                    r |> Validation.invalidMessage context.palette

                Nothing ->
                    Element.none

        invalidNameReason =
            -- TODO fixme
            Nothing

        invalidValueReason =
            -- TODO fixme
            Nothing
    in
    Element.column
        VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join " "
                [ "FIXME"
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
                                    [ "FIXME" ]
                            )
                        )
                , onChange = GotName
                , label =
                    Input.labelAbove []
                        (Element.text <|
                            String.join " "
                                [ "FIXME" ]
                        )
                }
            , renderInvalidReason invalidNameReason
            ]
        , renderInvalidReason invalidValueReason
        , let
            ( createIp, ipWarnText ) =
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
            [ case ipWarnText of
                Just text ->
                    Element.el [ Font.color <| SH.toElementColor context.palette.danger.textOnNeutralBG ] <| Element.text text

                Nothing ->
                    Element.none
            , Element.el [ Element.alignRight ] <|
                Button.primary
                    context.palette
                    { text = "Create"
                    , onPress = createIp
                    }
            ]
        ]
