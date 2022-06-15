module Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput, toMaybe)

import Element
import Element.Font as Font
import Element.Input as Input
import Style.Helpers as SH
import Style.Types
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..), NumericTextInputParams)


numericTextInput : Style.Types.ExoPalette -> List (Element.Attribute msg) -> NumericTextInput -> NumericTextInputParams -> (NumericTextInput -> msg) -> Element.Element msg
numericTextInput palette attribs currentVal params onchangeFunc =
    let
        currentValStr =
            case currentVal of
                ValidNumericTextInput i ->
                    String.fromInt i

                InvalidNumericTextInput s ->
                    s

        runValidators : String -> ( NumericTextInput, Maybe String )
        runValidators s =
            let
                tooLarge : Int -> Maybe String
                tooLarge v =
                    Maybe.andThen
                        (\max ->
                            if v > max then
                                Just ("Input is too large, must be " ++ String.fromInt max ++ " or smaller")

                            else
                                Nothing
                        )
                        params.maxVal

                tooSmall : Int -> Maybe String
                tooSmall v =
                    Maybe.andThen
                        (\min ->
                            if v < min then
                                Just ("Input is too small, must be " ++ String.fromInt min ++ " or larger")

                            else
                                Nothing
                        )
                        params.minVal
            in
            case String.toInt s of
                Just i ->
                    case tooLarge i of
                        Just reason ->
                            ( InvalidNumericTextInput s, Just reason )

                        Nothing ->
                            case tooSmall i of
                                Just reason_ ->
                                    ( InvalidNumericTextInput s, Just reason_ )

                                Nothing ->
                                    ( ValidNumericTextInput i, Nothing )

                Nothing ->
                    ( InvalidNumericTextInput s, Just "Input must be a whole number." )

        textInput =
            Input.text
                attribs
                { text = currentValStr
                , placeholder =
                    Maybe.map
                        (\v -> Input.placeholder [] (Element.text <| String.fromInt v))
                        params.defaultVal
                , onChange = \v -> runValidators v |> Tuple.first |> onchangeFunc
                , label = Input.labelAbove [] (Element.text params.labelText)
                }

        warnText =
            case runValidators currentValStr |> Tuple.second of
                Nothing ->
                    Element.none

                Just reason ->
                    Element.el
                        [ Font.color <| SH.toElementColor palette.danger.textOnNeutralBG ]
                        (Element.text reason)
    in
    Element.column
        [ Element.padding 10
        , Element.spacing 10
        ]
        [ textInput
        , warnText
        ]


toMaybe : NumericTextInput -> Maybe Int
toMaybe numericTextInput_ =
    case numericTextInput_ of
        ValidNumericTextInput i ->
            Just i

        InvalidNumericTextInput _ ->
            Nothing
