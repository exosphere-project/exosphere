module Style.Widgets.NumericPicker.NumericPicker exposing (numericPicker)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Style.Widgets.NumericPicker.Types exposing (NumericPickerParams, NumericTextInput(..))


numericPicker : NumericTextInput -> NumericPickerParams -> (NumericTextInput -> msg) -> Element.Element msg
numericPicker currentVal params onchangeFunc =
    -- TODO input is not valid if currentVal exceeds maxVal or is exceeded by minVal?
    let
        betweenMinMax : Int -> Bool
        betweenMinMax v =
            (params.minVal < v) || (v < params.maxVal)

        currentValStr =
            case currentVal of
                ValidNumericTextInput i ->
                    String.fromInt i

                InvalidNumericTextInput s ->
                    s

        isValid : String -> NumericTextInput
        isValid s =
            case String.toInt s of
                Just i ->
                    ValidNumericTextInput i

                Nothing ->
                    InvalidNumericTextInput s

        textInput =
            Input.text
                []
                { text = currentValStr
                , placeholder =
                    case params.defaultVal of
                        Just v ->
                            Just <| Input.placeholder [] (Element.text <| String.fromInt v)

                        Nothing ->
                            Nothing
                , onChange = \v -> isValid v |> onchangeFunc
                , label = Input.labelAbove [] (Element.text params.labelText)
                }

        warnText =
            case currentVal of
                ValidNumericTextInput _ ->
                    Element.none

                InvalidNumericTextInput _ ->
                    Element.el
                        [ Font.color <| Element.rgb255 255 56 96 ]
                        (Element.text "Input must be an integer.")

        slider =
            let
                sliderVal =
                    case currentVal of
                        ValidNumericTextInput v ->
                            if betweenMinMax v then
                                toFloat v

                            else
                                toFloat params.minVal

                        InvalidNumericTextInput _ ->
                            case params.defaultVal of
                                Just d ->
                                    toFloat d

                                Nothing ->
                                    toFloat params.minVal
            in
            Input.slider
                [ Element.height (Element.px 30)
                , Element.width (Element.px 100 |> Element.minimum 200)

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color (Element.rgb 0.5 0.5 0.5)
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = \c -> round c |> ValidNumericTextInput |> onchangeFunc
                , label = Input.labelHidden (currentValStr ++ " GB")
                , min = toFloat params.minVal
                , max = toFloat params.maxVal
                , step = Just 1
                , value = sliderVal
                , thumb =
                    Input.defaultThumb
                }
    in
    Element.column
        [ Element.padding 10
        , Element.spacing 10
        ]
        [ textInput
        , warnText
        , slider
        ]
