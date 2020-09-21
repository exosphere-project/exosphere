module Style.Widgets.NumericPicker.NumericPicker exposing (numericPicker)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Style.Widgets.NumericPicker.Types exposing (NumericTextInput)


numericPicker : Int -> ( Int, Int ) -> (Int -> msg) -> Element.Element msg
numericPicker currentVal ( minVal, maxVal ) onchangeFunc =
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
        { onChange = \c -> round c |> onchangeFunc
        , label = Input.labelHidden (String.fromInt currentVal ++ " GB")
        , min = toFloat minVal
        , max = toFloat maxVal
        , step = Just 1
        , value = toFloat currentVal
        , thumb =
            Input.defaultThumb
        }
