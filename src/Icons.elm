module Icons exposing (bell, roundRect)

import Element
import Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)


roundRect : String -> Html.Html msg
roundRect color =
    svg
        [ width "60", height "60", viewBox "0 0 60 60" ]
        [ rect [ x "10", y "10", width "50", height "50", rx "15", ry "15", fill color ] [] ]


bell : Int -> Element.Element msg
bell size =
    Element.html <|
        Svg.svg [ Html.Attributes.style "height" (String.fromInt size ++ "px"), viewBox "0 0 448 512" ] [ Svg.path [ fill "currentColor", d "M224 512c35.32 0 63.97-28.65 63.97-64H160.03c0 35.35 28.65 64 63.97 64zm215.39-149.71c-19.32-20.76-55.47-51.99-55.47-154.29 0-77.7-54.48-139.9-127.94-155.16V32c0-17.67-14.32-32-31.98-32s-31.98 14.33-31.98 32v20.84C118.56 68.1 64.08 130.3 64.08 208c0 102.3-36.15 133.53-55.47 154.29-6 6.45-8.66 14.16-8.61 21.71.11 16.4 12.98 32 32.1 32h383.8c19.12 0 32-15.6 32.1-32 .05-7.55-2.61-15.27-8.61-21.71z" ] [] ]
