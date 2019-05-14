module Style.Widgets.Icon exposing (bell, question, remove, roundRect)

import Color
import Element
import Html
import Svg
import Svg.Attributes as SA


bell : Color.Color -> Int -> Element.Element msg
bell cl size =
    Element.el [] <|
        Element.html <|
            bell_ cl size


bell_ : Color.Color -> Int -> Html.Html msg
bell_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 500 500"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M224 512c35.32 0 63.97-28.65 63.97-64H160.03c0 35.35 28.65 64 63.97 64zm215.39-149.71c-19.32-20.76-55.47-51.99-55.47-154.29 0-77.7-54.48-139.9-127.94-155.16V32c0-17.67-14.32-32-31.98-32s-31.98 14.33-31.98 32v20.84C118.56 68.1 64.08 130.3 64.08 208c0 102.3-36.15 133.53-55.47 154.29-6 6.45-8.66 14.16-8.61 21.71.11 16.4 12.98 32 32.1 32h383.8c19.12 0 32-15.6 32.1-32 .05-7.55-2.61-15.27-8.61-21.71z"
            ]
            []
        ]


question : Color.Color -> Int -> Element.Element msg
question cl size =
    Element.el [] <|
        Element.html <|
            question_ cl size


question_ : Color.Color -> Int -> Html.Html msg
question_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 1792 1792"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)

            -- From https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/black/svg/question-circle.svg
            , SA.d "M1024 1376v-192q0-14-9-23t-23-9h-192q-14 0-23 9t-9 23v192q0 14 9 23t23 9h192q14 0 23-9t9-23zm256-672q0-88-55.5-163t-138.5-116-170-41q-243 0-371 213-15 24 8 42l132 100q7 6 19 6 16 0 25-12 53-68 86-92 34-24 86-24 48 0 85.5 26t37.5 59q0 38-20 61t-68 45q-63 28-115.5 86.5t-52.5 125.5v36q0 14 9 23t23 9h192q14 0 23-9t9-23q0-19 21.5-49.5t54.5-49.5q32-18 49-28.5t46-35 44.5-48 28-60.5 12.5-81zm384 192q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"
            ]
            []
        ]


remove : Color.Color -> Int -> Element.Element msg
remove cl size =
    Element.el [] <|
        Element.html <|
            remove_ cl size


remove_ : Color.Color -> Int -> Html.Html msg
remove_ cl size =
    Svg.svg
        [ SA.viewBox "0 0 12 14"
        , SA.height <| String.fromInt size
        ]
        [ Svg.path
            [ SA.fill (Color.colorToHex cl)
            , SA.d "M2 14h8l1-9h-10l1 9zM8 1.996v-1.996h-4v1.996l-4 0.004v3l0.998-1h10.002l1 1v-3.004h-4zM6.986 1.996h-1.986v-0.998h1.986v0.998z"
            ]
            []
        ]


roundRect : Color.Color -> Int -> Element.Element msg
roundRect cl size =
    Element.el [] <|
        Element.html <|
            roundRect_ cl size


roundRect_ : Color.Color -> Int -> Html.Html msg
roundRect_ cl size =
    Svg.svg
        [ SA.width <| String.fromInt size, SA.height <| String.fromInt size, SA.viewBox "0 0 60 60" ]
        [ Svg.rect [ SA.x "10", SA.y "10", SA.width "50", SA.height "50", SA.rx "15", SA.ry "15", SA.fill (Color.colorToHex cl) ] [] ]
