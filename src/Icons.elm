module Icons exposing (roundRect)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


roundRect : String -> Html.Html msg
roundRect color =
    svg
        [ width "60", height "60", viewBox "0 0 60 60" ]
        [ rect [ x "10", y "10", width "50", height "50", rx "15", ry "15", fill color ] [] ]
