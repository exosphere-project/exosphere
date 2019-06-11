module Style.Widgets.Card exposing (exoCard)

import Color
import Element exposing (Element)
import Framework.Card as Card
import Framework.Color


exoCard : String -> String -> Element msg -> Element msg
exoCard title subTitle content =
    Card.normal
        { title = title
        , subTitle = subTitle
        , content = content
        , colorBackground = Framework.Color.white
        , colorFont = Framework.Color.black
        , colorFontSecondary = Framework.Color.grey
        , colorBorder = Framework.Color.grey_light
        , colorBorderSecondary = Framework.Color.grey_light
        , colorShadow = Color.rgba 0 0 0 0.05
        , extraAttributes = []
        }
