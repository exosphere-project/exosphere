module View.Settings exposing (settings)

import Element
import Element.Input as Input
import Style.Types
import Types.Types
    exposing
        ( Msg(..)
        )
import View.Helpers as VH
import View.Types


settings : View.Types.Context -> Style.Types.StyleMode -> Element.Element Msg
settings context styleMode =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette) <| Element.text "Settings"
        , Input.radio
            VH.exoColumnAttributes
            { onChange =
                \newStyleMode ->
                    SetStyle newStyleMode
            , options =
                [ Input.option Style.Types.LightMode (Element.text "Light")
                , Input.option Style.Types.DarkMode (Element.text "Dark")
                ]
            , selected =
                Just styleMode
            , label = Input.labelAbove [] (Element.text "Color theme")
            }
        ]
