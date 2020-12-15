module View.Settings exposing (settings)

import Element
import Element.Input as Input
import Style.Types
import Types.Types
    exposing
        ( Model
        , Msg(..)
        )
import View.Helpers as VH


settings : Model -> Element.Element Msg
settings model =
    Element.column
        VH.exoColumnAttributes
        [ Element.el VH.heading2 <| Element.text "Settings"
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
                Just model.style.styleMode
            , label = Input.labelAbove [] (Element.text "Color theme")
            }
        ]
