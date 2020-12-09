module View.Settings exposing (settings)

import Element
import Element.Input as Input
import Style.Types
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , Style
        )
import View.Helpers as VH


settings : Model -> Element.Element Msg
settings model =
    Element.column
        VH.exoColumnAttributes
        [ Element.el VH.heading2 <| Element.text "Settings"
        , Input.radio
            VH.exoColumnAttributes
            { onChange = \newStyle -> SetStyle (Style newStyle model.style.logo)
            , options =
                [ Input.option Style.Types.defaultPalette (Element.text "Light")
                , Input.option Style.Types.darkPalette (Element.text "Dark")
                ]
            , selected =
                Just model.style.palette
            , label = Input.labelAbove [] (Element.text "Color theme")
            }
        ]
