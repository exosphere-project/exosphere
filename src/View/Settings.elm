module View.Settings exposing (settings)

import Element
import Element.Input as Input
import FeatherIcons
import Style.Types
import Types.Msg exposing (SharedMsg(..))
import View.Helpers as VH
import View.Types


settings : View.Types.Context -> Style.Types.StyleMode -> Element.Element SharedMsg
settings context styleMode =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.row (VH.heading2 context.palette ++ [ Element.spacing 12 ])
            [ FeatherIcons.settings
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , Element.text "Settings"
            ]
        , Element.column VH.formContainer
            [ Input.radio
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
        ]
