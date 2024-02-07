module Style.Widgets.NavButton exposing (navButton)

import Element
import Element.Font as Font
import Route
import Style.Helpers as SH
import Style.Widgets.Icon exposing (Icon)
import Style.Widgets.IconButton exposing (FlowOrder(..), iconButton)
import View.Types


navButton : View.Types.Context -> List (Element.Attribute msg) -> { icon : Icon, label : String, route : Route.Route } -> Element.Element msg
navButton context attributes { icon, label, route } =
    Element.link
        (Font.color (SH.toElementColor context.palette.menu.textOrIcon)
            :: attributes
        )
        { url = Route.toUrl context.urlPathPrefix route
        , label = iconButton context.palette [] { icon = icon, iconPlacement = Before, label = label, onClick = Nothing }
        }
