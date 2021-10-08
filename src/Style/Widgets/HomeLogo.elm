module Style.Widgets.HomeLogo exposing (homeLogo)

import Element
import Element.Font as Font
import Element.Region as Region
import Route
import Style.Helpers as SH
import View.Types


homeLogo : View.Types.Context -> { logoUrl : String, title : String } -> Element.Element msg
homeLogo context { logoUrl, title } =
    let
        linkUrl =
            Route.toUrl context.urlPathPrefix Route.Home
    in
    Element.link []
        { url = linkUrl
        , label =
            Element.row
                [ Element.padding 5
                , Element.spacing 20
                ]
                [ Element.image
                    [ Element.height (Element.px 50) ]
                    { src = logoUrl, description = "" }
                , Element.el
                    [ Region.heading 1
                    , Font.bold
                    , Font.size 26
                    , Font.color (SH.toElementColor context.palette.menu.on.surface)
                    ]
                    (Element.text title)
                ]
        }
