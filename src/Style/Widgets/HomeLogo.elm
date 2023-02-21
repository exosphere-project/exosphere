module Style.Widgets.HomeLogo exposing (homeLogo)

import Element
import Element.Font as Font
import Element.Region as Region
import Route
import Style.Helpers as SH
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text
import View.Helpers as VH
import View.Types


homeLogo : View.Types.Context -> { logoUrl : String, title : Maybe String } -> Element.Element msg
homeLogo context { logoUrl, title } =
    let
        linkUrl =
            Route.toUrl context.urlPathPrefix Route.Home
    in
    Element.link []
        { url = linkUrl
        , label =
            Element.row
                [ Element.spacing spacer.px12
                ]
                [ Element.image
                    [ Element.height (Element.px 48) ]
                    { src = logoUrl, description = "" }
                , VH.renderMaybe title
                    (Style.Widgets.Text.text Style.Widgets.Text.AppTitle
                        [ Region.heading 1
                        , Font.color (SH.toElementColor context.palette.menu.textOrIcon)
                        ]
                    )
                ]
        }
