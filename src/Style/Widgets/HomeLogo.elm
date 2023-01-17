module Style.Widgets.HomeLogo exposing (homeLogo)

import Element
import Element.Font as Font
import Element.Region as Region
import Route
import Style.Helpers as SH
import Style.Widgets.Spacer exposing (spacer)
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
                    (\t ->
                        Element.el
                            [ Region.heading 1
                            , Font.semiBold
                            , Font.size 26
                            , Font.color (SH.toElementColor context.palette.menu.textOrIcon)
                            ]
                            (Element.text t)
                    )
                ]
        }
