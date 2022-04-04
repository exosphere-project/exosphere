module Style.Widgets.Link exposing (Behaviour(..), externalLink, link, linkStyle, navigate)

import Element
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Text as Text
import Types.HelperTypes



--- model


{-| How does this link behave when clicked? Does it navigate using the current window or open in a new tab?
-}
type Behaviour
    = Direct
    | NewTab



--- styles


{-| Returns element attributes for link styling, including color & underline on mouseover.
-}
linkStyle : ExoPalette -> List (Element.Attribute msg)
linkStyle palette =
    [ palette.primary |> SH.toElementColor |> Font.color
    , Element.pointer
    , Border.color (SH.toElementColor palette.background)
    , Border.widthEach
        { bottom = 1
        , left = 0
        , top = 0
        , right = 0
        }
    , Element.mouseOver [ Border.color (SH.toElementColor palette.primary) ]
    ]



--- component


{-| Creates a link element which behaves as specified when clicked & has a label element.

    Link.navigate Link.Direct context.palette "http://app.exosphere.localhost:8000/home" (Element.text "Home")

-}
navigate : Behaviour -> ExoPalette -> Types.HelperTypes.Url -> Element.Element msg -> Element.Element msg
navigate behaviour palette url label =
    let
        handler =
            case behaviour of
                Direct ->
                    Element.link

                NewTab ->
                    Element.newTabLink
    in
    handler
        (linkStyle palette)
        { url = url
        , label = label
        }


{-| Creates a link element which navigates using the current window & uses default body text.

    Link.link context.palette "http://app.exosphere.localhost:8000/home" "Home"

-}
link : ExoPalette -> Types.HelperTypes.Url -> String -> Element.Element msg
link palette url label =
    navigate Direct palette url (Text.body label)


{-| Creates a link element which opens a new tab & uses default body text.

    Link.externalLink context.palette "https://gitlab.com/exosphere/exosphere" "Visit Exosphere"

-}
externalLink : ExoPalette -> Types.HelperTypes.Url -> String -> Element.Element msg
externalLink palette url label =
    navigate NewTab palette url (Text.body label)
