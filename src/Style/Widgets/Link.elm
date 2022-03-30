module Style.Widgets.Link exposing (Behaviour(..), externalLink, link, navigate)

import Element
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Text as Text
import Types.HelperTypes



--- model


type Behaviour
    = Direct
    | NewTab



--- styles


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


link : ExoPalette -> Types.HelperTypes.Url -> String -> Element.Element msg
link palette url label =
    navigate Direct palette url (Text.body label)


externalLink : ExoPalette -> Types.HelperTypes.Url -> String -> Element.Element msg
externalLink palette url label =
    navigate NewTab palette url (Text.body label)
