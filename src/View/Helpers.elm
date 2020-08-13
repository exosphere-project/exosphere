module View.Helpers exposing
    ( browserLink
    , compactKVRow
    , compactKVSubRow
    , edges
    , exoColumnAttributes
    , exoElementAttributes
    , exoPaddingSpacingAttributes
    , exoRowAttributes
    , heading2
    , heading3
    , hint
    , possiblyUntitledResource
    , renderMessage
    )

import Element
import Element.Events
import Element.Font as Font
import Element.Region as Region
import Helpers.Error exposing (ErrorLevel(..), toFriendlyErrorLevel)
import Helpers.Time exposing (humanReadableTime)
import Types.HelperTypes
import Types.Types exposing (LogMessage, Msg(..))
import View.Types



{- Elm UI Doodads -}


exoRowAttributes : List (Element.Attribute Msg)
exoRowAttributes =
    exoElementAttributes


exoColumnAttributes : List (Element.Attribute Msg)
exoColumnAttributes =
    exoElementAttributes


exoElementAttributes : List (Element.Attribute Msg)
exoElementAttributes =
    exoPaddingSpacingAttributes


exoPaddingSpacingAttributes : List (Element.Attribute Msg)
exoPaddingSpacingAttributes =
    [ Element.padding 10
    , Element.spacing 10
    ]


heading2 : List (Element.Attribute Msg)
heading2 =
    [ Region.heading 2
    , Font.bold
    , Font.size 24
    ]


heading3 : List (Element.Attribute Msg)
heading3 =
    [ Region.heading 3
    , Font.bold
    , Font.size 20
    ]


compactKVRow : String -> Element.Element Msg -> Element.Element Msg
compactKVRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10 ])
        [ Element.paragraph [ Element.alignTop, Element.width (Element.px 200), Font.bold ] [ Element.text key ]
        , value
        ]


compactKVSubRow : String -> Element.Element Msg -> Element.Element Msg
compactKVSubRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10, Font.size 14 ])
        [ Element.paragraph [ Element.width (Element.px 175), Font.bold ] [ Element.text key ]
        , Element.el [] value
        ]


type alias Edges =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


edges : Edges
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


hint : String -> Element.Attribute msg
hint hintText =
    Element.below
        (Element.el
            [ Font.color (Element.rgb 1 0 0)
            , Font.size 14
            , Element.alignRight
            , Element.moveDown 6
            ]
            (Element.text hintText)
        )


renderMessage : LogMessage -> Element.Element Msg
renderMessage message =
    let
        levelColor : ErrorLevel -> Element.Color
        levelColor errLevel =
            case errLevel of
                ErrorDebug ->
                    Element.rgb 0 0.55 0

                ErrorInfo ->
                    Element.rgb 0 0 0

                ErrorWarn ->
                    Element.rgb 0.8 0.5 0

                ErrorCrit ->
                    Element.rgb 0.7 0 0
    in
    Element.column (exoColumnAttributes ++ [ Element.spacing 13 ])
        [ Element.row [ Element.alignRight ]
            [ Element.el
                [ Font.color <| levelColor message.context.level
                , Font.bold
                ]
                (Element.text
                    (toFriendlyErrorLevel message.context.level)
                )
            , Element.el [ Font.color <| Element.rgb 0.4 0.4 0.4 ]
                (Element.text
                    (" at " ++ humanReadableTime message.timestamp)
                )
            ]
        , compactKVRow "We were trying to"
            (Element.paragraph [] [ Element.text message.context.actionContext ])
        , compactKVRow "Message"
            (Element.paragraph [] [ Element.text message.message ])
        , case message.context.recoveryHint of
            Just hint_ ->
                compactKVRow "Recovery hint" (Element.paragraph [] [ Element.text hint_ ])

            Nothing ->
                Element.none
        ]


browserLink : Bool -> Types.HelperTypes.Url -> View.Types.BrowserLinkLabel -> Element.Element Msg
browserLink isElectron url label =
    let
        linkAttribs =
            [ Font.color (Element.rgb255 50 115 220)
            , Font.underline
            , Element.pointer
            ]

        renderedLabel =
            case label of
                View.Types.BrowserLinkTextLabel str ->
                    { attribs = linkAttribs
                    , contents = Element.text str
                    }

                View.Types.BrowserLinkFancyLabel el ->
                    { attribs = []
                    , contents = el
                    }
    in
    if isElectron then
        Element.el
            (renderedLabel.attribs
                ++ [ Element.Events.onClick (OpenInBrowser url) ]
            )
            renderedLabel.contents

    else
        Element.newTabLink
            renderedLabel.attribs
            { url = url
            , label = renderedLabel.contents
            }


possiblyUntitledResource : String -> String -> String
possiblyUntitledResource name resourceType =
    case name of
        "" ->
            "(Untitled " ++ resourceType ++ ")"

        _ ->
            name
