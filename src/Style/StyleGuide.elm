module Style.StyleGuide exposing (main)

import Color
import Element
import Element.Font as Font
import Element.Region as Region
import Framework.Modifier exposing (Modifier(..))
import Html exposing (text)
import Style.Widgets.Card exposing (..)
import Style.Widgets.Icon exposing (..)
import Style.Widgets.IconButton exposing (..)
import Style.Widgets.MenuItem exposing (..)



{- When you create a new widget, add example usages to the `widgets` list here! -}


widgets : List (Element.Element a)
widgets =
    [ Element.text "Style.Widgets.MenuItem.menuItem"
    , menuItem Active "Active menu item" Nothing
    , menuItem Inactive "Inactive menu item" Nothing
    , Element.text "Style.Widgets.Icon.roundRect"
    , roundRect "blue"
    , Element.text "Style.Widgets.Icon.bell"
    , bell Color.black 40
    , Element.text "Style.Widgets.Icon.remove"
    , remove Color.black 40
    , Element.text "Style.Widgets.Card.exoCard"
    , exoCard "Title" "Subtitle" (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.IconButton.iconButton"
    , iconButton [ Small, Danger ] Nothing (remove Color.white 16)
    ]


intro : List (Element.Element a)
intro =
    [ Element.el
        [ Region.heading 2, Font.size 22, Font.bold ]
        (Element.text "Exosphere Style Guide")
    , Element.paragraph
        []
        [ Element.text "This page demonstrates usage of Exosphere's UI widgets. "
        , Element.text "See also the style guide for elm-style-framework (TODO link to demo style guide)"
        ]
    ]


main =
    Element.layout [] <|
        Element.column
            [ Element.padding 10
            , Element.spacing 20
            ]
        <|
            List.concat
                [ intro
                , widgets
                ]
