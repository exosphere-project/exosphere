module Style.StyleGuide exposing (main)

import Browser
import Color
import Element
import Element.Font as Font
import Element.Region as Region
import Framework.Modifier exposing (Modifier(..))
import Html
import Set exposing (Set)
import Style.Widgets.Card exposing (badge, exoCard)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon exposing (bell, question, remove, roundRect, timesCircle)
import Style.Widgets.IconButton exposing (chip, iconButton)
import Style.Widgets.MenuItem exposing (MenuItemState(..), menuItem)
import Widget
import Widget.Style exposing (ColumnStyle, TextInputStyle)
import Widget.Style.Material as Material



{- When you create a new widget, add example usages to the `widgets` list here! -}


widgets : List (Element.Element a)
widgets =
    [ Element.text "Style.Widgets.MenuItem.menuItem"
    , menuItem Active "Active menu item" Nothing
    , menuItem Inactive "Inactive menu item" Nothing
    , Element.text "Style.Widgets.Icon.roundRect"
    , roundRect Color.black 40
    , Element.text "Style.Widgets.Icon.bell"
    , bell Color.black 40
    , Element.text "Style.Widgets.Icon.question"
    , question Color.black 40
    , Element.text "Style.Widgets.Icon.remove"
    , remove Color.black 40
    , Element.text "Style.Widgets.Icon.timesCircle (black)"
    , timesCircle Color.black 40
    , Element.text "Style.Widgets.Icon.timesCircle (white)"
    , timesCircle Color.white 40
    , Element.text "Style.Widgets.Card.exoCard"
    , exoCard "Title" "Subtitle" (Element.text "Lorem ipsum dolor sit amet.")
    , Element.text "Style.Widgets.Card.badge"
    , badge "belongs to this project"
    , Element.text "Style.Widgets.IconButton.iconButton"
    , iconButton [ Small, Danger ] Nothing (remove Color.white 16)
    , Element.text "Style.Widgets.CopyableText.CopyableText"
    , copyableText "foobar"
    , Element.text "Style.Widgets.IconButton.chip"
    , chip Nothing (Element.text "chip label")
    , Element.text "Style.Widgets.IconButton.chip (with badge)"
    , chip Nothing (Element.row [ Element.spacing 5 ] [ Element.text "ubuntu", badge "10" ])
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


oldMain : Html.Html msg
oldMain =
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



-- Playing with elm-ui-widgets below


type alias Style style msg =
    { style
        | textInput : TextInputStyle msg
        , column : ColumnStyle msg
    }


materialStyle : Style {} msg
materialStyle =
    { textInput = Material.textInput Material.defaultPalette
    , column = Material.column
    }


options : List String
options =
    [ "Apple"
    , "Kiwi"
    , "Strawberry"
    , "Pineapple"
    , "Mango"
    , "Grapes"
    , "Watermelon"
    , "Orange"
    , "Lemon"
    , "Blueberry"
    , "Grapefruit"
    , "Coconut"
    , "Cherry"
    , "Banana"
    ]


type alias Model =
    { selected : Set String
    , textInput : String
    }


type Msg
    = ToggleSelection String
    | SetTextInput String


init : ( Model, Cmd Msg )
init =
    ( { selected = Set.empty
      , textInput = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSelection string ->
            ( { model
                | selected =
                    model.selected
                        |> (if model.selected |> Set.member string then
                                Set.remove string

                            else
                                Set.insert string
                           )
              }
            , Cmd.none
            )

        SetTextInput string ->
            ( { model | textInput = string }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : (Msg -> msg) -> Style style msg -> Model -> Element.Element msg
view msgMapper style model =
    [ { chips =
            model.selected
                |> Set.toList
                |> List.map
                    (\string ->
                        { icon = Element.none
                        , text = string
                        , onPress =
                            string
                                |> ToggleSelection
                                |> msgMapper
                                |> Just
                        }
                    )
      , text = model.textInput
      , placeholder = Nothing
      , label = "Chips"
      , onChange = SetTextInput >> msgMapper
      }
        |> Widget.textInput style.textInput
    , model.selected
        |> Set.diff
            (options |> Set.fromList)
        |> Set.filter (String.toUpper >> String.contains (model.textInput |> String.toUpper))
        |> Set.toList
        |> List.map
            (\string ->
                Widget.button style.textInput.chipButton
                    { onPress =
                        string
                            |> ToggleSelection
                            |> msgMapper
                            |> Just
                    , text = string
                    , icon = Element.none
                    }
            )
        |> Element.wrappedRow [ Element.spacing 10 ]
    ]
        |> Widget.column style.column


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view identity materialStyle >> Element.layout []
        , update = update
        , subscriptions = subscriptions
        }
