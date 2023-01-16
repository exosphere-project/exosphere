module Style.Widgets.ChipsFilter exposing (ChipsFilterMsg(..), chipsFilter)

import Element
import Set exposing (Set)
import Style.Widgets.Spacer exposing (spacer)
import Widget
import Widget.Style exposing (ColumnStyle, TextInputStyle)


type alias ChipsFilterStyle style msg =
    { style
        | textInput : TextInputStyle msg
        , column : ColumnStyle msg
    }


type alias ChipsFilterModel =
    { selected : Set String
    , textInput : String
    , options : List String
    }


type ChipsFilterMsg
    = ToggleSelection String
    | SetTextInput String


chipsFilter : ChipsFilterStyle style ChipsFilterMsg -> ChipsFilterModel -> Element.Element ChipsFilterMsg
chipsFilter style model =
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
                                |> Just
                        }
                    )
      , text = model.textInput
      , placeholder = Nothing
      , label = "Chips"
      , onChange = SetTextInput
      }
        |> Widget.textInput style.textInput
    , model.selected
        |> Set.diff
            (model.options |> Set.fromList)
        |> Set.filter (String.toUpper >> String.contains (model.textInput |> String.toUpper))
        |> Set.toList
        |> List.map
            (\string ->
                Widget.button style.textInput.chipButton
                    { onPress =
                        string
                            |> ToggleSelection
                            |> Just
                    , text = string
                    , icon = Element.none
                    }
            )
        |> Element.wrappedRow [ Element.spacing spacer.px8 ]
    ]
        |> Widget.column style.column
