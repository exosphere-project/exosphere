module Style.Widgets.DataList exposing (Model, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Input as Input
import Set


type alias Model =
    { selectedRowIndices : Set.Set Int
    }


init : Model
init =
    { selectedRowIndices = Set.empty }


type Msg
    = ChangeRowSelection Int Bool
    | ChangeAllRowSelection Int Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeRowSelection rowIndex isSelected ->
            if isSelected then
                { model | selectedRowIndices = Set.insert rowIndex model.selectedRowIndices }

            else
                { model | selectedRowIndices = Set.remove rowIndex model.selectedRowIndices }

        ChangeAllRowSelection numOfRows isSelected ->
            { model
                | selectedRowIndices =
                    if isSelected then
                        Set.fromList <| List.range 0 (numOfRows - 1)

                    else
                        Set.empty
            }


view :
    Model
    -> (Msg -> msg) -- convert local Msg to a consumer's msg
    -> List (Element.Attribute msg)
    -> (dataRecord -> Element.Element msg)
    -> List dataRecord -- view will auto rerender (reflect deletion/addition) as this changes
    -- -> Maybe (List (Element.Element Msg )) - bulkActions (views that emit msgs)
    -> Element.Element msg
view model toMsg styleAttrs listItemView data =
    let
        defaultRowStyle =
            [ Element.padding 24
            , Element.spacing 20
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <| Element.rgba255 0 0 0 0.16
            , Element.width Element.fill
            ]

        numOfRows =
            List.length data

        rowStyle i =
            if i == numOfRows - 1 then
                -- Don't show divider (bottom border) for last row
                defaultRowStyle ++ [ Border.width 0 ]

            else
                defaultRowStyle

        rowView : Int -> dataRecord -> Element.Element msg
        rowView i dataRecord_ =
            Element.row (rowStyle i)
                -- TODO: add condition: only when bulkActions is something
                -- TODO: option to specify if a row is selectable or locked
                [ Input.checkbox [ Element.width Element.shrink ]
                    { checked = Set.member i model.selectedRowIndices
                    , onChange = \isChecked -> ChangeRowSelection i isChecked
                    , icon = Input.defaultCheckbox
                    , label = Input.labelHidden ("select row " ++ String.fromInt i)
                    }
                    |> Element.map toMsg
                , listItemView dataRecord_ -- consumer-provided view already returns consumer's msg
                ]

        toolbar =
            Element.row (rowStyle -1)
                [ -- Checkbox to select all rows
                  Input.checkbox [ Element.width Element.shrink ]
                    { checked = Set.size model.selectedRowIndices == numOfRows
                    , onChange = ChangeAllRowSelection numOfRows
                    , icon = Input.defaultCheckbox
                    , label = Input.labelRight [] (Element.text "Select All")
                    }
                    |> Element.map toMsg
                , Element.text
                    (String.fromInt (Set.size model.selectedRowIndices)
                        ++ " row(s) selected"
                    )

                -- TODO: actionButtons views add
                ]
    in
    Element.column
        ([ Element.width Element.fill
         , Border.width 1
         , Border.color (Element.rgba255 0 0 0 0.1)
         , Border.rounded 4
         ]
            -- Add or override default style with passed style attributes
            ++ styleAttrs
        )
        (toolbar
            :: List.indexedMap rowView data
        )
