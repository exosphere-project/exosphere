module Style.Widgets.DataList exposing (Model, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Input as Input
import Set


type alias Model =
    { selectedRowIds : Set.Set String }


init : Model
init =
    { selectedRowIds = Set.empty }


type Msg
    = ChangeRowSelection String Bool
    | ChangeAllRowsSelection (Set.Set String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeRowSelection rowId isSelected ->
            if isSelected then
                { model | selectedRowIds = Set.insert rowId model.selectedRowIds }

            else
                { model | selectedRowIds = Set.remove rowId model.selectedRowIds }

        ChangeAllRowsSelection newSelectedRowIds ->
            { model
                | selectedRowIds = newSelectedRowIds
            }


type alias DataRecord record =
    { record | id : String }


view :
    Model
    -> (Msg -> msg) -- convert local Msg to a consumer's msg
    -> List (Element.Attribute msg)
    -> (DataRecord record -> Element.Element msg)
    -> List (DataRecord record)
    -> (Set.Set String -> Element.Element msg)
    -> Element.Element msg
view model toMsg styleAttrs listItemView data bulkAction =
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

        rowView : Int -> DataRecord record -> Element.Element msg
        rowView i dataRecord =
            Element.row (rowStyle i)
                -- TODO: add condition: only when bulkActions is something
                -- TODO: option to specify if a row is selectable or locked
                [ Input.checkbox [ Element.width Element.shrink ]
                    { checked = Set.member dataRecord.id model.selectedRowIds
                    , onChange = \isChecked -> ChangeRowSelection dataRecord.id isChecked
                    , icon = Input.defaultCheckbox
                    , label = Input.labelHidden ("select row " ++ String.fromInt i)
                    }
                    |> Element.map toMsg
                , listItemView dataRecord -- consumer-provided view already returns consumer's msg
                ]

        toolbar dataRecords =
            Element.row (rowStyle -1)
                [ -- Checkbox to select all rows
                  Input.checkbox [ Element.width Element.shrink ]
                    { checked = Set.size model.selectedRowIds == numOfRows
                    , onChange =
                        \isChecked ->
                            if isChecked then
                                ChangeAllRowsSelection <|
                                    Set.fromList <|
                                        List.map
                                            (\dataRecord -> dataRecord.id)
                                            dataRecords

                            else
                                ChangeAllRowsSelection Set.empty
                    , icon = Input.defaultCheckbox
                    , label = Input.labelRight [] (Element.text "Select All")
                    }
                    |> Element.map toMsg
                , Element.text
                    (String.fromInt (Set.size model.selectedRowIds)
                        ++ " row(s) selected"
                    )
                , bulkAction model.selectedRowIds -- make type be the same
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
        (toolbar data
            :: List.indexedMap rowView data
        )
