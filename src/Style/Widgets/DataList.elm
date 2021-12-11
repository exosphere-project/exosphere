module Style.Widgets.DataList exposing (DataListModel, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Input as Input
import Set


type alias DataListModel =
    { selectedRowIndices : Set.Set Int
    }


init : DataListModel
init =
    { selectedRowIndices = Set.empty }


type Msg
    = ChangeRowSelection Int Bool


update : Msg -> DataListModel -> DataListModel
update msg model =
    case msg of
        ChangeRowSelection rowIndex isSelected ->
            if isSelected then
                { model | selectedRowIndices = Set.insert rowIndex model.selectedRowIndices }

            else
                { model | selectedRowIndices = Set.remove rowIndex model.selectedRowIndices }


view :
    List (Element.Attribute msg)
    -> (Msg -> msg)
    -> (dataRecord -> Element.Element Msg)
    -> List dataRecord
    -> DataListModel
    -> Element.Element msg
view styleAttrs msgMapper listItemView data model =
    let
        defaultRowStyle =
            [ Element.padding 24
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <| Element.rgba255 0 0 0 0.16
            , Element.width Element.fill
            ]

        rowStyle i =
            if i == List.length data - 1 then
                -- Don't show divider (bottom border) for last row
                defaultRowStyle ++ [ Border.width 0 ]

            else
                defaultRowStyle

        rowView : Int -> dataRecord -> Element.Element Msg
        rowView i dataRecord_ =
            Element.row (rowStyle i)
                [ Input.checkbox []
                    { checked = False
                    , onChange = \isChecked -> ChangeRowSelection i isChecked
                    , icon = Input.defaultCheckbox
                    , label = Input.labelHidden ("select row " ++ String.fromInt i)
                    }
                , Element.el [] (listItemView dataRecord_)
                ]

        toolbar : Bool -> Element.Element Msg
        toolbar _ =
            Element.row (rowStyle -1)
                [ --- Some action button that corresponds model-view-update handling by user
                  Element.text
                    (String.fromInt (Set.size model.selectedRowIndices)
                        ++ " row(s) selected"
                    )
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
        (toolbar True
            :: List.indexedMap rowView data
            |> List.map (\element -> Element.map msgMapper element)
        )
