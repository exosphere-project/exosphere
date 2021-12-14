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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeRowSelection rowIndex isSelected ->
            if isSelected then
                { model | selectedRowIndices = Set.insert rowIndex model.selectedRowIndices }

            else
                { model | selectedRowIndices = Set.remove rowIndex model.selectedRowIndices }


view :
    List (Element.Attribute Msg)
    -> (dataRecord -> Element.Element Msg)
    -> List dataRecord
    -> Model
    -> Element.Element Msg
view styleAttrs listItemView data model =
    let
        defaultRowStyle =
            [ Element.padding 24
            , Element.spacing 20
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
                [ Input.checkbox [ Element.width Element.shrink ]
                    { checked = Set.member i model.selectedRowIndices
                    , onChange = \isChecked -> ChangeRowSelection i isChecked
                    , icon = Input.defaultCheckbox
                    , label = Input.labelHidden ("select row " ++ String.fromInt i)
                    }
                , listItemView dataRecord_
                ]

        toolbar =
            Element.row (rowStyle -1)
                [ -- Some action button that corresponds model-view-update handling by user
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
        (toolbar
            :: List.indexedMap rowView data
        )
