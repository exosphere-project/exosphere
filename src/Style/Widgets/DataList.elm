module Style.Widgets.DataList exposing (DataRecord, Model, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Input as Input
import List.Extra
import Set
import Style.Widgets.Icon as Icon


type alias Model =
    { selectedRowIds : Set.Set String
    , selectedFilters : List (Set.Set String)
    }


init : List (Set.Set String) -> Model
init selectedFilters =
    -- TODO: Figure if selectedFilters can be enforced to be of same length as view's filters argument
    -- and if multipleSelection = False can enforce one element in set
    { selectedRowIds = Set.empty
    , selectedFilters = selectedFilters
    }


selectedFiltOpts : Int -> Model -> Set.Set String
selectedFiltOpts filterIndex model =
    List.Extra.getAt filterIndex model.selectedFilters
        |> Maybe.withDefault Set.empty


type Msg
    = ChangeRowSelection String Bool
    | ChangeAllRowsSelection (Set.Set String)
    | ChangeFiltOptCheckboxSelection Int String Bool
    | ChangeFiltOptRadioSelection Int String
    | NoOp


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

        ChangeFiltOptCheckboxSelection filterIndex option isSelected ->
            let
                selectedOptions =
                    selectedFiltOpts filterIndex model
            in
            { model
                | selectedFilters =
                    List.Extra.setAt filterIndex
                        (if isSelected then
                            Set.insert option selectedOptions

                         else
                            Set.remove option selectedOptions
                        )
                        model.selectedFilters
            }

        ChangeFiltOptRadioSelection filterIndex option ->
            { model
                | selectedFilters =
                    List.Extra.setAt filterIndex
                        (Set.singleton option)
                        model.selectedFilters
            }

        NoOp ->
            model


type alias DataRecord record =
    { record
        | id : String
        , selectable : Bool
    }


idsSet : List (DataRecord record) -> Set.Set String
idsSet dataRecords =
    Set.fromList <| List.map (\dataRecord -> dataRecord.id) dataRecords


type alias Filter record =
    { label : String
    , filterOptions :
        List
            { text : String
            , value : String
            }
    , multipleSelection : Bool
    , onFilter : String -> DataRecord record -> Bool
    }


view :
    Model
    -> (Msg -> msg) -- convert local Msg to a consumer's msg
    -> List (Element.Attribute msg)
    -> (DataRecord record -> Element.Element msg)
    -> List (DataRecord record)
    -> List (Set.Set String -> Element.Element msg)
    -> List (Filter record)
    -> Element.Element msg
view model toMsg styleAttrs listItemView data bulkActions filters =
    let
        defaultRowStyle =
            [ Element.padding 24
            , Element.spacing 20
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <| Element.rgba255 0 0 0 0.16
            , Element.width Element.fill
            ]

        rowStyle : Int -> List (Element.Attribute msg)
        rowStyle i =
            if i == List.length data - 1 then
                -- Don't show divider (bottom border) for last row
                defaultRowStyle ++ [ Border.width 0 ]

            else
                defaultRowStyle

        showRowCheckbox =
            not (List.isEmpty bulkActions)

        filterRecord : Int -> Filter record -> DataRecord record -> Bool
        filterRecord filterIndex filter dataRecord =
            let
                selectedOptions =
                    Set.toList (selectedFiltOpts filterIndex model)
            in
            if List.isEmpty selectedOptions then
                True

            else
                List.foldl
                    (\selectedOption isFiltered ->
                        filter.onFilter selectedOption dataRecord
                            || isFiltered
                    )
                    -- False is identity element for OR operation
                    False
                    selectedOptions

        filteredData =
            List.Extra.indexedFoldl
                (\i filter dataRecords ->
                    List.filter (filterRecord i filter) dataRecords
                )
                data
                filters
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
        -- TODO: Use context.palette instead of hard coded colors to create dark-theme version
        (toolbarView model toMsg defaultRowStyle filteredData bulkActions filters
            :: List.indexedMap
                (rowView model toMsg rowStyle listItemView showRowCheckbox)
                filteredData
        )


rowView :
    Model
    -> (Msg -> msg) -- convert local Msg to a consumer's msg
    -> (Int -> List (Element.Attribute msg))
    -> (DataRecord record -> Element.Element msg)
    -> Bool
    -> Int
    -> DataRecord record
    -> Element.Element msg
rowView model toMsg rowStyle listItemView showRowCheckbox i dataRecord =
    let
        rowCheckbox =
            if showRowCheckbox then
                if dataRecord.selectable then
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = Set.member dataRecord.id model.selectedRowIds
                        , onChange = \isChecked -> ChangeRowSelection dataRecord.id isChecked
                        , icon = Input.defaultCheckbox
                        , label = Input.labelHidden ("select row " ++ String.fromInt i)
                        }

                else
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = False
                        , onChange = \_ -> NoOp
                        , icon = \_ -> Icon.lock (Element.rgb255 42 42 42) 14 -- TODO: use color from context
                        , label = Input.labelHidden "locked row cannot be selected"
                        }

            else
                Element.none
    in
    Element.row (rowStyle i)
        [ rowCheckbox |> Element.map toMsg
        , listItemView dataRecord -- consumer-provided view already returns consumer's msg
        ]


toolbarView :
    Model
    -> (Msg -> msg) -- convert local Msg to a consumer's msg
    -> List (Element.Attribute msg)
    -> List (DataRecord record)
    -> List (Set.Set String -> Element.Element msg)
    -> List (Filter record)
    -> Element.Element msg
toolbarView model toMsg rowStyle data bulkActions filters =
    let
        selectableRecords =
            List.filter (\record -> record.selectable) data

        selectedRowIds =
            -- Remove those records' Ids that were deleted after being selected
            -- (This is because there seems no direct way to update the model
            -- as the data passed to the view changes)
            Set.filter
                (\selectedRowId -> Set.member selectedRowId (idsSet selectableRecords))
                model.selectedRowIds

        areAllRowsSelected =
            if List.isEmpty selectableRecords then
                False

            else
                selectedRowIds == idsSet selectableRecords

        selectAllCheckbox =
            if List.isEmpty bulkActions then
                -- don't show select all checkbox if no bulkActions are passed
                Element.none

            else
                Input.checkbox [ Element.width Element.shrink, Element.alignTop ]
                    { checked = areAllRowsSelected
                    , onChange =
                        \isChecked ->
                            if isChecked then
                                ChangeAllRowsSelection <| idsSet selectableRecords

                            else
                                ChangeAllRowsSelection Set.empty
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [ Element.paddingXY 14 0 ]
                            (Element.text "Select All")
                    }
                    |> Element.map toMsg

        bulkActionsView =
            -- show only when bulkActions are passed and atleast 1 row is selected
            if List.isEmpty bulkActions || Set.isEmpty selectedRowIds then
                Element.none

            else
                Element.row
                    [ Element.alignRight
                    , Element.alignTop
                    , Element.spacing 15
                    ]
                    (Element.text
                        ("Apply action to "
                            ++ String.fromInt (Set.size selectedRowIds)
                            ++ " row(s):"
                        )
                        :: List.map (\bulkAction -> bulkAction selectedRowIds)
                            bulkActions
                    )

        filtOptCheckbox filterIndex filterOption =
            Input.checkbox [ Element.width Element.shrink ]
                { checked =
                    Set.member
                        filterOption.value
                        (selectedFiltOpts filterIndex model)
                , onChange = ChangeFiltOptCheckboxSelection filterIndex filterOption.value
                , icon = Input.defaultCheckbox
                , label =
                    Input.labelRight []
                        (Element.text filterOption.text)
                }
                |> Element.map toMsg

        filtOptsRadioSelector filterLabel filterIndex filterOptions =
            Input.radioRow [ Element.spacing 18 ]
                { onChange = ChangeFiltOptRadioSelection filterIndex
                , selected =
                    List.head <|
                        Set.toList
                            (selectedFiltOpts filterIndex model)
                , label =
                    Input.labelLeft
                        [ Element.paddingEach
                            { left = 0, right = 18, top = 0, bottom = 0 }
                        ]
                        (Element.text <| filterLabel ++ ":")
                , options =
                    List.map
                        (\filterOption ->
                            Input.option filterOption.value
                                (Element.text filterOption.text)
                        )
                        filterOptions
                }
                |> Element.map toMsg

        filtersView =
            -- TODO: make it a dropdown, show filter chips
            Element.column
                [ Element.spacing 10
                , Element.paddingEach
                    { top = 0, right = 0, bottom = 0, left = 150 }
                ]
                (Element.el [ Element.centerX ] (Element.text "Apply filters:")
                    :: List.indexedMap
                        (\index filter ->
                            if filter.multipleSelection then
                                Element.row [ Element.spacing 15 ]
                                    (Element.text (filter.label ++ ":")
                                        :: List.map
                                            (filtOptCheckbox index)
                                            filter.filterOptions
                                    )

                            else
                                filtOptsRadioSelector filter.label
                                    index
                                    filter.filterOptions
                        )
                        filters
                )
    in
    Element.row rowStyle <|
        [ selectAllCheckbox, filtersView, bulkActionsView ]
