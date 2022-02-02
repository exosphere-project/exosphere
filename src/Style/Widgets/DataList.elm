module Style.Widgets.DataList exposing
    ( DataRecord
    , Filter
    , FilterSelectionValue(..)
    , Model
    , Msg
    , UniselectOptionIdentifier(..)
    , getDefaultFilterOptions
    , init
    , update
    , view
    )

import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Set
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Icon as Icon
import Widget


type alias FilterId =
    String


type alias RowId =
    String


type alias FilterOptionValue =
    String


type alias FilterOptionText =
    String


type FilterSelectionValue
    = MultiselectOption MultiselectOptionIdentifier
    | UniselectOption UniselectOptionIdentifier


type alias MultiselectOptionIdentifier =
    Set.Set FilterOptionValue


type UniselectOptionIdentifier
    = UniselectNoChoice
    | UniselectHasChoice FilterOptionValue


{-| Opaque type representing option values currently selected for each filter
-}
type SelectedFilterOptions
    = SelectedFilterOptions (Dict.Dict FilterId FilterSelectionValue)


type alias Model =
    { selectedRowIds : Set.Set RowId
    , selectedFilters : SelectedFilterOptions
    , showFiltersDropdown : Bool
    }


getDefaultFilterOptions : List (Filter record) -> SelectedFilterOptions
getDefaultFilterOptions filters =
    SelectedFilterOptions
        (List.foldl
            (\filter selectedFiltOptsDict ->
                Dict.insert filter.id
                    filter.filterTypeAndDefaultValue
                    selectedFiltOptsDict
            )
            Dict.empty
            filters
        )


init : SelectedFilterOptions -> Model
init selectedFilters =
    { selectedRowIds = Set.empty
    , selectedFilters = selectedFilters
    , showFiltersDropdown = False
    }


selectedFilterOptionValue : FilterId -> Model -> Maybe FilterSelectionValue
selectedFilterOptionValue filterId model =
    case model.selectedFilters of
        SelectedFilterOptions selectedFiltOpts ->
            Dict.get filterId selectedFiltOpts


type Msg
    = ChangeRowSelection RowId Bool
    | ChangeAllRowsSelection (Set.Set RowId)
    | ChangeFiltOptCheckboxSelection FilterId FilterOptionValue Bool
    | ChangeFiltOptRadioSelection FilterId UniselectOptionIdentifier
    | ToggleFiltersDropdownVisiblity
    | ClearFilter FilterId
    | ClearAllFilters
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

        ChangeFiltOptCheckboxSelection filterId option isSelected ->
            let
                updateSelection selectedOptions =
                    if isSelected then
                        Set.insert option selectedOptions

                    else
                        Set.remove option selectedOptions
            in
            case model.selectedFilters of
                SelectedFilterOptions selectedFiltOptsDict ->
                    case selectedFilterOptionValue filterId model of
                        Just (MultiselectOption selectedOptions) ->
                            { model
                                | selectedFilters =
                                    SelectedFilterOptions <|
                                        Dict.insert filterId
                                            (MultiselectOption <|
                                                updateSelection selectedOptions
                                            )
                                            selectedFiltOptsDict
                            }

                        _ ->
                            model

        ChangeFiltOptRadioSelection filterId uniselectOptValue ->
            { model
                | selectedFilters =
                    case model.selectedFilters of
                        SelectedFilterOptions selectedFiltOptsDict ->
                            SelectedFilterOptions <|
                                Dict.insert filterId
                                    (UniselectOption uniselectOptValue)
                                    selectedFiltOptsDict
            }

        ToggleFiltersDropdownVisiblity ->
            { model | showFiltersDropdown = not model.showFiltersDropdown }

        ClearFilter filterId ->
            case selectedFilterOptionValue filterId model of
                Just filterOptionValue ->
                    let
                        clearedFilterOpts =
                            case filterOptionValue of
                                MultiselectOption _ ->
                                    MultiselectOption Set.empty

                                UniselectOption _ ->
                                    UniselectOption UniselectNoChoice
                    in
                    { model
                        | selectedFilters =
                            case model.selectedFilters of
                                SelectedFilterOptions selectedFiltOptsDict ->
                                    SelectedFilterOptions <|
                                        Dict.insert filterId
                                            clearedFilterOpts
                                            selectedFiltOptsDict
                    }

                Nothing ->
                    model

        ClearAllFilters ->
            { model
                | selectedFilters =
                    case model.selectedFilters of
                        SelectedFilterOptions selectedFiltOptsDict ->
                            SelectedFilterOptions <|
                                Dict.map
                                    (\_ selectedOptValue ->
                                        case selectedOptValue of
                                            MultiselectOption _ ->
                                                MultiselectOption Set.empty

                                            UniselectOption _ ->
                                                UniselectOption UniselectNoChoice
                                    )
                                    selectedFiltOptsDict
            }

        NoOp ->
            model


type alias DataRecord record =
    { record
        | id : RowId
        , selectable : Bool
    }


idsSet : List (DataRecord record) -> Set.Set RowId
idsSet dataRecords =
    Set.fromList <| List.map (\dataRecord -> dataRecord.id) dataRecords


type alias Filter record =
    { id : FilterId
    , label : String
    , chipPrefix : String

    -- TODO: Can create a union type to better express interdependence of following 4
    -- will also allow to add more ways of filtering other than multiselect and uniselect
    , filterOptions : List (DataRecord record) -> List FilterOptionValue
    , optionsTextMap : List (DataRecord record) -> Dict.Dict FilterOptionValue FilterOptionText
    , filterTypeAndDefaultValue : FilterSelectionValue
    , onFilter : FilterOptionValue -> DataRecord record -> Bool
    }


getFilterOptionText : Filter record -> List (DataRecord record) -> FilterOptionValue -> FilterOptionText
getFilterOptionText filter data filterOptionValue =
    Dict.get filterOptionValue (filter.optionsTextMap data)
        |> Maybe.withDefault filterOptionValue


view :
    Model
    -> (Msg -> msg) -- convert local Msg to a consumer's msg
    -> Style.Types.ExoPalette
    -> List (Element.Attribute msg)
    -> (DataRecord record -> Element.Element msg)
    -> List (DataRecord record)
    -> List (Set.Set RowId -> Element.Element msg)
    -> List (Filter record)
    -> Element.Element msg
view model toMsg palette styleAttrs listItemView data bulkActions filters =
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

        keepARecord : Filter record -> DataRecord record -> Bool
        keepARecord filter dataRecord =
            case selectedFilterOptionValue filter.id model of
                Just (MultiselectOption multiselectOptValues) ->
                    if Set.isEmpty multiselectOptValues then
                        True

                    else
                        Set.foldl
                            (\selectedOptValue isKeepable ->
                                filter.onFilter selectedOptValue dataRecord
                                    || isKeepable
                            )
                            -- False is identity element for OR operation
                            False
                            multiselectOptValues

                Just (UniselectOption uniselectOptValue) ->
                    case uniselectOptValue of
                        UniselectNoChoice ->
                            True

                        UniselectHasChoice selectedOptValue ->
                            filter.onFilter selectedOptValue dataRecord

                Nothing ->
                    True

        filteredData =
            List.foldl
                (\filter dataRecords ->
                    List.filter (keepARecord filter) dataRecords
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
        (toolbarView model
            toMsg
            palette
            defaultRowStyle
            { complete = data, filtered = filteredData }
            bulkActions
            filters
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
    -> Style.Types.ExoPalette
    -> List (Element.Attribute msg)
    ->
        { complete : List (DataRecord record)
        , filtered : List (DataRecord record)
        }
    -> List (Set.Set RowId -> Element.Element msg)
    -> List (Filter record)
    -> Element.Element msg
toolbarView model toMsg palette rowStyle data bulkActions filters =
    let
        selectableRecords =
            List.filter (\record -> record.selectable) data.filtered

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
                Input.checkbox
                    [ Element.width Element.shrink
                    , Element.alignTop
                    , Element.paddingEach
                        { top = 8, left = 0, right = 0, bottom = 0 }
                    ]
                    { checked = areAllRowsSelected
                    , onChange =
                        \isChecked ->
                            if isChecked then
                                ChangeAllRowsSelection <| idsSet selectableRecords

                            else
                                ChangeAllRowsSelection Set.empty
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelHidden "Select all rows"
                    }
                    |> Element.map toMsg

        bulkActionsView =
            -- show only when bulkActions are passed and atleast 1 row is selected
            if List.isEmpty bulkActions || Set.isEmpty selectedRowIds then
                Element.none

            else
                Element.row
                    [ Element.alignRight
                    , Element.spacing 15
                    , Element.alignTop
                    ]
                    (Element.text
                        ("Apply action to "
                            ++ String.fromInt (Set.size selectedRowIds)
                            ++ " row(s):"
                        )
                        :: List.map (\bulkAction -> bulkAction selectedRowIds)
                            bulkActions
                    )
    in
    Element.row
        rowStyle
        [ selectAllCheckbox
        , filtersView model toMsg palette filters data.complete
        , bulkActionsView
        ]


filtersView :
    Model
    -> (Msg -> msg)
    -> Style.Types.ExoPalette
    -> List (Filter record)
    -> List (DataRecord record)
    -> Element.Element msg
filtersView model toMsg palette filters data =
    let
        filtOptCheckbox : Filter record -> MultiselectOptionIdentifier -> FilterOptionValue -> Element.Element msg
        filtOptCheckbox filter optionValues filterOptionValue =
            let
                checked =
                    Set.member filterOptionValue optionValues
            in
            Input.checkbox [ Element.width Element.shrink ]
                { checked = checked
                , onChange = ChangeFiltOptCheckboxSelection filter.id filterOptionValue
                , icon = Input.defaultCheckbox
                , label =
                    Input.labelRight []
                        (getFilterOptionText filter data filterOptionValue
                            |> Element.text
                        )
                }
                |> Element.map toMsg

        filtOptsRadioSelector : Filter record -> UniselectOptionIdentifier -> Element.Element msg
        filtOptsRadioSelector filter uniselectOptValue =
            Input.radioRow [ Element.spacing 18 ]
                { onChange = ChangeFiltOptRadioSelection filter.id
                , selected = Just uniselectOptValue
                , label =
                    Input.labelLeft
                        [ Element.paddingEach
                            { left = 0, right = 18, top = 0, bottom = 0 }
                        ]
                        (Element.text <| filter.label ++ ":")
                , options =
                    List.map
                        (\filterOptionValue ->
                            Input.option (UniselectHasChoice filterOptionValue)
                                (getFilterOptionText filter data filterOptionValue
                                    |> Element.text
                                )
                        )
                        (filter.filterOptions data)
                        -- TODO: Let consumer control it. With custom type,
                        -- ensure that they pass text for UniselectNoChoice
                        ++ [ Input.option UniselectNoChoice (Element.text "No choice") ]
                }
                |> Element.map toMsg

        iconButtonStyleDefaults =
            (SH.materialStyle palette).iconButton

        iconButtonStyle padding =
            { iconButtonStyleDefaults
                | container =
                    iconButtonStyleDefaults.container
                        ++ [ Element.padding padding
                           , Element.height Element.shrink
                           ]
            }

        filtersDropdown =
            Element.el [ Element.paddingXY 0 6 ] <|
                Element.column
                    [ Element.padding 24
                    , Element.spacingXY 0 24
                    , Background.color <| SH.toElementColor palette.background
                    , Border.width 1
                    , Border.color <| Element.rgba255 0 0 0 0.16
                    , Border.shadow SH.shadowDefaults
                    ]
                    (Element.row [ Element.width Element.fill ]
                        [ Element.el [ Font.size 18 ]
                            (Element.text "Apply Filters")
                        , Element.el [ Element.alignRight ]
                            (Widget.iconButton
                                (iconButtonStyle 0)
                                { icon =
                                    FeatherIcons.x
                                        |> FeatherIcons.withSize 16
                                        |> FeatherIcons.toHtml []
                                        |> Element.html
                                , text = "Close"
                                , onPress = Just <| ToggleFiltersDropdownVisiblity
                                }
                                |> Element.map toMsg
                            )
                        ]
                        :: List.map
                            (\filter ->
                                case ( filter.filterTypeAndDefaultValue, selectedFilterOptionValue filter.id model ) of
                                    ( MultiselectOption _, Just (MultiselectOption selectedOptionValues) ) ->
                                        Element.row [ Element.spacing 15 ]
                                            (Element.text (filter.label ++ ":")
                                                :: List.map
                                                    (filtOptCheckbox filter selectedOptionValues)
                                                    (filter.filterOptions data)
                                            )

                                    ( UniselectOption _, Just (UniselectOption selectedOptionValue) ) ->
                                        filtOptsRadioSelector filter selectedOptionValue

                                    _ ->
                                        Element.none
                            )
                            filters
                    )

        addFilterBtn =
            let
                buttonStyleDefaults =
                    (SH.materialStyle palette).button

                buttonStyle =
                    { buttonStyleDefaults
                        | container =
                            buttonStyleDefaults.container
                                ++ [ Element.padding 4
                                   , Element.height Element.shrink
                                   ]
                        , labelRow =
                            buttonStyleDefaults.labelRow
                                ++ [ Element.padding 0
                                   , Element.spacing 0
                                   , Element.width Element.shrink
                                   ]
                    }
            in
            Element.el
                []
                (Widget.iconButton
                    buttonStyle
                    { icon =
                        Element.el []
                            (FeatherIcons.plus
                                |> FeatherIcons.withSize 20
                                |> FeatherIcons.toHtml []
                                |> Element.html
                            )
                    , text = "Add Filters"
                    , onPress = Just <| ToggleFiltersDropdownVisiblity
                    }
                    |> Element.map toMsg
                )

        filterChipView : Filter record -> List (Element.Element msg) -> Element.Element msg
        filterChipView filter selectedOptContent =
            Element.row
                [ Border.width 1
                , Border.color <| Element.rgba255 0 0 0 0.16
                , Border.rounded 4
                ]
                [ Element.row
                    [ Font.size 14
                    , Element.paddingEach { top = 0, bottom = 0, left = 6, right = 0 }
                    ]
                    (Element.el [ Font.color (Element.rgb255 96 96 96) ]
                        (Element.text filter.chipPrefix)
                        :: selectedOptContent
                    )
                , Widget.iconButton (iconButtonStyle 6)
                    { text = "Clear filter"
                    , icon =
                        Element.el []
                            (FeatherIcons.x
                                |> FeatherIcons.withSize 16
                                |> FeatherIcons.toHtml []
                                |> Element.html
                            )
                    , onPress = Just <| ClearFilter filter.id
                    }
                    |> Element.map toMsg
                ]

        selectedFiltersChips =
            List.map
                (\filter ->
                    case selectedFilterOptionValue filter.id model of
                        Just (MultiselectOption selectedOptVals) ->
                            if Set.isEmpty selectedOptVals then
                                Element.none

                            else
                                filterChipView filter
                                    (Set.toList selectedOptVals
                                        |> List.map
                                            (\selectedOptVal ->
                                                getFilterOptionText filter
                                                    data
                                                    selectedOptVal
                                                    |> Element.text
                                            )
                                        |> List.intersperse
                                            (Element.el
                                                [ Font.color (Element.rgb255 96 96 96) ]
                                                (Element.text " or ")
                                            )
                                    )

                        Just (UniselectOption uniselectOptVal) ->
                            case uniselectOptVal of
                                UniselectNoChoice ->
                                    Element.none

                                UniselectHasChoice selectedOptVal ->
                                    filterChipView filter
                                        [ getFilterOptionText filter data selectedOptVal
                                            |> Element.text
                                        ]

                        Nothing ->
                            Element.none
                )
                filters

        clearAllBtn =
            let
                textBtnStyleDefaults =
                    (SH.materialStyle palette).textButton

                textBtnStyle =
                    { textBtnStyleDefaults
                        | container =
                            textBtnStyleDefaults.container
                                ++ [ Font.medium
                                   , Element.padding 6
                                   , Element.height Element.shrink
                                   ]
                    }

                isAnyOptSelected selectedOpts =
                    case selectedOpts of
                        MultiselectOption multiselectOptValues ->
                            not (Set.isEmpty multiselectOptValues)

                        UniselectOption uniselectOptValue ->
                            case uniselectOptValue of
                                UniselectNoChoice ->
                                    False

                                UniselectHasChoice _ ->
                                    True

                isAnyFilterApplied =
                    case model.selectedFilters of
                        SelectedFilterOptions selectedFiltOptsDict ->
                            Dict.foldl
                                (\_ selectedOpts anyOptSelected ->
                                    isAnyOptSelected selectedOpts
                                        || anyOptSelected
                                )
                                False
                                selectedFiltOptsDict
            in
            if isAnyFilterApplied then
                Widget.textButton
                    textBtnStyle
                    { text = "Clear filters"
                    , onPress = Just <| ClearAllFilters
                    }
                    |> Element.map toMsg

            else
                Element.none
    in
    Element.wrappedRow
        ([ Element.spacing 10
         , Element.width Element.fill
         , Element.alignTop
         ]
            ++ (if model.showFiltersDropdown then
                    [ Element.below filtersDropdown ]

                else
                    []
               )
        )
        (List.concat
            [ [ Element.text "Filters: " ]
            , selectedFiltersChips
            , [ addFilterBtn, clearAllBtn ]
            ]
        )
