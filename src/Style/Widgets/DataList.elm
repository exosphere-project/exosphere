module Style.Widgets.DataList exposing
    ( DataRecord
    , Filter
    , FilterId
    , FilterOptionText
    , FilterOptionValue
    , FilterSelectionValue(..)
    , Model
    , Msg
    , MultiselectOptionIdentifier
    , RowId
    , SearchFilter
    , SelectedFilterOptions
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
import FeatherIcons as Icons
import Helpers.Helpers exposing (alwaysRegex)
import Helpers.String
import Html.Attributes as HtmlA
import Murmur3
import Regex
import Set
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.Chip exposing (chip)
import Style.Widgets.Icon as Icon
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import View.Helpers as VH
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
    , searchText : String
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
    , searchText = ""
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
    | GotSearchText String
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

        GotSearchText searchText ->
            { model | searchText = searchText }

        ClearFilter filterId ->
            case selectedFilterOptionValue filterId model of
                Just filterOptionValue ->
                    { model
                        | selectedFilters =
                            case model.selectedFilters of
                                SelectedFilterOptions selectedFiltOptsDict ->
                                    let
                                        clearedFilterOpts =
                                            case filterOptionValue of
                                                MultiselectOption _ ->
                                                    MultiselectOption Set.empty

                                                UniselectOption _ ->
                                                    UniselectOption UniselectNoChoice
                                    in
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
                , searchText = ""
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

    -- TODO: Can create a union type to better express interdependence of following 3
    -- will also allow to add more ways of filtering other than multiselect and uniselect
    , filterOptions : List (DataRecord record) -> Dict.Dict FilterOptionValue FilterOptionText
    , filterTypeAndDefaultValue : FilterSelectionValue
    , onFilter : FilterOptionValue -> DataRecord record -> Bool
    }


type alias SelectionFilters record msg =
    { filters : List (Filter record)
    , dropdownMsgMapper : PopoverId -> msg
    }


type alias SearchFilter record =
    { label : String
    , placeholder : Maybe String
    , textToSearch : DataRecord record -> String
    }


getFilterOptionText :
    Filter record
    -> List (DataRecord record)
    -> FilterOptionValue
    -> FilterOptionText
getFilterOptionText filter data filterOptionValue =
    Dict.get filterOptionValue (filter.filterOptions data)
        |> Maybe.withDefault filterOptionValue


defaultRowStyle : ExoPalette -> List (Element.Attribute msg)
defaultRowStyle palette =
    [ Element.padding spacer.px24
    , Element.spacing spacer.px24
    , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
    , Border.color <|
        SH.toElementColor palette.neutral.border
    , Element.width Element.fill
    ]


borderStyleForRow : List (Element.Attribute msg) -> Int -> Int -> List (Element.Attribute msg)
borderStyleForRow rowStyle length i =
    if i == length - 1 then
        -- Don't show divider (bottom border) for last row
        rowStyle ++ [ Border.width 0 ]

    else
        rowStyle


view :
    String
    -> Model
    -> (Msg -> msg) -- convert DataList.Msg to a consumer's msg
    -> { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> List (Element.Attribute msg)
    -> (DataRecord record -> Element.Element msg)
    -> List (DataRecord record)
    -> List (Set.Set RowId -> Element.Element msg)
    -> Maybe (SelectionFilters record msg)
    -> Maybe (SearchFilter record)
    -> Element.Element msg
view resourceName model toMsg context styleAttrs listItemView data bulkActions selectionFilters searchFilter =
    let
        filteredData =
            case selectionFilters of
                Just selectionFilters_ ->
                    List.foldl
                        (\filter dataRecords ->
                            List.filter (keepARecord filter) dataRecords
                        )
                        data
                        selectionFilters_.filters
                        |> List.filter filterRecordsBySearchText

                Nothing ->
                    data |> List.filter filterRecordsBySearchText

        styleForRow i =
            borderStyleForRow (defaultRowStyle context.palette) (List.length filteredData) i

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

        filterRecordsBySearchText : DataRecord record -> Bool
        filterRecordsBySearchText =
            case searchFilter of
                Just searchFilter_ ->
                    \dataRecord ->
                        let
                            sanitize =
                                -- Replace whitespace, hyphen & underscore. (Retain e.g. ".", ":" for IP addresses.)
                                String.toLower >> Regex.replace (alwaysRegex "[-_\\s]") (always "")

                            needle =
                                model.searchText |> sanitize

                            hay =
                                searchFilter_.textToSearch dataRecord |> sanitize
                        in
                        String.contains needle hay

                Nothing ->
                    \_ -> True

        rows =
            if List.isEmpty filteredData then
                [ Element.column
                    (styleForRow -1
                        ++ [ Font.color <|
                                SH.toElementColor context.palette.neutral.text.subdued
                           ]
                    )
                    [ Icon.featherIcon [ Element.centerX ] (Icons.search |> Icons.withSize 36)
                    , Element.el
                        ([ Element.centerX
                         , Font.color (SH.toElementColor context.palette.neutral.text.default)
                         ]
                            ++ Text.typographyAttrs Text.Emphasized
                        )
                        (Element.text "No data found!")
                    , if not (List.isEmpty data) then
                        Element.el
                            [ Element.centerX
                            , Text.fontSize Text.Body
                            ]
                        <|
                            Element.text
                                "No records match the filter criteria. Clear all filters and try again."

                      else
                        Element.none
                    ]
                ]

            else
                let
                    showRowCheckbox =
                        not (List.isEmpty bulkActions)
                in
                List.indexedMap
                    (rowView model toMsg context.palette styleForRow listItemView showRowCheckbox)
                    filteredData
    in
    Element.column
        ([ Element.width Element.fill
         , Border.width 1
         , Border.color <| SH.toElementColor context.palette.neutral.border
         , Border.rounded 4
         , Background.color <| SH.toElementColor context.palette.neutral.background.frontLayer
         ]
            -- Add or override default style with passed style attributes
            ++ styleAttrs
        )
        (toolbarView
            resourceName
            model
            toMsg
            context
            (defaultRowStyle context.palette)
            { complete = data, filtered = filteredData }
            bulkActions
            selectionFilters
            searchFilter
            :: rows
        )


rowView :
    Model
    -> (Msg -> msg) -- convert DataList.Msg to a consumer's msg
    -> Style.Types.ExoPalette
    -> (Int -> List (Element.Attribute msg))
    -> (DataRecord record -> Element.Element msg)
    -> Bool
    -> Int
    -> DataRecord record
    -> Element.Element msg
rowView model toMsg palette rowStyle listItemView showRowCheckbox i dataRecord =
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
                        , icon =
                            \_ ->
                                Icon.lock
                                    (SH.toElementColor palette.neutral.icon)
                                    16
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
    String
    -> Model
    -> (Msg -> msg) -- convert DataList.Msg to a consumer's msg
    -> { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> List (Element.Attribute msg)
    ->
        { complete : List (DataRecord record)
        , filtered : List (DataRecord record)
        }
    -> List (Set.Set RowId -> Element.Element msg)
    -> Maybe (SelectionFilters record msg)
    -> Maybe (SearchFilter record)
    -> Element.Element msg
toolbarView resourceName model toMsg context rowStyle data bulkActions selectionFilters searchFilter =
    let
        ( selectionFiltersView, selectionFiltersAreActive ) =
            case selectionFilters of
                Just selectionFilters_ ->
                    ( filtersView model
                        toMsg
                        context
                        selectionFilters_
                        data.complete
                    , True
                    )

                Nothing ->
                    ( Element.none
                    , False
                    )

        ( searchFilterView, searchFilterIsActive ) =
            case searchFilter of
                Just searchFilter_ ->
                    ( Input.text
                        -- TODO: change background color?
                        (VH.inputItemAttributes context.palette
                            ++ [ Element.htmlAttribute <| HtmlA.style "height" "calc(1em + 16px)"
                               ]
                        )
                        { text = model.searchText
                        , placeholder =
                            Maybe.map
                                (\placeholderText ->
                                    Input.placeholder
                                        [ -- based on how placeholder is placed in default text input (hence "spacer" is not used)
                                          Element.paddingXY 12 6
                                        ]
                                        (Element.text placeholderText)
                                )
                                searchFilter_.placeholder
                        , onChange = GotSearchText
                        , label =
                            Input.labelLeft []
                                (Element.text searchFilter_.label)
                        }
                        |> Element.map toMsg
                    , True
                    )

                Nothing ->
                    ( Element.none, False )
    in
    if List.isEmpty bulkActions && not selectionFiltersAreActive && not searchFilterIsActive then
        Element.none

    else
        let
            selectableRecords =
                List.filter (\record -> record.selectable) data.filtered

            numberOfRecords =
                List.length data.complete |> String.fromInt

            numberOfFilteredRecords =
                List.length data.filtered |> String.fromInt

            displayedResourceWord =
                if List.length data.filtered > 1 then
                    Helpers.String.pluralize resourceName

                else
                    resourceName

            numberOfRecordsDisplay =
                if numberOfRecords == numberOfFilteredRecords then
                    Element.el [ Text.fontSize Text.Small, Font.color <| SH.toElementColor context.palette.neutral.text.subdued ]
                        (Element.text <| displayedResourceWord ++ ": " ++ numberOfRecords)

                else
                    Element.el [ Text.fontSize Text.Small, Font.color <| SH.toElementColor context.palette.neutral.text.subdued ]
                        (Element.text <| numberOfFilteredRecords ++ " " ++ displayedResourceWord ++ " filtered from " ++ numberOfRecords ++ " total")

            selectedRowIds =
                -- Remove those records' Ids that were deleted after being selected
                -- (This is because there seems no direct way to update the model
                -- as the data passed to the view changes)
                Set.filter
                    (\selectedRowId -> Set.member selectedRowId (idsSet selectableRecords))
                    model.selectedRowIds

            selectAllCheckbox =
                if List.isEmpty bulkActions then
                    -- don't show select all checkbox if no bulkActions are passed
                    Element.none

                else
                    let
                        areAllRowsSelected =
                            if List.isEmpty selectableRecords then
                                False

                            else
                                selectedRowIds == idsSet selectableRecords
                    in
                    Input.checkbox
                        [ Element.width Element.shrink
                        , Element.alignTop
                        , Element.paddingEach
                            { top = spacer.px8, left = 0, right = 0, bottom = 0 }
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
                -- show only when bulkActions are passed and at least 1 row is selected
                if List.isEmpty bulkActions || Set.isEmpty selectedRowIds then
                    Element.none

                else
                    Element.el [ Element.alignTop ] <|
                        Element.row [ Element.spacing spacer.px16 ]
                            (Element.text
                                ("Apply action to "
                                    ++ String.fromInt (Set.size selectedRowIds)
                                    ++ " row(s):"
                                )
                                :: List.map (\bulkAction -> bulkAction selectedRowIds)
                                    bulkActions
                            )
        in
        Element.column
            (rowStyle ++ [ Element.spacing spacer.px16 ])
            [ searchFilterView
            , Element.row [ Element.spacing spacer.px24, Element.width Element.fill ]
                [ selectAllCheckbox
                , selectionFiltersView
                , Element.column [ Element.spacing spacer.px12, Element.alignTop ]
                    [ Element.el [ Element.alignRight ] bulkActionsView
                    , Element.el [ Element.alignRight ] numberOfRecordsDisplay
                    ]
                ]
            ]


filtersView :
    Model
    -> (Msg -> msg)
    -> { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> SelectionFilters record msg
    -> List (DataRecord record)
    -> Element.Element msg
filtersView model toMsg context { filters, dropdownMsgMapper } data =
    if List.isEmpty filters then
        Element.none

    else
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
                Input.radioRow [ Element.spacing spacer.px16 ]
                    { onChange = ChangeFiltOptRadioSelection filter.id
                    , selected = Just uniselectOptValue
                    , label =
                        Input.labelLeft
                            [ Element.paddingEach
                                { left = 0, right = spacer.px16, top = 0, bottom = 0 }
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
                            (filter.filterOptions data |> Dict.keys)
                            -- TODO: Let consumer control it. With custom type,
                            -- ensure that they pass text for UniselectNoChoice
                            ++ [ Input.option UniselectNoChoice (Element.text "No choice") ]
                    }
                    |> Element.map toMsg

            filtersDropdownId =
                -- an ugly workaround to generate a unique id for filtersDropdown
                -- (until we make popover widget capable of generating unique ids internallly)
                "dataListfiltersDropdown-"
                    ++ (List.foldl
                            (\filter allFilterOptionTexts ->
                                allFilterOptionTexts ++ (Dict.values <| filter.filterOptions data)
                            )
                            []
                            filters
                            |> String.concat
                            |> Murmur3.hashString 4321
                            |> String.fromInt
                       )

            filtersDropdown closeDropdown =
                Element.column
                    [ Element.spacingXY 0 spacer.px24
                    ]
                    (Element.row [ Element.width Element.fill ]
                        [ Element.el [ Text.fontSize Text.Body ]
                            (Element.text "Apply Filters")
                        , Element.el [ Element.alignRight, closeDropdown ]
                            (Widget.iconButton
                                (SH.materialStyle context.palette).iconButton
                                { icon = Icon.sizedFeatherIcon 16 Icons.x
                                , text = "Close"
                                , onPress = Just NoOp
                                }
                                |> Element.map toMsg
                            )
                        ]
                        :: List.map
                            (\filter ->
                                if Dict.isEmpty <| filter.filterOptions data then
                                    Element.none

                                else
                                    case
                                        ( filter.filterTypeAndDefaultValue
                                        , selectedFilterOptionValue filter.id model
                                        )
                                    of
                                        ( MultiselectOption _, Just (MultiselectOption selectedOptionValues) ) ->
                                            Element.row [ Element.spacing spacer.px16 ]
                                                (Element.text (filter.label ++ ":")
                                                    :: List.map
                                                        (filtOptCheckbox filter selectedOptionValues)
                                                        (filter.filterOptions data |> Dict.keys)
                                                )

                                        ( UniselectOption _, Just (UniselectOption selectedOptionValue) ) ->
                                            filtOptsRadioSelector filter selectedOptionValue

                                        _ ->
                                            Element.none
                            )
                            filters
                    )

            addFilterBtn toggleDropdownMsg =
                let
                    buttonStyleDefaults =
                        (SH.materialStyle context.palette).button

                    buttonStyle =
                        { buttonStyleDefaults
                            | container =
                                buttonStyleDefaults.container
                                    ++ [ Element.padding spacer.px4
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
                        { icon = Icon.sizedFeatherIcon 20 Icons.plus
                        , text = "Add Filters"
                        , onPress = Just toggleDropdownMsg
                        }
                    )

            filterChipView : Filter record -> List (Element.Element Msg) -> Element.Element msg
            filterChipView filter selectedOptContent =
                chip context.palette
                    []
                    (Element.row []
                        (Element.el
                            [ Font.color <|
                                SH.toElementColor context.palette.neutral.text.subdued
                            ]
                            (Element.text filter.chipPrefix)
                            :: selectedOptContent
                        )
                    )
                    (Just <| ClearFilter filter.id)
                    |> Element.map toMsg

            selectedFiltersChips : List (Element.Element msg)
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
                                                    [ Font.color <|
                                                        SH.toElementColor context.palette.neutral.text.subdued
                                                    ]
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
                if isAnyFilterApplied || not (String.isEmpty model.searchText) then
                    Button.button Button.Text
                        context.palette
                        { text = "Clear filters"
                        , onPress = Just <| ClearAllFilters
                        }
                        |> Element.map toMsg

                else
                    Element.none
        in
        popover context
            dropdownMsgMapper
            { id = filtersDropdownId
            , content = filtersDropdown
            , contentStyleAttrs = [ Element.padding spacer.px24 ]
            , position = Style.Types.PositionBottomLeft
            , distanceToTarget = Nothing
            , target =
                \toggleDropdownMsg _ ->
                    Element.wrappedRow
                        [ Element.spacing spacer.px8
                        , Element.width Element.fill
                        , Element.alignTop
                        ]
                        (List.concat
                            [ [ Element.text "Filters: " ]
                            , selectedFiltersChips
                            , [ addFilterBtn toggleDropdownMsg, clearAllBtn ]
                            ]
                        )
            , targetStyleAttrs =
                [ Element.alignTop
                , Element.htmlAttribute <|
                    -- to let wrappedRow take all available width
                    HtmlA.style "flex-grow" "1"
                ]
            }
