module Style.Widgets.DataList exposing
    ( DataRecord
    , Filter
    , Model
    , Msg
    , getDefaultFiltOpts
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


{-| Opaque type representing options of all filters
-}
type FiltOpts
    = FiltOpts (Dict.Dict String (Set.Set String))


type alias Model =
    { selectedRowIds : Set.Set String
    , selectedFilters : FiltOpts
    , showFiltersDropdown : Bool
    }


getDefaultFiltOpts : List (Filter record) -> FiltOpts
getDefaultFiltOpts filters =
    FiltOpts
        (List.foldl
            (\filter filtOptsDict ->
                let
                    filtOpts =
                        if filter.multipleSelection then
                            filter.defaultFilterOptions

                        else
                            -- enforce one element in set
                            case
                                filter.defaultFilterOptions
                                    |> Set.toList
                                    |> List.head
                            of
                                Just oneFilterOption ->
                                    Set.fromList [ oneFilterOption ]

                                Nothing ->
                                    Set.empty
                in
                Dict.insert filter.id filtOpts filtOptsDict
            )
            Dict.empty
            filters
        )


init : FiltOpts -> Model
init selectedFilters =
    { selectedRowIds = Set.empty
    , selectedFilters = selectedFilters
    , showFiltersDropdown = False
    }


selectedFiltOpts : String -> Model -> Set.Set String
selectedFiltOpts filterId model =
    case model.selectedFilters of
        FiltOpts selectedFiltOpts_ ->
            Dict.get filterId selectedFiltOpts_
                |> Maybe.withDefault Set.empty


type Msg
    = ChangeRowSelection String Bool
    | ChangeAllRowsSelection (Set.Set String)
    | ChangeFiltOptCheckboxSelection String String Bool
    | ChangeFiltOptRadioSelection String String
    | ToggleFiltersDropdownVisiblity
    | ClearFilter String String
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
                selectedOptions =
                    selectedFiltOpts filterId model
            in
            { model
                | selectedFilters =
                    case model.selectedFilters of
                        FiltOpts selectedFiltOpts_ ->
                            FiltOpts <|
                                Dict.insert filterId
                                    (if isSelected then
                                        Set.insert option selectedOptions

                                     else
                                        Set.remove option selectedOptions
                                    )
                                    selectedFiltOpts_
            }

        ChangeFiltOptRadioSelection filterId option ->
            { model
                | selectedFilters =
                    case model.selectedFilters of
                        FiltOpts selectedFiltOpts_ ->
                            FiltOpts <|
                                Dict.insert filterId
                                    (Set.singleton option)
                                    selectedFiltOpts_
            }

        ToggleFiltersDropdownVisiblity ->
            { model | showFiltersDropdown = not model.showFiltersDropdown }

        ClearFilter filterId option ->
            { model
                | selectedFilters =
                    case model.selectedFilters of
                        FiltOpts selectedFiltOpts_ ->
                            FiltOpts <|
                                Dict.insert filterId
                                    (Set.remove
                                        option
                                        (selectedFiltOpts filterId model)
                                    )
                                    selectedFiltOpts_
            }

        ClearAllFilters ->
            { model
                | selectedFilters =
                    case model.selectedFilters of
                        FiltOpts selectedFiltOpts_ ->
                            FiltOpts <|
                                Dict.map (\_ _ -> Set.empty)
                                    selectedFiltOpts_
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
    { id : String
    , label : String
    , chipPrefix : String

    -- TODO: Can create a union type to better express interdependence of following 4
    -- will also allow to add more ways of filtering other than multiselect and uniselect
    , filterOptions :
        List FilterOption
    , defaultFilterOptions : Set.Set String
    , multipleSelection : Bool
    , onFilter : String -> DataRecord record -> Bool
    }


type alias FilterOption =
    { text : String
    , value : String
    }


view :
    Model
    -> (Msg -> msg) -- convert local Msg to a consumer's msg
    -> Style.Types.ExoPalette
    -> List (Element.Attribute msg)
    -> (DataRecord record -> Element.Element msg)
    -> List (DataRecord record)
    -> List (Set.Set String -> Element.Element msg)
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
            let
                selectedOptions =
                    Set.toList (selectedFiltOpts filter.id model)
            in
            if List.isEmpty selectedOptions then
                True

            else
                List.foldl
                    (\selectedOption isKeepable ->
                        (if selectedOption == "noChoice" then
                            True

                         else
                            filter.onFilter selectedOption dataRecord
                        )
                            || isKeepable
                    )
                    -- False is identity element for OR operation
                    False
                    selectedOptions

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
        (toolbarView model toMsg palette defaultRowStyle filteredData bulkActions filters
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
    -> List (DataRecord record)
    -> List (Set.Set String -> Element.Element msg)
    -> List (Filter record)
    -> Element.Element msg
toolbarView model toMsg palette rowStyle data bulkActions filters =
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
        , filtersView model toMsg palette filters
        , bulkActionsView
        ]


filtersView :
    Model
    -> (Msg -> msg)
    -> Style.Types.ExoPalette
    -> List (Filter record)
    -> Element.Element msg
filtersView model toMsg palette filters =
    let
        filtOptCheckbox : String -> FilterOption -> Element.Element msg
        filtOptCheckbox filterId filterOption =
            Input.checkbox [ Element.width Element.shrink ]
                { checked =
                    Set.member
                        filterOption.value
                        (selectedFiltOpts filterId model)
                , onChange = ChangeFiltOptCheckboxSelection filterId filterOption.value
                , icon = Input.defaultCheckbox
                , label =
                    Input.labelRight []
                        (Element.text filterOption.text)
                }
                |> Element.map toMsg

        filtOptsRadioSelector : Filter record -> Element.Element msg
        filtOptsRadioSelector filter =
            Input.radioRow [ Element.spacing 18 ]
                { onChange = ChangeFiltOptRadioSelection filter.id
                , selected =
                    List.head <|
                        Set.toList
                            (selectedFiltOpts filter.id model)
                , label =
                    Input.labelLeft
                        [ Element.paddingEach
                            { left = 0, right = 18, top = 0, bottom = 0 }
                        ]
                        (Element.text <| filter.label ++ ":")
                , options =
                    List.map
                        (\filterOption ->
                            Input.option filterOption.value
                                (Element.text filterOption.text)
                        )
                        filter.filterOptions
                        -- TODO: Let consumer control it. With custom type,
                        -- ensure that they pass a noChoice option value becuase
                        -- it doesn't render a filter chip
                        ++ [ Input.option "noChoice" (Element.text "No choice") ]
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
                                if filter.multipleSelection then
                                    Element.row [ Element.spacing 15 ]
                                        (Element.text (filter.label ++ ":")
                                            :: List.map
                                                (filtOptCheckbox filter.id)
                                                filter.filterOptions
                                        )

                                else
                                    filtOptsRadioSelector filter
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

        filterChipView : Filter record -> FilterOption -> Element.Element msg
        filterChipView filter selectedOpt =
            Element.row
                [ Border.width 1
                , Border.color <| Element.rgba255 0 0 0 0.16
                , Border.rounded 4
                ]
                [ Element.row
                    [ Font.size 14
                    , Element.paddingEach { top = 0, bottom = 0, left = 6, right = 0 }
                    ]
                    [ Element.el [ Font.color (Element.rgb255 96 96 96) ]
                        (Element.text filter.chipPrefix)
                    , Element.text selectedOpt.text
                    ]
                , Widget.iconButton (iconButtonStyle 6)
                    { text = "Clear filter"
                    , icon =
                        Element.el []
                            (FeatherIcons.x
                                |> FeatherIcons.withSize 16
                                |> FeatherIcons.toHtml []
                                |> Element.html
                            )
                    , onPress = Just <| ClearFilter filter.id selectedOpt.value
                    }
                    |> Element.map toMsg
                ]

        selectedFiltersChips =
            let
                filtOptsValueTextMap filtOpts =
                    List.foldl
                        (\filtOpt valTextMap ->
                            Dict.insert
                                filtOpt.value
                                filtOpt.text
                                valTextMap
                        )
                        Dict.empty
                        filtOpts
            in
            List.foldl
                (\filter filtChipsList ->
                    let
                        optsValTextMap =
                            filtOptsValueTextMap filter.filterOptions

                        selectedOptVals =
                            selectedFiltOpts filter.id model |> Set.toList

                        filterChip selectedOptVal =
                            case Dict.get selectedOptVal optsValTextMap of
                                Just selectedOptText ->
                                    filterChipView filter
                                        { text = selectedOptText
                                        , value = selectedOptVal
                                        }

                                Nothing ->
                                    -- when the selected option was added internally,
                                    -- for e.g. "noChoice" in radio selector
                                    Element.none
                    in
                    filtChipsList
                        ++ List.map filterChip selectedOptVals
                )
                []
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

                isNotEmpty selectedOpts =
                    not
                        (Set.isEmpty selectedOpts
                            || (Set.size selectedOpts
                                    == 1
                                    && Set.member "noChoice" selectedOpts
                               )
                        )

                isAnyFilterApplied =
                    case model.selectedFilters of
                        FiltOpts selectedFiltOpts_ ->
                            Dict.foldl
                                (\_ selectedOpts anyOptsSelected ->
                                    isNotEmpty selectedOpts || anyOptsSelected
                                )
                                False
                                selectedFiltOpts_
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
