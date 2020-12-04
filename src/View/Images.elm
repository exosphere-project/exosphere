module View.Images exposing (imagesIfLoaded)

import Element
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Filesize
import List.Extra
import OpenStack.Types as OSTypes
import Set
import Set.Extra
import Style.Theme
import Style.Widgets.Card as ExoCard
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton exposing (chip)
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( ImageListViewParams
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , SortTableParams
        )
import View.Helpers as VH
import View.Types exposing (ImageTag)
import Widget
import Widget.Style.Material


imagesIfLoaded : Project -> ImageListViewParams -> SortTableParams -> Element.Element Msg
imagesIfLoaded project imageListViewParams sortTableParams =
    if List.isEmpty project.images then
        Element.row [ Element.spacing 15 ]
            [ Widget.circularProgressIndicator Style.Theme.materialStyle.progressIndicator Nothing
            , Element.text "Images loading..."
            ]

    else
        images project imageListViewParams sortTableParams


projectOwnsImage : Project -> OSTypes.Image -> Bool
projectOwnsImage project image =
    image.projectUuid == project.auth.project.uuid


filterByOwner : Bool -> Project -> List OSTypes.Image -> List OSTypes.Image
filterByOwner onlyOwnImages project someImages =
    if not onlyOwnImages then
        someImages

    else
        List.filter (projectOwnsImage project) someImages


filterByTags : Set.Set String -> List OSTypes.Image -> List OSTypes.Image
filterByTags tagsToFilterBy someImages =
    if tagsToFilterBy == Set.empty then
        someImages

    else
        List.filter
            (\i ->
                let
                    imageTags =
                        Set.fromList i.tags
                in
                Set.Extra.subset tagsToFilterBy imageTags
            )
            someImages


filterBySearchText : String -> List OSTypes.Image -> List OSTypes.Image
filterBySearchText searchText someImages =
    if searchText == "" then
        someImages

    else
        List.filter (\i -> String.contains (String.toUpper searchText) (String.toUpper i.name)) someImages


filterImages : ImageListViewParams -> Project -> List OSTypes.Image -> List OSTypes.Image
filterImages imageListViewParams project someImages =
    someImages
        |> filterByOwner imageListViewParams.onlyOwnImages project
        |> filterByTags imageListViewParams.tags
        |> filterBySearchText imageListViewParams.searchText


images : Project -> ImageListViewParams -> SortTableParams -> Element.Element Msg
images project imageListViewParams sortTableParams =
    let
        generateAllTags : List OSTypes.Image -> List ImageTag
        generateAllTags someImages =
            List.map (\i -> i.tags) someImages
                |> List.concat
                |> List.sort
                |> List.Extra.gatherEquals
                |> List.map (\t -> { label = Tuple.first t, frequency = 1 + List.length (Tuple.second t) })
                |> List.sortBy .frequency
                |> List.reverse

        filteredImages =
            project.images |> filterImages imageListViewParams project

        tagsAfterFilteringImages =
            generateAllTags filteredImages

        noMatchWarning =
            (imageListViewParams.tags /= Set.empty) && (List.length filteredImages == 0)

        tagView : ImageTag -> Element.Element Msg
        tagView tag =
            let
                iconFunction checked =
                    if checked then
                        Element.none

                    else
                        Icon.plusCircle (Element.rgb255 10 10 10) 12

                tagChecked =
                    Set.member tag.label imageListViewParams.tags

                checkboxLabel =
                    tag.label ++ " (" ++ String.fromInt tag.frequency ++ ")"
            in
            if tagChecked then
                Element.none

            else
                Input.checkbox [ Element.paddingXY 10 5 ]
                    { checked = tagChecked
                    , onChange =
                        \_ ->
                            ProjectMsg project.auth.project.uuid <|
                                SetProjectView <|
                                    ListImages { imageListViewParams | tags = Set.Extra.toggle tag.label imageListViewParams.tags } sortTableParams
                    , icon = iconFunction
                    , label = Input.labelRight [] (Element.text checkboxLabel)
                    }

        tagChipView : ImageTag -> Element.Element Msg
        tagChipView tag =
            let
                tagChecked =
                    Set.member tag.label imageListViewParams.tags

                chipLabel =
                    Element.text tag.label

                unselectTag =
                    ProjectMsg project.auth.project.uuid <|
                        SetProjectView <|
                            ListImages
                                { imageListViewParams
                                    | tags = Set.remove tag.label imageListViewParams.tags
                                }
                                sortTableParams
            in
            if tagChecked then
                chip (Just unselectTag) chipLabel

            else
                Element.none

        tagsView =
            Element.column [ Element.spacing 10 ]
                [ Element.text "Filtering on these tags:"
                , Element.wrappedRow
                    [ Element.height Element.shrink
                    , Element.width Element.shrink
                    ]
                    (List.map tagChipView tagsAfterFilteringImages)
                , Element.text "Select tags to filter images on:"
                , Element.wrappedRow []
                    (List.map tagView tagsAfterFilteringImages)
                ]
    in
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.width Element.fill ]
        )
        [ Element.el VH.heading2 (Element.text "Choose an image")
        , Input.text []
            { text = imageListViewParams.searchText
            , placeholder = Just (Input.placeholder [] (Element.text "try \"Ubuntu\""))
            , onChange =
                \t ->
                    ProjectMsg project.auth.project.uuid <|
                        SetProjectView <|
                            ListImages
                                { imageListViewParams | searchText = t }
                                sortTableParams
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Filter on image name:")
            }
        , tagsView
        , Input.checkbox []
            { checked = imageListViewParams.onlyOwnImages
            , onChange =
                \new ->
                    ProjectMsg project.auth.project.uuid <|
                        SetProjectView <|
                            ListImages { imageListViewParams | onlyOwnImages = new }
                                sortTableParams
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (Element.text "Show only images owned by this project")
            }
        , Widget.textButton
            (Widget.Style.Material.textButton Style.Theme.exoPalette)
            { text = "Clear filters (show all)"
            , onPress =
                Just <|
                    ProjectMsg project.auth.project.uuid <|
                        SetProjectView <|
                            ListImages
                                { searchText = ""
                                , tags = Set.empty
                                , onlyOwnImages = False
                                , expandImageDetails = Set.empty
                                }
                                sortTableParams
            }
        , if noMatchWarning then
            Element.text "No matches found. Broaden your search terms, or clear the search filter."

          else
            Element.none
        , List.map (renderImage project imageListViewParams sortTableParams) filteredImages
            |> Widget.column
                (Style.Theme.materialStyle.column
                    |> (\x ->
                            { x
                                | containerColumn =
                                    Style.Theme.materialStyle.column.containerColumn
                                        ++ [ Element.width Element.fill
                                           , Element.padding 0
                                           ]
                                , element =
                                    Style.Theme.materialStyle.column.element
                                        ++ [ Element.width Element.fill
                                           ]
                            }
                       )
                )
        ]


renderImage : Project -> ImageListViewParams -> SortTableParams -> OSTypes.Image -> Element.Element Msg
renderImage project imageListViewParams sortTableParams image =
    let
        imageDetailsExpanded =
            Set.member image.uuid imageListViewParams.expandImageDetails

        expandImageDetailsButton : Element.Element Msg
        expandImageDetailsButton =
            let
                iconFunction checked =
                    let
                        featherIcon =
                            if checked then
                                FeatherIcons.chevronDown

                            else
                                FeatherIcons.chevronRight
                    in
                    featherIcon |> FeatherIcons.toHtml [] |> Element.html

                checkboxLabel =
                    ""
            in
            Element.el
                [ Element.alignLeft
                , Element.centerY
                , Element.width Element.shrink
                ]
                (Input.checkbox [ Element.paddingXY 10 5 ]
                    { checked = imageDetailsExpanded
                    , onChange =
                        \_ ->
                            ProjectMsg project.auth.project.uuid <|
                                SetProjectView <|
                                    ListImages
                                        { imageListViewParams
                                            | expandImageDetails =
                                                Set.Extra.toggle image.uuid imageListViewParams.expandImageDetails
                                        }
                                        sortTableParams
                    , icon = iconFunction
                    , label = Input.labelRight [] (Element.text checkboxLabel)
                    }
                )

        size =
            "("
                ++ (case image.size of
                        Just s ->
                            Filesize.format s

                        Nothing ->
                            "size unknown"
                   )
                ++ ")"

        chooseMsg =
            ProjectMsg project.auth.project.uuid <|
                SetProjectView <|
                    CreateServer <|
                        Defaults.createServerViewParams
                            image.uuid
                            image.name
                            (project.userAppProxyHostname |> Maybe.map (\_ -> True))

        tagChip tag =
            Element.el [ Element.paddingXY 5 0 ]
                (Widget.button Style.Theme.materialStyle.chipButton
                    { text = tag
                    , icon = Element.none
                    , onPress =
                        Nothing
                    }
                )

        chooseButton =
            Widget.textButton
                (Widget.Style.Material.containedButton Style.Theme.exoPalette)
                { text = "Choose"
                , onPress =
                    case image.status of
                        OSTypes.ImageActive ->
                            Just chooseMsg

                        _ ->
                            Nothing
                }

        ownerBadge =
            if projectOwnsImage project image then
                ExoCard.badge "belongs to this project"

            else
                Element.none

        imageBriefView =
            Element.row
                [ Element.width Element.fill
                , Element.spacingXY 0 0
                ]
                [ Element.wrappedRow
                    [ Element.width Element.fill
                    ]
                    [ expandImageDetailsButton
                    , Element.el
                        [ Font.bold
                        , Element.padding 5
                        ]
                        (Element.text image.name)
                    , Element.el
                        [ Font.color <| Element.rgb255 122 122 122
                        , Element.padding 5
                        ]
                        (Element.text size)
                    , ownerBadge
                    ]
                , chooseButton
                ]

        imageDetailsView =
            Element.column []
                [ Element.wrappedRow
                    [ Element.width Element.fill
                    ]
                    (Element.el
                        [ Font.color <| Element.rgb255 122 122 122
                        , Element.padding 5
                        ]
                        (Element.text "Tags:")
                        :: List.map
                            tagChip
                            image.tags
                    )
                ]
    in
    Widget.column
        (Style.Theme.materialStyle.cardColumn
            |> (\x ->
                    { x
                        | containerColumn =
                            Style.Theme.materialStyle.cardColumn.containerColumn
                                ++ [ Element.padding 0
                                   ]
                        , element =
                            Style.Theme.materialStyle.cardColumn.element
                                ++ [ Element.padding 3
                                   ]
                    }
               )
        )
        (if imageDetailsExpanded then
            [ imageBriefView
            , imageDetailsView
            ]

         else
            [ imageBriefView ]
        )
