module View.Images exposing (imagesIfLoaded)

import Element
import Element.Font as Font
import Element.Input as Input
import Filesize
import Helpers.String
import List.Extra
import OpenStack.Types as OSTypes
import Set
import Set.Extra
import Style.Helpers as SH
import Style.Widgets.Card as ExoCard
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton exposing (chip)
import Types.Defaults as Defaults
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..))
import Types.Types exposing (Project)
import Types.View exposing (ImageListViewParams, ImageListVisibilityFilter, ProjectViewConstructor(..), SortTableParams)
import View.Helpers as VH
import View.Types exposing (ImageTag)
import Widget


imagesIfLoaded : View.Types.Context -> Project -> ImageListViewParams -> SortTableParams -> Element.Element Msg
imagesIfLoaded context project imageListViewParams sortTableParams =
    if List.isEmpty project.images then
        Element.row [ Element.spacing 15 ]
            [ Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing
            , Element.text <|
                String.join " "
                    [ context.localization.staticRepresentationOfBlockDeviceContents
                        |> Helpers.String.toTitleCase
                        |> Helpers.String.pluralize
                    , "loading..."
                    ]
            ]

    else
        images context project imageListViewParams sortTableParams


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


filterByVisibility : ImageListVisibilityFilter -> List OSTypes.Image -> List OSTypes.Image
filterByVisibility filter someImages =
    let
        include i =
            List.any identity
                [ i.visibility == OSTypes.ImagePublic && filter.public
                , i.visibility == OSTypes.ImagePrivate && filter.private
                , i.visibility == OSTypes.ImageCommunity && filter.community
                , i.visibility == OSTypes.ImageShared && filter.shared
                ]
    in
    List.filter include someImages


isImageFeaturedByDeployer : Maybe String -> OSTypes.Image -> Bool
isImageFeaturedByDeployer maybeFeaturedImageNamePrefix image =
    case maybeFeaturedImageNamePrefix of
        Nothing ->
            False

        Just featuredImageNamePrefix ->
            String.startsWith featuredImageNamePrefix image.name && image.visibility == OSTypes.ImagePublic


filterImages : ImageListViewParams -> Project -> List OSTypes.Image -> List OSTypes.Image
filterImages imageListViewParams project someImages =
    someImages
        |> filterByOwner imageListViewParams.onlyOwnImages project
        |> filterByTags imageListViewParams.tags
        |> filterBySearchText imageListViewParams.searchText
        |> filterByVisibility imageListViewParams.visibilityFilter


images : View.Types.Context -> Project -> ImageListViewParams -> SortTableParams -> Element.Element Msg
images context project imageListViewParams sortTableParams =
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

        featuredImageNamePrefix =
            VH.featuredImageNamePrefixLookup context project

        ( featuredImages, nonFeaturedImages_ ) =
            List.partition (isImageFeaturedByDeployer featuredImageNamePrefix) filteredImages

        ( ownImages, otherImages ) =
            List.partition (\i -> projectOwnsImage project i) nonFeaturedImages_

        combinedImages =
            List.concat [ featuredImages, ownImages, otherImages ]

        tagView : ImageTag -> Element.Element Msg
        tagView tag =
            let
                iconFunction checked =
                    if checked then
                        Element.none

                    else
                        Icon.plusCircle (SH.toElementColor context.palette.on.background) 12

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
                chip context.palette (Just unselectTag) chipLabel

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
                , Element.text <|
                    String.join " "
                        [ "Select tags to filter"
                        , context.localization.staticRepresentationOfBlockDeviceContents
                            |> Helpers.String.pluralize
                        , "on:"
                        ]
                , Element.wrappedRow []
                    (List.map tagView tagsAfterFilteringImages)
                ]

        imagesColumnView =
            Widget.column
                ((SH.materialStyle context.palette).column
                    |> (\x ->
                            { x
                                | containerColumn =
                                    (SH.materialStyle context.palette).column.containerColumn
                                        ++ [ Element.width Element.fill
                                           , Element.padding 0
                                           ]
                                , element =
                                    (SH.materialStyle context.palette).column.element
                                        ++ [ Element.width Element.fill
                                           ]
                            }
                       )
                )

        visibilityFilters =
            Element.row
                [ Element.spacing 10 ]
                [ Element.text <|
                    String.join " "
                        [ "Filter on"
                        , context.localization.staticRepresentationOfBlockDeviceContents
                        , "visibility:"
                        ]
                , Input.checkbox []
                    { checked = imageListViewParams.visibilityFilter.public
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    imageListViewParams.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | public = new }
                            in
                            ProjectMsg project.auth.project.uuid <|
                                SetProjectView <|
                                    ListImages { imageListViewParams | visibilityFilter = newVisibilityFilter }
                                        sortTableParams
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Public"
                    }
                , Input.checkbox []
                    { checked = imageListViewParams.visibilityFilter.community
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    imageListViewParams.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | community = new }
                            in
                            ProjectMsg project.auth.project.uuid <|
                                SetProjectView <|
                                    ListImages { imageListViewParams | visibilityFilter = newVisibilityFilter }
                                        sortTableParams
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Community"
                    }
                , Input.checkbox []
                    { checked = imageListViewParams.visibilityFilter.shared
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    imageListViewParams.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | shared = new }
                            in
                            ProjectMsg project.auth.project.uuid <|
                                SetProjectView <|
                                    ListImages { imageListViewParams | visibilityFilter = newVisibilityFilter }
                                        sortTableParams
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Shared"
                    }
                , Input.checkbox []
                    { checked = imageListViewParams.visibilityFilter.private
                    , onChange =
                        \new ->
                            let
                                oldVisibilityFilter =
                                    imageListViewParams.visibilityFilter

                                newVisibilityFilter =
                                    { oldVisibilityFilter | private = new }
                            in
                            ProjectMsg project.auth.project.uuid <|
                                SetProjectView <|
                                    ListImages { imageListViewParams | visibilityFilter = newVisibilityFilter }
                                        sortTableParams
                    , icon = Input.defaultCheckbox
                    , label =
                        Input.labelRight [] <|
                            Element.text "Private"
                    }
                ]
    in
    Element.column
        (VH.exoColumnAttributes
            ++ [ Element.width Element.fill ]
        )
        [ Element.el (VH.heading2 context.palette)
            (Element.text <|
                String.join " "
                    [ "Choose"
                    , Helpers.String.indefiniteArticle context.localization.staticRepresentationOfBlockDeviceContents
                    , context.localization.staticRepresentationOfBlockDeviceContents
                    ]
            )
        , Element.column VH.contentContainer
            [ Input.text (VH.inputItemAttributes context.palette.background)
                { text = imageListViewParams.searchText
                , placeholder = Just (Input.placeholder [] (Element.text "try \"Ubuntu\""))
                , onChange =
                    \t ->
                        ProjectMsg project.auth.project.uuid <|
                            SetProjectView <|
                                ListImages
                                    { imageListViewParams | searchText = t }
                                    sortTableParams
                , label =
                    Input.labelAbove []
                        (Element.text <|
                            String.join " "
                                [ "Filter on"
                                , context.localization.staticRepresentationOfBlockDeviceContents
                                , "name:"
                                ]
                        )
                }
            , visibilityFilters
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
                , label =
                    Input.labelRight [] <|
                        Element.text <|
                            String.join
                                " "
                                [ "Show only"
                                , context.localization.staticRepresentationOfBlockDeviceContents
                                    |> Helpers.String.pluralize
                                , "owned by this"
                                , context.localization.unitOfTenancy
                                ]
                }
            , Widget.textButton
                (SH.materialStyle context.palette).button
                { text = "Clear filters (show all)"
                , onPress =
                    Just <|
                        ProjectMsg project.auth.project.uuid <|
                            SetProjectView <|
                                ListImages
                                    Defaults.imageListViewParams
                                    sortTableParams
                }
            , if noMatchWarning then
                Element.text "No matches found. Broaden your search terms, or clear the search filter."

              else
                Element.none
            , List.map (renderImage context project imageListViewParams sortTableParams) combinedImages
                |> imagesColumnView
            ]
        ]


renderImage : View.Types.Context -> Project -> ImageListViewParams -> SortTableParams -> OSTypes.Image -> Element.Element Msg
renderImage context project imageListViewParams sortTableParams image =
    let
        imageDetailsExpanded =
            Set.member image.uuid imageListViewParams.expandImageDetails

        size =
            case image.size of
                Just s ->
                    Filesize.format s

                Nothing ->
                    "size unknown"

        chooseMsg =
            ProjectMsg project.auth.project.uuid <|
                SetProjectView <|
                    CreateServer <|
                        Defaults.createServerViewParams
                            image.uuid
                            image.name
                            (VH.userAppProxyLookup context project
                                |> Maybe.map (\_ -> True)
                            )

        tagChip tag =
            Element.el [ Element.paddingXY 5 0 ]
                (Widget.button (SH.materialStyle context.palette).chipButton
                    { text = tag
                    , icon = Element.none
                    , onPress =
                        Nothing
                    }
                )

        chooseButton =
            Widget.textButton
                (SH.materialStyle context.palette).primaryButton
                { text = "Choose"
                , onPress =
                    case image.status of
                        OSTypes.ImageActive ->
                            Just chooseMsg

                        _ ->
                            Nothing
                }

        featuredImageNamePrefix =
            VH.featuredImageNamePrefixLookup context project

        featuredBadge =
            if isImageFeaturedByDeployer featuredImageNamePrefix image then
                ExoCard.badge "featured"

            else
                Element.none

        ownerBadge =
            if projectOwnsImage project image then
                ExoCard.badge <|
                    String.join " "
                        [ "belongs to this"
                        , context.localization.unitOfTenancy
                        ]

            else
                Element.none

        title =
            Element.row
                [ Element.width Element.fill
                ]
                [ Element.el
                    [ Font.bold
                    , Element.padding 5
                    ]
                    (Element.text image.name)
                , featuredBadge
                , ownerBadge
                ]

        subtitle =
            Element.row
                []
                [ Element.el
                    [ Font.color <| SH.toElementColor <| context.palette.muted
                    , Element.padding 5
                    ]
                    (Element.text size)
                ]

        imageDetailsView =
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.width Element.fill ]
                )
                [ Element.wrappedRow
                    [ Element.width Element.fill
                    ]
                    [ Element.el
                        [ Font.color <| SH.toElementColor <| context.palette.muted
                        , Element.padding 5
                        ]
                        (Element.text <| "Visibility: " ++ OSTypes.imageVisibilityToString image.visibility)
                    ]
                , Element.wrappedRow
                    [ Element.width Element.fill
                    ]
                    (Element.el
                        [ Font.color <| SH.toElementColor <| context.palette.muted
                        , Element.padding 5
                        ]
                        (Element.text "Tags:")
                        :: List.map
                            tagChip
                            image.tags
                    )
                , Element.el
                    [ Element.alignRight ]
                    chooseButton
                ]
    in
    ExoCard.expandoCard
        context.palette
        imageDetailsExpanded
        (\_ ->
            ProjectMsg project.auth.project.uuid <|
                SetProjectView <|
                    ListImages
                        { imageListViewParams
                            | expandImageDetails =
                                Set.Extra.toggle image.uuid imageListViewParams.expandImageDetails
                        }
                        sortTableParams
        )
        title
        subtitle
        imageDetailsView
