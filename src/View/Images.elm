module View.Images exposing (imagesIfLoaded)

import Color
import Element
import Element.Font as Font
import Element.Input as Input
import Filesize
import Framework.Button as Button
import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import List.Extra
import OpenStack.Types as OSTypes
import Set
import Set.Extra
import Style.Widgets.Card as ExoCard
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton exposing (chip)
import Types.Types
    exposing
        ( CreateServerRequest
        , GlobalDefaults
        , ImageFilter
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH
import View.Types exposing (ImageTag)


imagesIfLoaded : GlobalDefaults -> Project -> ImageFilter -> Element.Element Msg
imagesIfLoaded globalDefaults project imageFilter =
    if List.isEmpty project.images then
        Element.text "Images loading"

    else
        images globalDefaults project imageFilter


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


filterImages : ImageFilter -> Project -> List OSTypes.Image -> List OSTypes.Image
filterImages imageFilter project someImages =
    someImages
        |> filterByOwner imageFilter.onlyOwnImages project
        |> filterByTags imageFilter.tags
        |> filterBySearchText imageFilter.searchText


images : GlobalDefaults -> Project -> ImageFilter -> Element.Element Msg
images globalDefaults project imageFilter =
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
            project.images |> filterImages imageFilter project

        tagsAfterFilteringImages =
            generateAllTags filteredImages

        noMatchWarning =
            (imageFilter.tags /= Set.empty) && (List.length filteredImages == 0)

        projectId =
            Helpers.getProjectId project

        tagView : ImageTag -> Element.Element Msg
        tagView tag =
            let
                iconFunction checked =
                    if checked then
                        Element.none

                    else
                        Icon.plusCircle Color.black 12

                tagChecked =
                    Set.member tag.label imageFilter.tags

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
                            ProjectMsg projectId <|
                                SetProjectView <|
                                    ListImages { imageFilter | tags = Set.Extra.toggle tag.label imageFilter.tags }
                    , icon = iconFunction
                    , label = Input.labelRight [] (Element.text checkboxLabel)
                    }

        tagChipView : ImageTag -> Element.Element Msg
        tagChipView tag =
            let
                tagChecked =
                    Set.member tag.label imageFilter.tags

                chipLabel =
                    Element.text tag.label

                unselectTag =
                    ProjectMsg projectId <| SetProjectView <| ListImages { imageFilter | tags = Set.remove tag.label imageFilter.tags }
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
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 (Element.text "Choose an image")
        , Input.text []
            { text = imageFilter.searchText
            , placeholder = Just (Input.placeholder [] (Element.text "try \"Ubuntu\""))
            , onChange = \t -> ProjectMsg projectId <| SetProjectView <| ListImages { imageFilter | searchText = t }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Filter on image name:")
            }
        , tagsView
        , Input.checkbox []
            { checked = imageFilter.onlyOwnImages
            , onChange = \new -> ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| ListImages { imageFilter | onlyOwnImages = new }
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (Element.text "Show only images owned by this project")
            }
        , Button.button []
            (Just <|
                ProjectMsg projectId <|
                    SetProjectView <|
                        ListImages
                            { searchText = ""
                            , tags = Set.empty
                            , onlyOwnImages = False
                            }
            )
            "Clear filters (show all)"
        , if noMatchWarning then
            Element.text "No matches found. Broaden your search terms, or clear the search filter."

          else
            Element.none
        , Element.wrappedRow
            (VH.exoRowAttributes ++ [ Element.spacing 15 ])
            (List.map (renderImage globalDefaults project) filteredImages)
        ]


renderImage : GlobalDefaults -> Project -> OSTypes.Image -> Element.Element Msg
renderImage globalDefaults project image =
    let
        size =
            case image.size of
                Just s ->
                    Filesize.format s

                Nothing ->
                    "N/A"

        chooseMsg =
            ProjectMsg (Helpers.getProjectId project) <|
                SetProjectView <|
                    CreateServer <|
                        -- TODO this should not be hard-coded here
                        CreateServerRequest
                            image.name
                            (Helpers.getProjectId project)
                            image.uuid
                            image.name
                            1
                            ""
                            Nothing
                            Nothing
                            globalDefaults.shellUserData
                            ""
                            False

        chooseButton =
            case image.status of
                OSTypes.ImageActive ->
                    Button.button
                        [ Modifier.Primary ]
                        (Just chooseMsg)
                        "Choose"

                _ ->
                    Button.button
                        [ Modifier.Disabled ]
                        Nothing
                        "Choose"

        ownerRows =
            if projectOwnsImage project image then
                [ Element.row VH.exoRowAttributes
                    [ ExoCard.badge "belongs to this project"
                    ]
                ]

            else
                []
    in
    ExoCard.exoCard
        image.name
        size
    <|
        Element.column VH.exoColumnAttributes
            (ownerRows
                ++ [ Element.row VH.exoRowAttributes
                        [ Element.text "Status: "
                        , Element.text (Debug.toString image.status)
                        ]
                   , Element.row VH.exoRowAttributes
                        [ Element.text "Tags: "
                        , Element.paragraph [] [ Element.text (List.foldl (\a b -> a ++ ", " ++ b) "" image.tags) ]
                        ]
                   , Element.el
                        [ Element.alignRight ]
                        chooseButton
                   ]
            )
