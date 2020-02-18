module View.Images exposing (imagesIfLoaded)

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
import Style.Widgets.Card as ExoCard
import Types.Types
    exposing
        ( CreateServerRequest
        , GlobalDefaults
        , ImageFilter
        , ImageTag
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH


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
filterByTags tagLabels someImages =
    if tagLabels == Set.empty then
        someImages

    else
        List.filter (\i -> Set.intersect tagLabels (Set.fromList i.tags) |> (==) tagLabels) someImages


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


generateAllTags : List OSTypes.Image -> List ImageTag
generateAllTags someImages =
    List.map (\i -> i.tags) someImages
        |> List.concat
        |> List.sort
        |> List.Extra.gatherEquals
        |> List.map (\t -> { label = Tuple.first t, frequency = 1 + List.length (Tuple.second t) })
        |> List.sortBy .frequency
        |> List.reverse


images : GlobalDefaults -> Project -> ImageFilter -> Element.Element Msg
images globalDefaults project imageFilter =
    let
        tags =
            generateAllTags project.images

        filteredImages =
            project.images |> filterImages imageFilter project

        noMatchWarning =
            (imageFilter.tags /= Set.empty) && (List.length filteredImages == 0)

        projectId =
            Helpers.getProjectId project

        tagView : ImageTag -> Element.Element Msg
        tagView tag =
            let
                tagChecked =
                    Set.member tag.label imageFilter.tags

                checkboxLabel =
                    tag.label ++ " (" ++ String.fromInt tag.frequency ++ ")"

                toggleTags : Bool -> ImageTag -> Set.Set String -> Set.Set String
                toggleTags newBool someTag someTags =
                    if newBool then
                        Set.insert someTag.label someTags

                    else
                        Set.remove someTag.label someTags
            in
            Input.checkbox []
                { checked = tagChecked
                , onChange = \t -> ProjectMsg projectId <| SetProjectView <| ListImages { imageFilter | tags = toggleTags t tag imageFilter.tags }
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [] (Element.text checkboxLabel)
                }

        tagsView =
            Element.column []
                [ Element.text "Filter on tag:"
                , Element.paragraph []
                    (List.map tagView tags)
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
        , Button.button [] (Just <| ProjectMsg projectId <| SetProjectView <| ListImages { searchText = "", tags = Set.empty, onlyOwnImages = False }) "Clear filter (show all)"
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
