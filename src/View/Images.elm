module View.Images exposing (imagesIfLoaded)

import Element
import Element.Font as Font
import Element.Input as Input
import Filesize
import Framework.Button as Button
import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import Style.Widgets.Card as ExoCard
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


filterByTag : String -> List OSTypes.Image -> List OSTypes.Image
filterByTag tag someImages =
    if tag == "" then
        someImages

    else
        List.filter (\i -> List.member tag i.tags) someImages


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
        |> filterByTag imageFilter.tag
        |> filterBySearchText imageFilter.searchText


images : GlobalDefaults -> Project -> ImageFilter -> Element.Element Msg
images globalDefaults project imageFilter =
    let
        filteredImages =
            project.images |> filterImages imageFilter project

        noMatchWarning =
            (imageFilter.tag /= "") && (List.length filteredImages == 0)

        projectId =
            Helpers.getProjectId project
    in
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 (Element.text "Choose an image")
        , Input.text []
            { text = imageFilter.searchText
            , placeholder = Just (Input.placeholder [] (Element.text "try \"Ubuntu\""))
            , onChange = \t -> ProjectMsg projectId <| SetProjectView <| ListImages { imageFilter | searchText = t }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Filter on image name:")
            }
        , Input.text []
            { text = imageFilter.tag
            , placeholder = Just (Input.placeholder [] (Element.text "try \"distro-base\""))
            , onChange = \t -> ProjectMsg projectId <| SetProjectView <| ListImages { imageFilter | tag = t }
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Filter on tag:")
            }
        , Input.checkbox []
            { checked = imageFilter.onlyOwnImages
            , onChange = \new -> ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| ListImages { imageFilter | onlyOwnImages = new }
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (Element.text "Show only images owned by this project")
            }
        , Button.button [] (Just <| ProjectMsg projectId <| SetProjectView <| ListImages { searchText = "", tag = "", onlyOwnImages = False }) "Clear filter (show all)"
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
