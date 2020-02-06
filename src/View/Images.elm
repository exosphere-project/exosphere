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


images : GlobalDefaults -> Project -> ImageFilter -> Element.Element Msg
images globalDefaults project imageFilter =
    let
        imageContainsTag tag image =
            if tag == "" then
                True

            else
                List.member tag image.tags

        imageMatchesSearchText searchText image =
            if searchText == "" then
                True

            else
                String.contains (String.toUpper searchText) (String.toUpper image.name)

        filteredImagesByOwner =
            if imageFilter.onlyOwnImages then
                List.filter (\i -> imageIsOwnedByProject i project) project.images

            else
                project.images

        filteredImagesByTag =
            List.filter (imageContainsTag imageFilter.tag) filteredImagesByOwner

        filteredImagesBySearchText =
            List.filter (imageMatchesSearchText imageFilter.searchText) filteredImagesByTag

        filteredImages =
            filteredImagesBySearchText

        noMatchWarning =
            (imageFilter.tag /= "") && (List.length filteredImages == 0)

        displayedImages =
            filteredImages

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
            , label = Input.labelRight [] (Element.text "Show only images owned by me")
            }
        , Button.button [] (Just <| ProjectMsg projectId <| SetProjectView <| ListImages { searchText = "", tag = "", onlyOwnImages = False }) "Clear filter (show all)"
        , if noMatchWarning then
            Element.text "No matches found. Broaden your search terms, or clear the search filter."

          else
            Element.none
        , Element.wrappedRow
            (VH.exoRowAttributes ++ [ Element.spacing 15 ])
            (List.map (renderImage globalDefaults project) displayedImages)
        ]


imageIsOwnedByProject : OSTypes.Image -> Project -> Bool
imageIsOwnedByProject image project =
    image.projectUuid == project.auth.project.uuid


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
                            "changeme123"
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
            if imageIsOwnedByProject image project then
                [ Element.row VH.exoRowAttributes
                    [ Element.image [ Element.paddingXY 10 0 ] { src = "assets/img/created-by-you-badge.svg", description = "" }
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
