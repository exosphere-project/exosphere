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
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH


imagesIfLoaded : GlobalDefaults -> Project -> Maybe String -> Maybe String -> Element.Element Msg
imagesIfLoaded globalDefaults project maybeFilterTag maybeFilterSearchText =
    if List.isEmpty project.images then
        Element.text "Images loading"

    else
        images globalDefaults project maybeFilterTag maybeFilterSearchText


images : GlobalDefaults -> Project -> Maybe String -> Maybe String -> Element.Element Msg
images globalDefaults project maybeFilterTag maybeFilterSearchText =
    let
        imageContainsTag tag image =
            List.member tag image.tags

        imageMatchesSearchText searchText image =
            String.contains searchText image.name

        filteredImagesByTag =
            case maybeFilterTag of
                Nothing ->
                    project.images

                Just filterTag ->
                    List.filter (imageContainsTag filterTag) project.images

        filteredImagesBySearchText =
            case maybeFilterSearchText of
                Nothing ->
                    filteredImagesByTag

                Just filterSearchText ->
                    List.filter (imageMatchesSearchText filterSearchText) filteredImagesByTag

        filteredImages =
            filteredImagesBySearchText

        noMatchWarning =
            (maybeFilterTag /= Nothing) && (List.length filteredImages == 0)

        displayedImages =
            if noMatchWarning == False then
                filteredImages

            else
                project.images
    in
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 (Element.text "Choose an image")
        , Input.text []
            { text = Maybe.withDefault "" maybeFilterTag
            , placeholder = Just (Input.placeholder [] (Element.text "try \"distro-base\""))
            , onChange = \t -> InputImageFilterTag t
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Filter on tag:")
            }
        , Button.button [] (Just <| InputImageFilterTag "") "Clear filter (show all)"
        , if noMatchWarning then
            Element.text "No matches found, showing all images"

          else
            Element.none
        , Element.wrappedRow
            (VH.exoRowAttributes ++ [ Element.spacing 15 ])
            (List.map (renderImage globalDefaults project) displayedImages)
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
                            "1"
                            ""
                            False
                            ""
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
    in
    ExoCard.exoCard
        image.name
        size
    <|
        Element.column VH.exoColumnAttributes
            [ Element.row VH.exoRowAttributes
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
