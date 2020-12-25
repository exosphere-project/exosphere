module View.GetSupport exposing (getSupport, viewStateToSupportableItem)

import Element
import Element.Background as Background
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import RemoteData
import SearchBox
import Style.Types
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , ProjectIdentifier
        , ProjectViewConstructor(..)
        , SupportableItemType(..)
        , ViewState(..)
        )
import View.Helpers as VH


getSupport :
    Model
    -> Style.Types.ExoPalette
    -> Maybe SupportableItemType
    -> Element.Element Msg
getSupport model palette maybeSupportableResource =
    Element.column VH.exoColumnAttributes
        [ Input.radio
            VH.exoColumnAttributes
            { onChange =
                \option ->
                    option
                        |> GetSupport
                        |> SetNonProjectView
            , selected =
                case maybeSupportableResource of
                    Just supportableItemType ->
                        Just <|
                            case supportableItemType of
                                -- Ugh.
                                SupportableServer _ _ ->
                                    Just <| SupportableServer newSearchBoxStateAndText Nothing

                                SupportableVolume _ _ ->
                                    Just <| SupportableVolume newSearchBoxStateAndText Nothing

                                SupportableImage _ _ ->
                                    Just <| SupportableImage newSearchBoxStateAndText Nothing

                                SupportableProject _ ->
                                    Just <| SupportableProject Nothing

                    Nothing ->
                        Just Nothing
            , label = Input.labelAbove [] (Element.text "What do you need help with?")
            , options =
                [ Input.option (Just <| SupportableServer newSearchBoxStateAndText Nothing) (Element.text "A server")
                , Input.option (Just <| SupportableVolume newSearchBoxStateAndText Nothing) (Element.text "A volume")
                , Input.option (Just <| SupportableImage newSearchBoxStateAndText Nothing) (Element.text "An image")
                , Input.option (Just <| SupportableProject Nothing) (Element.text "A project")
                , Input.option Nothing (Element.text "None of these things")
                ]
            }
        , case maybeSupportableResource of
            Nothing ->
                Element.none

            Just supportableItemType ->
                case supportableItemType of
                    SupportableProject maybeSupportableItemUuid ->
                        Element.none

                    {-
                       Input.radio
                           VH.exoColumnAttributes
                           { onChange =
                               \projectUuid ->
                                   SetNonProjectView <| GetSupport (Just ( SupportableProject, searchBoxState, Just projectUuid ))
                           , selected = maybeSupportableItemUuid
                           , label = Input.labelAbove [] (Element.text "Which project do you need help with?")
                           , options =
                               model.projects
                                   |> List.map
                                       (\project ->
                                           Input.option
                                               project.auth.project.uuid
                                               (Element.text <| VH.friendlyProjectTitle model project)
                                       )
                           }
                    -}
                    SupportableImage searchBoxStateAndText maybeSupportableItemUuid ->
                        let
                            imageNameFromUuid : OSTypes.ImageUuid -> String
                            imageNameFromUuid uuid =
                                model.projects
                                    |> List.map (\p -> GetterSetters.imageLookup p uuid)
                                    |> List.filterMap identity
                                    |> List.head
                                    |> Maybe.map .name
                                    |> Maybe.withDefault uuid
                        in
                        Element.el [] <|
                            SearchBox.input
                                VH.searchBoxAttributes
                                { onChange =
                                    \changeEvent ->
                                        case Debug.log "Change event" changeEvent of
                                            SearchBox.SelectionChanged imageUuid ->
                                                SetNonProjectView <|
                                                    GetSupport <|
                                                        Just
                                                            (SupportableImage
                                                                searchBoxStateAndText
                                                                (Just imageUuid)
                                                            )

                                            SearchBox.TextChanged newText ->
                                                SetNonProjectView <|
                                                    GetSupport <|
                                                        Just
                                                            (SupportableImage
                                                                { searchBoxStateAndText
                                                                    | searchBoxText = newText
                                                                    , searchBoxState = SearchBox.reset searchBoxStateAndText.searchBoxState
                                                                }
                                                                Nothing
                                                            )

                                            SearchBox.SearchBoxChanged searchBoxMsg ->
                                                let
                                                    newSearchBoxState =
                                                        SearchBox.update
                                                            searchBoxMsg
                                                            searchBoxStateAndText.searchBoxState
                                                in
                                                SetNonProjectView <|
                                                    GetSupport <|
                                                        Just
                                                            (SupportableImage
                                                                { searchBoxStateAndText
                                                                    | searchBoxState = newSearchBoxState
                                                                }
                                                                maybeSupportableItemUuid
                                                            )
                                , text = searchBoxStateAndText.searchBoxText
                                , selected = maybeSupportableItemUuid
                                , options =
                                    model.projects
                                        |> List.map .images
                                        |> List.concat
                                        |> List.map (\image -> image.uuid)
                                        |> Just
                                , label =
                                    Input.labelAbove [] (Element.text "Which image do you need help with?")
                                , placeholder = Nothing
                                , toLabel = imageNameFromUuid
                                , filter =
                                    \filterString imageUuid ->
                                        String.contains
                                            (String.toLower filterString)
                                            (String.toLower <| imageNameFromUuid imageUuid)
                                , state = searchBoxStateAndText.searchBoxState
                                }

                    {-
                       Input.radio
                           VH.exoColumnAttributes
                           { onChange =
                               \imageUuid ->
                                   SetNonProjectView <| GetSupport (Just ( SupportableImage, searchBoxState, Just imageUuid ))
                           , selected = maybeSupportableItemUuid
                           , label = Input.labelAbove [] (Element.text "Which image do you need help with?")
                           , options =
                               model.projects
                                   |> List.map .images
                                   |> List.concat
                                   |> List.map (\image -> ( image.uuid, image.name ))
                                   |> List.map
                                       (\uuidNameTuple ->
                                           Input.option
                                               (Tuple.first uuidNameTuple)
                                               (Element.text <| Tuple.second uuidNameTuple)
                                       )
                           }
                    -}
                    SupportableServer searchBoxStateAndText maybeSupportableItemUuid ->
                        Element.none

                    {-
                       Input.radio
                           VH.exoColumnAttributes
                           { onChange =
                               \serverUuid ->
                                   SetNonProjectView <| GetSupport (Just ( SupportableServer, searchBoxState, Just serverUuid ))
                           , selected = maybeSupportableItemUuid
                           , label = Input.labelAbove [] (Element.text "Which server do you need help with?")
                           , options =
                               model.projects
                                   |> List.map .servers
                                   |> List.map (RDPP.withDefault [])
                                   |> List.concat
                                   |> List.map (\server -> ( server.osProps.uuid, server.osProps.name ))
                                   |> List.map
                                       (\uuidNameTuple ->
                                           Input.option
                                               (Tuple.first uuidNameTuple)
                                               (Element.text <| Tuple.second uuidNameTuple)
                                       )
                           }
                    -}
                    SupportableVolume searchBoxStateAndText maybeSupportableItemUuid ->
                        Element.none

        {-
           Input.radio
               VH.exoColumnAttributes
               { onChange =
                   \volumeUuid ->
                       SetNonProjectView <| GetSupport (Just ( SupportableVolume, searchBoxState, Just volumeUuid ))
               , selected = maybeSupportableItemUuid
               , label = Input.labelAbove [] (Element.text "Which volume do you need help with?")
               , options =
                   model.projects
                       |> List.map .volumes
                       |> List.map (RemoteData.withDefault [])
                       |> List.concat
                       |> List.map (\volume -> ( volume.uuid, volume.name ))
                       |> List.map
                           (\uuidNameTuple ->
                               Input.option
                                   (Tuple.first uuidNameTuple)
                                   (Element.text <| Tuple.second uuidNameTuple)
                           )
               }
        -}
        ]


supportableItemTypeStr : SupportableItemType -> String
supportableItemTypeStr supportableItemType =
    case supportableItemType of
        SupportableProject _ ->
            "project"

        SupportableImage _ _ ->
            "image"

        SupportableServer _ _ ->
            "server"

        SupportableVolume _ _ ->
            "volume"


newSearchBoxStateAndText : { searchBoxState : SearchBox.State, searchBoxText : String }
newSearchBoxStateAndText =
    { searchBoxState = SearchBox.init, searchBoxText = "" }


viewStateToSupportableItem : ViewState -> Maybe SupportableItemType
viewStateToSupportableItem viewState =
    let
        supportableProjectItem :
            ProjectIdentifier
            -> ProjectViewConstructor
            -> SupportableItemType
        supportableProjectItem projectUuid projectViewConstructor =
            case projectViewConstructor of
                CreateServer createServerViewParams ->
                    SupportableImage newSearchBoxStateAndText (Just createServerViewParams.imageUuid)

                ServerDetail serverUuid _ ->
                    SupportableServer newSearchBoxStateAndText (Just serverUuid)

                CreateServerImage serverUuid _ ->
                    SupportableServer newSearchBoxStateAndText (Just serverUuid)

                VolumeDetail volumeUuid _ ->
                    SupportableVolume newSearchBoxStateAndText (Just volumeUuid)

                AttachVolumeModal _ maybeVolumeUuid ->
                    maybeVolumeUuid
                        |> Maybe.map (\uuid -> SupportableVolume newSearchBoxStateAndText (Just uuid))
                        |> Maybe.withDefault (SupportableProject (Just projectUuid))

                MountVolInstructions attachment ->
                    SupportableServer newSearchBoxStateAndText (Just attachment.serverUuid)

                _ ->
                    SupportableProject (Just projectUuid)
    in
    case viewState of
        NonProjectView _ ->
            Nothing

        ProjectView projectUuid _ projectViewConstructor ->
            Just <| supportableProjectItem projectUuid projectViewConstructor
