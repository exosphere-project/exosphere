module View.GetSupport exposing (getSupport, viewStateToSupportableItem)

import Element
import Element.Input as Input
import Helpers.RemoteDataPlusPlus as RDPP
import RemoteData
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


getSupport : Model -> Style.Types.ExoPalette -> Maybe ( SupportableItemType, Maybe HelperTypes.Uuid ) -> Element.Element Msg
getSupport model palette maybeSupportableResource =
    Element.column VH.exoColumnAttributes
        [ Input.radio
            VH.exoColumnAttributes
            { onChange =
                \option ->
                    option
                        |> Maybe.map (\type_ -> Tuple.pair type_ Nothing)
                        |> GetSupport
                        |> SetNonProjectView
            , selected =
                case maybeSupportableResource of
                    Just ( supportableItemType, _ ) ->
                        Just <| Just supportableItemType

                    Nothing ->
                        Just Nothing
            , label = Input.labelAbove [] (Element.text "What do you need help with?")
            , options =
                [ Input.option (Just SupportableServer) (Element.text "A server")
                , Input.option (Just SupportableVolume) (Element.text "A volume")
                , Input.option (Just SupportableImage) (Element.text "An image")
                , Input.option (Just SupportableProject) (Element.text "A project")
                , Input.option Nothing (Element.text "None of these things")
                ]
            }
        , case maybeSupportableResource of
            Nothing ->
                Element.none

            Just ( supportableItemType, maybeSupportableItemUuid ) ->
                case supportableItemType of
                    SupportableProject ->
                        Input.radio
                            VH.exoColumnAttributes
                            { onChange =
                                \projectUuid ->
                                    SetNonProjectView <| GetSupport (Just ( SupportableProject, Just projectUuid ))
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

                    SupportableImage ->
                        Input.radio
                            VH.exoColumnAttributes
                            { onChange =
                                \imageUuid ->
                                    SetNonProjectView <| GetSupport (Just ( SupportableImage, Just imageUuid ))
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

                    SupportableServer ->
                        Input.radio
                            VH.exoColumnAttributes
                            { onChange =
                                \serverUuid ->
                                    SetNonProjectView <| GetSupport (Just ( SupportableServer, Just serverUuid ))
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

                    SupportableVolume ->
                        Input.radio
                            VH.exoColumnAttributes
                            { onChange =
                                \volumeUuid ->
                                    SetNonProjectView <| GetSupport (Just ( SupportableVolume, Just volumeUuid ))
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
        ]


supportableItemTypeStr : SupportableItemType -> String
supportableItemTypeStr supportableItemType =
    case supportableItemType of
        SupportableProject ->
            "project"

        SupportableImage ->
            "image"

        SupportableServer ->
            "server"

        SupportableVolume ->
            "volume"


viewStateToSupportableItem : ViewState -> Maybe ( SupportableItemType, Maybe HelperTypes.Uuid )
viewStateToSupportableItem viewState =
    let
        supportableProjectItem : ProjectIdentifier -> ProjectViewConstructor -> ( SupportableItemType, Maybe HelperTypes.Uuid )
        supportableProjectItem projectUuid projectViewConstructor =
            case projectViewConstructor of
                CreateServer createServerViewParams ->
                    ( SupportableImage, Just createServerViewParams.imageUuid )

                ServerDetail serverUuid _ ->
                    ( SupportableServer, Just serverUuid )

                CreateServerImage serverUuid _ ->
                    ( SupportableServer, Just serverUuid )

                VolumeDetail volumeUuid _ ->
                    ( SupportableVolume, Just volumeUuid )

                AttachVolumeModal _ maybeVolumeUuid ->
                    maybeVolumeUuid
                        |> Maybe.map (\uuid -> ( SupportableVolume, Just uuid ))
                        |> Maybe.withDefault ( SupportableProject, Just projectUuid )

                MountVolInstructions attachment ->
                    ( SupportableServer, Just attachment.serverUuid )

                _ ->
                    ( SupportableProject, Just projectUuid )
    in
    case viewState of
        NonProjectView _ ->
            Nothing

        ProjectView projectUuid _ projectViewConstructor ->
            Just <| supportableProjectItem projectUuid projectViewConstructor
