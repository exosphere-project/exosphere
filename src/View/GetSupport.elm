module View.GetSupport exposing (getSupport, viewStateToSupportableItem)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode
import OpenStack.Types as OSTypes
import RemoteData
import Set
import Style.Types
import Style.Widgets.Select
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectIdentifier
        , ProjectViewConstructor(..)
        , SupportableItemType(..)
        , ViewState(..)
        )
import View.Helpers as VH


getSupport :
    Model
    -> Style.Types.ExoPalette
    -> Maybe ( SupportableItemType, Maybe HelperTypes.Uuid )
    -> String
    -> Element.Element Msg
getSupport model palette maybeSupportableResource requestDescription =
    Element.column VH.exoColumnAttributes
        [ Input.radio
            VH.exoColumnAttributes
            { onChange =
                \option ->
                    SetNonProjectView <| GetSupport (Maybe.map (\option_ -> ( option_, Nothing )) option) requestDescription
            , selected =
                maybeSupportableResource
                    |> Maybe.map Tuple.first
                    |> Just
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

            Just ( supportableItemType, _ ) ->
                Element.text ("Which " ++ supportableItemTypeStr supportableItemType ++ " do you need help with?")
        , case maybeSupportableResource of
            Nothing ->
                Element.none

            Just ( supportableItemType, maybeSupportableItemUuid ) ->
                let
                    onChange value =
                        let
                            newMaybeSupportableItemUuid =
                                if value == "" then
                                    Nothing

                                else
                                    Just value
                        in
                        SetNonProjectView <|
                            GetSupport
                                (Just ( supportableItemType, newMaybeSupportableItemUuid ))
                                requestDescription

                    options =
                        case supportableItemType of
                            SupportableProject ->
                                model.projects
                                    |> List.map
                                        (\proj ->
                                            ( proj.auth.project.uuid
                                            , VH.friendlyProjectTitle model proj
                                            )
                                        )

                            SupportableImage ->
                                model.projects
                                    |> List.map .images
                                    |> List.concat
                                    |> List.map
                                        (\image ->
                                            ( image.uuid
                                            , image.name
                                            )
                                        )
                                    -- This removes duplicate values, heh
                                    |> Set.fromList
                                    |> Set.toList
                                    |> List.sortBy Tuple.second

                            SupportableServer ->
                                model.projects
                                    |> List.map .servers
                                    |> List.map (RDPP.withDefault [])
                                    |> List.concat
                                    |> List.map
                                        (\server ->
                                            ( server.osProps.uuid
                                            , server.osProps.name
                                            )
                                        )
                                    |> List.sortBy Tuple.second

                            SupportableVolume ->
                                model.projects
                                    |> List.map .volumes
                                    |> List.map (RemoteData.withDefault [])
                                    |> List.concat
                                    |> List.map
                                        (\volume ->
                                            ( volume.uuid
                                            , volume.name
                                            )
                                        )
                                    |> List.sortBy Tuple.second

                    label =
                        "Select a " ++ supportableItemTypeStr supportableItemType
                in
                Style.Widgets.Select.select
                    []
                    { onChange =
                        onChange
                    , options = options
                    , selected = maybeSupportableItemUuid
                    , label = label
                    }
        , Input.multiline
            []
            { onChange =
                \newVal -> SetNonProjectView <| GetSupport maybeSupportableResource newVal
            , text = requestDescription
            , placeholder = Nothing
            , label = Input.labelAbove [] (Element.text "Please describe exactly what you need help with.")
            , spellcheck = True
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
        supportableProjectItem :
            ProjectIdentifier
            -> ProjectViewConstructor
            -> ( SupportableItemType, Maybe HelperTypes.Uuid )
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
