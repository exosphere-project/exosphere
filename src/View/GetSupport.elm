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
import Style.Types
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
    -> Element.Element Msg
getSupport model palette maybeSupportableResource =
    Element.column VH.exoColumnAttributes
        [ Input.radio
            VH.exoColumnAttributes
            { onChange =
                \option ->
                    option
                        |> Maybe.map (\option_ -> ( option_, Nothing ))
                        |> GetSupport
                        |> SetNonProjectView
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

            Just ( supportableItemType, maybeSupportableItemUuid ) ->
                case supportableItemType of
                    SupportableProject ->
                        let
                            selectProj : Maybe HelperTypes.Uuid -> List Project -> Html Msg
                            selectProj maybeProj projs =
                                Html.select
                                    [ HtmlE.on
                                        "change"
                                        (Json.Decode.map
                                            (\uuid ->
                                                SetNonProjectView <|
                                                    GetSupport <|
                                                        Just ( supportableItemType, Just uuid )
                                            )
                                            Json.Decode.string
                                        )
                                    , HtmlA.style "appearance" "none"
                                    , HtmlA.style "-webkit-appearance" "none"
                                    , HtmlA.style "-moz-appearance" "none"
                                    , HtmlA.style "padding" "5px"
                                    , HtmlA.style "border-width" "0"
                                    , HtmlA.style "height" "48px"
                                    , HtmlA.style "font-size" "18px"
                                    , HtmlA.style "background-color" "transparent"
                                    ]
                                    (Html.option [ HtmlA.value "" ] [ Html.text "Select project" ]
                                        :: List.map (optionProj maybeProj) projs
                                    )

                            optionProj : Maybe HelperTypes.Uuid -> Project -> Html Msg
                            optionProj maybeSelectedProjUuid proj =
                                Html.option
                                    ([ HtmlA.value proj.auth.project.uuid ]
                                        ++ (case maybeSelectedProjUuid of
                                                Nothing ->
                                                    []

                                                Just selectedProjUuid ->
                                                    [ HtmlA.selected (selectedProjUuid == proj.auth.project.uuid) ]
                                           )
                                    )
                                    [ Html.text proj.auth.project.name ]
                        in
                        Element.row
                            [ Border.width 1
                            , Border.rounded 3
                            ]
                            [ Element.el [] <| Element.html (selectProj maybeSupportableItemUuid model.projects)
                            , FeatherIcons.chevronDown |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                            ]

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
                    SupportableImage ->
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
                        Element.none

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
                    SupportableServer ->
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
                    SupportableVolume ->
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
