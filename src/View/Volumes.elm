module View.Volumes exposing (createVolume, volumeDetail, volumeDetailView, volumes)

import Color
import Element
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Color
import Framework.Modifier as Modifier
import Framework.Spinner as Spinner
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import OpenStack.Volumes
import RemoteData
import Style.Widgets.Card as ExoCard
import Types.Types exposing (..)
import View.Helpers as VH


volumes : Project -> Element.Element Msg
volumes project =
    Element.column
        VH.exoColumnAttributes
        [ Element.el VH.heading2 (Element.text "Volumes")
        , case project.volumes of
            RemoteData.NotAsked ->
                Element.text "Please wait"

            RemoteData.Loading ->
                Element.text "Loading volumes..."

            RemoteData.Failure _ ->
                Element.text "Error loading volumes :("

            RemoteData.Success vols ->
                Element.wrappedRow
                    (VH.exoRowAttributes ++ [ Element.spacing 15 ])
                    (List.map (renderVolumeCard project) vols)
        ]


renderVolumeCard : Project -> OSTypes.Volume -> Element.Element Msg
renderVolumeCard project volume =
    ExoCard.exoCard
        (VH.possiblyUntitledResource volume.name "volume")
        (String.fromInt volume.size ++ " GB")
    <|
        volumeDetail project volume.uuid


volumeActionButtons : Project -> OSTypes.Volume -> Element.Element Msg
volumeActionButtons project volume =
    let
        attachDetachButton =
            case volume.status of
                OSTypes.Available ->
                    Button.button
                        []
                        (Just
                            (ProjectMsg
                                (Helpers.getProjectId project)
                                (SetProjectView <| AttachVolumeModal Nothing (Just volume.uuid))
                            )
                        )
                        "Attach"

                OSTypes.InUse ->
                    Button.button
                        []
                        (Just
                            (ProjectMsg
                                (Helpers.getProjectId project)
                                (RequestDetachVolume volume.uuid)
                            )
                        )
                        "Detach"

                _ ->
                    Element.none

        deleteButton =
            case volume.status of
                OSTypes.Deleting ->
                    Spinner.spinner Spinner.ThreeCircles 20 Framework.Color.grey_darker

                _ ->
                    Button.button
                        [ Modifier.Danger ]
                        (Just <|
                            ProjectMsg
                                (Helpers.getProjectId project)
                                (RequestDeleteVolume volume.uuid)
                        )
                        "Delete"
    in
    Element.row [ Element.width Element.fill, Element.spacing 10 ]
        [ attachDetachButton
        , Element.el [ Element.alignRight ] deleteButton
        ]


volumeDetailView : Project -> OSTypes.VolumeUuid -> Element.Element Msg
volumeDetailView project volumeUuid =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading2 <| Element.text "Volume Detail"
        , volumeDetail project volumeUuid
        ]


volumeDetail : Project -> OSTypes.VolumeUuid -> Element.Element Msg
volumeDetail project volumeUuid =
    OpenStack.Volumes.volumeLookup project volumeUuid
        |> Maybe.withDefault (Element.text "No volume found")
        << Maybe.map
            (\volume ->
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.width Element.fill, Element.spacing 10 ])
                    [ VH.compactKVRow "Name:" <| Element.text <| VH.possiblyUntitledResource volume.name "volume"
                    , VH.compactKVRow "Status:" <| Element.text <| Debug.toString volume.status
                    , renderAttachments project volume
                    , VH.compactKVRow "Description:" <| Element.text <| Maybe.withDefault "" volume.description
                    , VH.compactKVRow "UUID:" <| Element.text <| volume.uuid
                    , volumeActionButtons project volume
                    ]
            )


renderAttachment : Project -> OSTypes.VolumeAttachment -> Element.Element Msg
renderAttachment project attachment =
    let
        serverName serverUuid =
            case Helpers.serverLookup project serverUuid of
                Just server ->
                    server.osProps.name

                Nothing ->
                    "(Could not resolve server name)"
    in
    Element.row
        (VH.exoColumnAttributes ++ [ Element.padding 0 ])
        [ Element.el [ Font.bold ] <| Element.text "Server:"
        , Element.text (serverName attachment.serverUuid)
        , Element.el [ Font.bold ] <| Element.text "Device:"
        , Element.text attachment.device
        ]


renderAttachments : Project -> OSTypes.Volume -> Element.Element Msg
renderAttachments project volume =
    case List.length volume.attachments of
        0 ->
            Element.none

        _ ->
            VH.compactKVRow "Attached to:" <|
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.padding 0 ])
                <|
                    List.map (renderAttachment project) volume.attachments


createVolume : Project -> OSTypes.VolumeName -> String -> Element.Element Msg
createVolume project volName volSizeStr =
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 20 ])
        [ Element.el VH.heading2 (Element.text "Create Volume")
        , Input.text
            [ Element.spacing 12 ]
            { text = volName
            , placeholder = Just (Input.placeholder [] (Element.text "My Important Data"))
            , onChange = \n -> ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| CreateVolume n volSizeStr
            , label = Input.labelAbove [] (Element.text "Name")
            }
        , Element.text "(Suggestion: choose a good name that describes what the volume will store.)"
        , Input.text
            []
            { text = volSizeStr
            , placeholder = Just (Input.placeholder [] (Element.text "10"))
            , onChange = \s -> ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| CreateVolume volName s
            , label = Input.labelAbove [] (Element.text "Size in GB")
            }
        , let
            params =
                case String.toInt volSizeStr of
                    Just volSizeInt ->
                        { attribs = [ Modifier.Primary ]
                        , onPress = Just (ProjectMsg (Helpers.getProjectId project) (RequestCreateVolume volName volSizeInt))
                        , warnText = Nothing
                        }

                    _ ->
                        { attribs = [ Modifier.Disabled ]
                        , onPress = Nothing
                        , warnText = Just "Volume size must be an integer"
                        }
          in
          Element.row (List.append VH.exoRowAttributes [ Element.width Element.fill ])
            [ case params.warnText of
                Just warnText ->
                    Element.el [ Font.color <| Color.toElementColor Framework.Color.red ] <| Element.text warnText

                Nothing ->
                    Element.none
            , Element.el [ Element.alignRight ] <|
                Button.button
                    params.attribs
                    params.onPress
                    "Create"
            ]
        ]
