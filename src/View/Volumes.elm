module View.Volumes exposing (createVolume, volumeDetail, volumeDetailView, volumes)

import Element
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import OpenStack.Quotas as OSQuotas
import OpenStack.Types as OSTypes
import OpenStack.Volumes
import RemoteData
import Style.Helpers as SH
import Style.Widgets.Button
import Style.Widgets.Card as ExoCard
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( DeleteVolumeConfirmation
        , IPInfoLevel(..)
        , Msg(..)
        , PasswordVisibility(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH
import View.Types
import Widget
import Widget.Style.Material


volumes : View.Types.Context -> Project -> List DeleteVolumeConfirmation -> Element.Element Msg
volumes context project deleteVolumeConfirmations =
    Element.column
        VH.exoColumnAttributes
        [ Element.el VH.heading2 (Element.text "Volumes")
        , case project.volumes of
            RemoteData.NotAsked ->
                Element.row [ Element.spacing 15 ]
                    [ Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing
                    , Element.text "Please wait..."
                    ]

            RemoteData.Loading ->
                Element.row [ Element.spacing 15 ]
                    [ Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing
                    , Element.text "Loading volumes..."
                    ]

            RemoteData.Failure _ ->
                Element.text "Error loading volumes :("

            RemoteData.Success vols ->
                Element.column
                    (VH.exoColumnAttributes
                        ++ [ Element.spacing 15
                           , Element.width (Element.fill |> Element.minimum 960)
                           ]
                    )
                    (List.map (renderVolumeCard context project deleteVolumeConfirmations) vols)
        ]


renderVolumeCard : View.Types.Context -> Project -> List DeleteVolumeConfirmation -> OSTypes.Volume -> Element.Element Msg
renderVolumeCard context project deleteVolumeConfirmations volume =
    ExoCard.exoCard
        context.palette
        (VH.possiblyUntitledResource volume.name "volume")
        (String.fromInt volume.size ++ " GB")
    <|
        volumeDetail context project ListProjectVolumes deleteVolumeConfirmations volume.uuid


volumeActionButtons :
    View.Types.Context
    -> Project
    -> (List DeleteVolumeConfirmation -> ProjectViewConstructor)
    -> List DeleteVolumeConfirmation
    -> OSTypes.Volume
    -> Element.Element Msg
volumeActionButtons context project toProjectViewConstructor deleteVolumeConfirmations volume =
    let
        volDetachDeleteWarning =
            if Helpers.isBootVol Nothing volume then
                Element.text "This volume backs a server; it cannot be detached or deleted until the server is deleted."

            else if volume.status == OSTypes.InUse then
                Element.text "This volume must be detached before it can be deleted."

            else
                Element.none

        attachDetachButton =
            case volume.status of
                OSTypes.Available ->
                    Widget.textButton
                        (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                        { text = "Attach"
                        , onPress =
                            Just
                                (ProjectMsg
                                    project.auth.project.uuid
                                    (SetProjectView <|
                                        AttachVolumeModal Nothing (Just volume.uuid)
                                    )
                                )
                        }

                OSTypes.InUse ->
                    if Helpers.isBootVol Nothing volume then
                        Widget.textButton
                            (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                            { text = "Detach"
                            , onPress = Nothing
                            }

                    else
                        Widget.textButton
                            (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                            { text = "Detach"
                            , onPress =
                                Just
                                    (ProjectMsg
                                        project.auth.project.uuid
                                        (RequestDetachVolume volume.uuid)
                                    )
                            }

                _ ->
                    Element.none

        confirmationNeeded =
            List.member volume.uuid deleteVolumeConfirmations

        deleteButton =
            case ( volume.status, confirmationNeeded ) of
                ( OSTypes.Deleting, _ ) ->
                    Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing

                ( _, True ) ->
                    Element.row [ Element.spacing 10 ]
                        [ Element.text "Confirm delete?"
                        , Widget.textButton
                            (Style.Widgets.Button.dangerButton context.palette)
                            { text = "Delete"
                            , onPress =
                                Just <|
                                    ProjectMsg
                                        project.auth.project.uuid
                                        (RequestDeleteVolume volume.uuid)
                            }
                        , Widget.textButton
                            (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                            { text = "Cancel"
                            , onPress =
                                Just <|
                                    ProjectMsg
                                        project.auth.project.uuid
                                        (SetProjectView <|
                                            toProjectViewConstructor (deleteVolumeConfirmations |> List.filter ((/=) volume.uuid))
                                        )
                            }
                        ]

                ( _, False ) ->
                    if volume.status == OSTypes.InUse then
                        Widget.textButton
                            (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                            { text = "Delete"
                            , onPress = Nothing
                            }

                    else
                        Widget.textButton
                            (Style.Widgets.Button.dangerButton context.palette)
                            { text = "Delete"
                            , onPress =
                                Just <|
                                    ProjectMsg
                                        project.auth.project.uuid
                                        (SetProjectView <| toProjectViewConstructor [ volume.uuid ])
                            }
    in
    Element.column (Element.width Element.fill :: VH.exoColumnAttributes)
        [ volDetachDeleteWarning
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ attachDetachButton
            , Element.el [ Element.alignRight ] deleteButton
            ]
        ]


volumeDetailView : View.Types.Context -> Project -> List DeleteVolumeConfirmation -> OSTypes.VolumeUuid -> Element.Element Msg
volumeDetailView context project deleteVolumeConfirmations volumeUuid =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading2 <| Element.text "Volume Detail"
        , volumeDetail context project (VolumeDetail volumeUuid) deleteVolumeConfirmations volumeUuid
        ]


volumeDetail :
    View.Types.Context
    -> Project
    -> (List DeleteVolumeConfirmation -> ProjectViewConstructor)
    -> List DeleteVolumeConfirmation
    -> OSTypes.VolumeUuid
    -> Element.Element Msg
volumeDetail context project toProjectViewConstructor deleteVolumeConfirmations volumeUuid =
    OpenStack.Volumes.volumeLookup project volumeUuid
        |> Maybe.withDefault (Element.text "No volume found")
        << Maybe.map
            (\volume ->
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.width Element.fill, Element.spacing 10 ])
                    [ VH.compactKVRow "Name:" <| Element.text <| VH.possiblyUntitledResource volume.name "volume"
                    , VH.compactKVRow "Status:" <| Element.text <| Debug.toString volume.status
                    , renderAttachments context project volume
                    , VH.compactKVRow "Description:" <|
                        Element.paragraph [ Element.width (Element.fill |> Element.maximum 706) ] <|
                            [ Element.text <| Maybe.withDefault "" volume.description ]
                    , VH.compactKVRow "UUID:" <| copyableText context.palette volume.uuid
                    , case volume.imageMetadata of
                        Nothing ->
                            Element.none

                        Just metadata ->
                            VH.compactKVRow "Created from image:" <| Element.text metadata.name
                    , volumeActionButtons context project toProjectViewConstructor deleteVolumeConfirmations volume
                    ]
            )


renderAttachment : View.Types.Context -> Project -> OSTypes.VolumeAttachment -> Element.Element Msg
renderAttachment context project attachment =
    let
        serverName serverUuid =
            case GetterSetters.serverLookup project serverUuid of
                Just server ->
                    server.osProps.name

                Nothing ->
                    "(Could not resolve server name)"
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.padding 0 ])
        [ Element.el [ Font.bold ] <| Element.text "Server:"
        , Element.row [ Element.spacing 5 ]
            [ Element.text (serverName attachment.serverUuid)
            , Input.button
                [ Border.width 1
                , Border.rounded 6
                , Border.color (SH.toElementColor context.palette.muted)
                , Element.padding 3
                ]
                { onPress =
                    Just
                        (ProjectMsg
                            project.auth.project.uuid
                            (SetProjectView <| ServerDetail attachment.serverUuid <| Defaults.serverDetailViewParams)
                        )
                , label =
                    FeatherIcons.chevronRight
                        |> FeatherIcons.withSize 14
                        |> FeatherIcons.toHtml []
                        |> Element.html
                        |> Element.el []
                }
            ]
        , Element.el [ Font.bold ] <| Element.text "Device:"
        , Element.text attachment.device
        , Element.el [ Font.bold ] <| Element.text "Mount point*:"
        , Helpers.volDeviceToMountpoint attachment.device |> Maybe.withDefault "" |> Element.text
        , Element.el [ Font.size 11 ] <| Element.text "* Volume will only be automatically formatted/mounted on operating"
        , Element.el [ Font.size 11 ] <| Element.text "systems which use systemd 236 or newer (e.g. Ubuntu 18.04 and CentOS 8)"
        ]


renderAttachments : View.Types.Context -> Project -> OSTypes.Volume -> Element.Element Msg
renderAttachments context project volume =
    case List.length volume.attachments of
        0 ->
            Element.none

        _ ->
            VH.compactKVRow "Attached to:" <|
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.padding 0 ])
                <|
                    List.map (renderAttachment context project) volume.attachments


createVolume : View.Types.Context -> Project -> OSTypes.VolumeName -> NumericTextInput -> Element.Element Msg
createVolume context project volName volSizeInput =
    let
        maybeVolumeQuotaAvail =
            project.volumeQuota
                |> RemoteData.toMaybe
                |> Maybe.map OSQuotas.volumeQuotaAvail

        ( canAttemptCreateVol, volGbAvail ) =
            case maybeVolumeQuotaAvail of
                Just ( numVolsAvail, volGbAvail_ ) ->
                    ( numVolsAvail |> Maybe.map (\v -> v >= 1) |> Maybe.withDefault True
                    , volGbAvail_
                    )

                Nothing ->
                    ( True, Nothing )
    in
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 20 ])
        [ Element.el VH.heading2 (Element.text "Create Volume")
        , Input.text
            (VH.inputItemAttributes context.palette.background)
            { text = volName
            , placeholder = Just (Input.placeholder [] (Element.text "My Important Data"))
            , onChange = \n -> ProjectMsg project.auth.project.uuid <| SetProjectView <| CreateVolume n volSizeInput
            , label = Input.labelAbove [] (Element.text "Name")
            }
        , Element.text "(Suggestion: choose a good name that describes what the volume will store.)"
        , numericTextInput
            context.palette
            (VH.inputItemAttributes context.palette.background)
            volSizeInput
            { labelText = "Size in GB"
            , minVal = Just 1
            , maxVal = volGbAvail
            , defaultVal = Just 2
            }
            (\newInput -> ProjectMsg project.auth.project.uuid <| SetProjectView <| CreateVolume volName newInput)
        , let
            ( onPress, quotaWarnText ) =
                if canAttemptCreateVol then
                    case volSizeInput of
                        ValidNumericTextInput volSizeGb ->
                            ( Just (ProjectMsg project.auth.project.uuid (RequestCreateVolume volName volSizeGb))
                            , Nothing
                            )

                        InvalidNumericTextInput _ ->
                            ( Nothing, Nothing )

                else
                    ( Nothing, Just "Your quota does not allow for creation of another volume." )
          in
          Element.row (List.append VH.exoRowAttributes [ Element.width Element.fill ])
            [ case quotaWarnText of
                Just text ->
                    Element.el [ Font.color <| SH.toElementColor context.palette.error ] <| Element.text text

                Nothing ->
                    Element.none
            , Element.el [ Element.alignRight ] <|
                Widget.textButton
                    (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                    { text = "Create"
                    , onPress = onPress
                    }
            ]
        ]
