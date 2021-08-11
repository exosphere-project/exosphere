module LegacyView.Volumes exposing (createVolume, volumeDetail, volumeDetailView, volumes)

import Element
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.String
import OpenStack.Quotas as OSQuotas
import OpenStack.Types as OSTypes
import OpenStack.Volumes
import Page.QuotaUsage
import RemoteData
import Style.Helpers as SH
import Style.Widgets.Card as ExoCard
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton
import Style.Widgets.NumericTextInput.NumericTextInput exposing (numericTextInput)
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.Defaults as Defaults
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Types.View
    exposing
        ( DeleteVolumeConfirmation
        , ProjectViewConstructor(..)
        , VolumeListViewParams
        )
import View.Helpers as VH
import View.Types
import Widget


volumes :
    View.Types.Context
    -> Bool
    -> Project
    -> VolumeListViewParams
    -> (VolumeListViewParams -> OuterMsg)
    -> Element.Element OuterMsg
volumes context showHeading project viewParams toMsg =
    let
        renderSuccessCase : List OSTypes.Volume -> Element.Element OuterMsg
        renderSuccessCase volumes_ =
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.paddingXY 10 0
                       , Element.spacing 15
                       , Element.width Element.fill
                       ]
                )
                (List.map
                    (renderVolumeCard context project viewParams toMsg)
                    volumes_
                )
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if showHeading then
            Element.row (VH.heading2 context.palette ++ [ Element.spacing 15 ])
                [ FeatherIcons.hardDrive |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                , Element.text
                    (context.localization.blockDevice
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )
                ]

          else
            Element.none
        , Element.column VH.contentContainer
            [ Page.QuotaUsage.view context (Page.QuotaUsage.Volume project.volumeQuota)
            , VH.renderWebData
                context
                project.volumes
                (Helpers.String.pluralize context.localization.blockDevice)
                renderSuccessCase
            ]
        ]


renderVolumeCard :
    View.Types.Context
    -> Project
    -> VolumeListViewParams
    -> (VolumeListViewParams -> OuterMsg)
    -> OSTypes.Volume
    -> Element.Element OuterMsg
renderVolumeCard context project viewParams toMsg volume =
    ExoCard.expandoCard
        context.palette
        (List.member volume.uuid viewParams.expandedVols)
        (\bool ->
            toMsg
                { viewParams
                    | expandedVols =
                        if bool then
                            volume.uuid :: viewParams.expandedVols

                        else
                            List.filter (\v -> v /= volume.uuid) viewParams.expandedVols
                }
        )
        (VH.possiblyUntitledResource volume.name context.localization.blockDevice
            |> Element.text
        )
        (Element.text <| String.fromInt volume.size ++ " GB")
    <|
        volumeDetail
            context
            project
            (\deleteConfirmations -> toMsg { viewParams | deleteConfirmations = deleteConfirmations })
            viewParams.deleteConfirmations
            volume.uuid


volumeActionButtons :
    View.Types.Context
    -> Project
    -> (List DeleteVolumeConfirmation -> OuterMsg)
    -> List DeleteVolumeConfirmation
    -> OSTypes.Volume
    -> Element.Element OuterMsg
volumeActionButtons context project toMsg deleteConfirmations volume =
    let
        volDetachDeleteWarning =
            if Helpers.isBootVol Nothing volume then
                Element.text <|
                    String.concat
                        [ "This "
                        , context.localization.blockDevice
                        , " backs a "
                        , context.localization.virtualComputer
                        , "; it cannot be detached or deleted until the "
                        , context.localization.virtualComputer
                        , " is deleted."
                        ]

            else if volume.status == OSTypes.InUse then
                Element.text <|
                    String.join " "
                        [ "This"
                        , context.localization.blockDevice
                        , "must be detached before it can be deleted."
                        ]

            else
                Element.none

        attachDetachButton =
            case volume.status of
                OSTypes.Available ->
                    Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Attach"
                        , onPress =
                            Just
                                (SetProjectView project.auth.project.uuid <|
                                    AttachVolumeModal Nothing (Just volume.uuid)
                                )
                        }

                OSTypes.InUse ->
                    if Helpers.isBootVol Nothing volume then
                        Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Detach"
                            , onPress = Nothing
                            }

                    else
                        Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Detach"
                            , onPress =
                                Just
                                    (SharedMsg <|
                                        ProjectMsg
                                            project.auth.project.uuid
                                            (RequestDetachVolume volume.uuid)
                                    )
                            }

                _ ->
                    Element.none

        confirmationNeeded =
            List.member volume.uuid deleteConfirmations

        deleteButton =
            case ( volume.status, confirmationNeeded ) of
                ( OSTypes.Deleting, _ ) ->
                    Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing

                ( _, True ) ->
                    Element.row [ Element.spacing 10 ]
                        [ Element.text "Confirm delete?"
                        , Widget.iconButton
                            (SH.materialStyle context.palette).dangerButton
                            { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                            , text = "Delete"
                            , onPress =
                                Just <|
                                    SharedMsg <|
                                        ProjectMsg
                                            project.auth.project.uuid
                                            (RequestDeleteVolume volume.uuid)
                            }
                        , Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Cancel"
                            , onPress =
                                Just <|
                                    toMsg
                                        (deleteConfirmations |> List.filter ((/=) volume.uuid))
                            }
                        ]

                ( _, False ) ->
                    if volume.status == OSTypes.InUse then
                        Widget.iconButton
                            (SH.materialStyle context.palette).button
                            { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                            , text = "Delete"
                            , onPress = Nothing
                            }

                    else
                        Widget.iconButton
                            (SH.materialStyle context.palette).dangerButton
                            { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                            , text = "Delete"
                            , onPress =
                                Just <|
                                    toMsg
                                        [ volume.uuid ]
                            }
    in
    Element.column (Element.width Element.fill :: VH.exoColumnAttributes)
        [ volDetachDeleteWarning
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ attachDetachButton
            , Element.el [ Element.alignRight ] deleteButton
            ]
        ]


volumeDetailView :
    View.Types.Context
    -> Project
    -> List DeleteVolumeConfirmation
    -> (List DeleteVolumeConfirmation -> OuterMsg)
    -> OSTypes.VolumeUuid
    -> Element.Element OuterMsg
volumeDetailView context project deleteVolumeConfirmations toMsg volumeUuid =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette) <|
            Element.text <|
                String.join " "
                    [ context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    , "Detail"
                    ]
        , volumeDetail context project toMsg deleteVolumeConfirmations volumeUuid
        ]


volumeDetail :
    View.Types.Context
    -> Project
    -> (List DeleteVolumeConfirmation -> OuterMsg)
    -> List DeleteVolumeConfirmation
    -> OSTypes.VolumeUuid
    -> Element.Element OuterMsg
volumeDetail context project toMsg deleteVolumeConfirmations volumeUuid =
    OpenStack.Volumes.volumeLookup project volumeUuid
        |> Maybe.withDefault
            (Element.text <|
                String.join " "
                    [ "No"
                    , context.localization.blockDevice
                    , "found"
                    ]
            )
        << Maybe.map
            (\volume ->
                Element.column
                    VH.contentContainer
                    [ VH.compactKVRow "Name:" <| Element.text <| VH.possiblyUntitledResource volume.name context.localization.blockDevice
                    , VH.compactKVRow "Status:" <| Element.text <| OSTypes.volumeStatusToString volume.status
                    , renderAttachments context project volume
                    , VH.compactKVRow "Description:" <|
                        Element.paragraph [ Element.width Element.fill ] <|
                            [ Element.text <| Maybe.withDefault "" volume.description ]
                    , VH.compactKVRow "UUID:" <| copyableText context.palette [] volume.uuid
                    , case volume.imageMetadata of
                        Nothing ->
                            Element.none

                        Just metadata ->
                            VH.compactKVRow
                                (String.concat
                                    [ "Created from "
                                    , context.localization.staticRepresentationOfBlockDeviceContents
                                    , ":"
                                    ]
                                )
                                (Element.text metadata.name)
                    , volumeActionButtons context project toMsg deleteVolumeConfirmations volume
                    ]
            )


renderAttachment : View.Types.Context -> Project -> OSTypes.VolumeAttachment -> Element.Element OuterMsg
renderAttachment context project attachment =
    let
        serverName serverUuid =
            case GetterSetters.serverLookup project serverUuid of
                Just server ->
                    server.osProps.name

                Nothing ->
                    String.join " "
                        [ "(Could not resolve"
                        , context.localization.virtualComputer
                        , "name)"
                        ]
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.padding 0 ])
        [ Element.el [ Font.bold ] <| Element.text "Server:"
        , Element.row [ Element.spacing 5 ]
            [ Element.text (serverName attachment.serverUuid)
            , Style.Widgets.IconButton.goToButton context.palette
                (Just <|
                    SetProjectView project.auth.project.uuid <|
                        ServerDetail attachment.serverUuid <|
                            Defaults.serverDetailViewParams
                )
            ]
        , Element.el [ Font.bold ] <| Element.text "Device:"
        , Element.text attachment.device
        , Element.el [ Font.bold ] <| Element.text "Mount point*:"
        , Helpers.volDeviceToMountpoint attachment.device |> Maybe.withDefault "" |> Element.text
        , Element.el [ Font.size 11 ] <|
            Element.text <|
                String.join " "
                    [ "*"
                    , context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    , "will only be automatically formatted/mounted on operating"
                    ]
        , Element.el [ Font.size 11 ] <| Element.text "systems which use systemd 236 or newer (e.g. Ubuntu 18.04 and CentOS 8)"
        ]


renderAttachments : View.Types.Context -> Project -> OSTypes.Volume -> Element.Element OuterMsg
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


createVolume : View.Types.Context -> Project -> OSTypes.VolumeName -> NumericTextInput -> Element.Element OuterMsg
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
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 20, Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette)
            (Element.text <|
                String.join " "
                    [ "Create"
                    , context.localization.blockDevice |> Helpers.String.toTitleCase
                    ]
            )
        , Element.column VH.formContainer
            [ Input.text
                (VH.inputItemAttributes context.palette.background)
                { text = volName
                , placeholder = Just (Input.placeholder [] (Element.text "My Important Data"))
                , onChange = \n -> SetProjectView project.auth.project.uuid <| CreateVolume n volSizeInput
                , label = Input.labelAbove [] (Element.text "Name")
                }
            , Element.text <|
                String.join " "
                    [ "(Suggestion: choose a good name that describes what the"
                    , context.localization.blockDevice
                    , "will store.)"
                    ]
            , numericTextInput
                context.palette
                (VH.inputItemAttributes context.palette.background)
                volSizeInput
                { labelText = "Size in GB"
                , minVal = Just 1
                , maxVal = volGbAvail
                , defaultVal = Just 2
                }
                (\newInput -> SetProjectView project.auth.project.uuid <| CreateVolume volName newInput)
            , let
                ( onPress, quotaWarnText ) =
                    if canAttemptCreateVol then
                        case volSizeInput of
                            ValidNumericTextInput volSizeGb ->
                                ( Just <|
                                    SharedMsg
                                        (ProjectMsg project.auth.project.uuid (RequestCreateVolume volName volSizeGb))
                                , Nothing
                                )

                            InvalidNumericTextInput _ ->
                                ( Nothing, Nothing )

                    else
                        ( Nothing
                        , Just <|
                            String.concat
                                [ "Your "
                                , context.localization.maxResourcesPerProject
                                , " does not allow for creation of another "
                                , context.localization.blockDevice
                                , "."
                                ]
                        )
              in
              Element.row (List.append VH.exoRowAttributes [ Element.width Element.fill ])
                [ case quotaWarnText of
                    Just text ->
                        Element.el [ Font.color <| SH.toElementColor context.palette.error ] <| Element.text text

                    Nothing ->
                        Element.none
                , Element.el [ Element.alignRight ] <|
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Create"
                        , onPress = onPress
                        }
                ]
            ]
        ]
