module View.AttachVolume exposing (attachVolume, mountVolInstructions)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( IPInfoLevel(..)
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


attachVolume : View.Types.Context -> Project -> Maybe OSTypes.ServerUuid -> Maybe OSTypes.VolumeUuid -> Element.Element Msg
attachVolume context project maybeServerUuid maybeVolumeUuid =
    let
        serverChoices =
            -- Future TODO instead of hiding servers that are ineligible to have a newly attached volume, show them grayed out with mouseover text like "volume cannot be attached to this server because X"
            RDPP.withDefault [] project.servers
                |> List.filter
                    (\s ->
                        not <|
                            List.member
                                s.osProps.details.openstackStatus
                                [ OSTypes.ServerShelved
                                , OSTypes.ServerShelvedOffloaded
                                , OSTypes.ServerError
                                , OSTypes.ServerSoftDeleted
                                , OSTypes.ServerBuilding
                                ]
                    )
                |> List.map
                    (\s ->
                        Input.option s.osProps.uuid
                            (Element.text <| VH.possiblyUntitledResource s.osProps.name context.localization.virtualComputer)
                    )

        volumeChoices =
            RemoteData.withDefault [] project.volumes
                |> List.filter (\v -> v.status == OSTypes.Available)
                |> List.map
                    (\v ->
                        Input.option
                            v.uuid
                            (Element.row VH.exoRowAttributes
                                [ Element.text <| VH.possiblyUntitledResource v.name context.localization.blockDevice
                                , Element.text " - "
                                , Element.text <| String.fromInt v.size ++ " GB"
                                ]
                            )
                    )
    in
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 <| Element.text "Attach a Volume"
        , Input.radio []
            { label =
                Input.labelAbove
                    [ Element.paddingXY 0 12 ]
                    (Element.text <|
                        String.join " "
                            [ "Select"
                            , Helpers.String.indefiniteArticle context.localization.virtualComputer
                            , context.localization.virtualComputer
                            ]
                    )
            , onChange =
                \new ->
                    ProjectMsg project.auth.project.uuid (SetProjectView (AttachVolumeModal (Just new) maybeVolumeUuid))
            , options = serverChoices
            , selected = maybeServerUuid
            }
        , Input.radio []
            -- TODO if no volumes in list, suggest user create a volume and provide link to that view
            { label =
                Input.labelAbove [ Element.paddingXY 0 12 ]
                    (Element.text ("Select a " ++ context.localization.blockDevice))
            , onChange =
                \new ->
                    ProjectMsg project.auth.project.uuid (SetProjectView (AttachVolumeModal maybeServerUuid (Just new)))
            , options = volumeChoices
            , selected = maybeVolumeUuid
            }
        , let
            params =
                case ( maybeServerUuid, maybeVolumeUuid ) of
                    ( Just serverUuid, Just volumeUuid ) ->
                        let
                            volAttachedToServer =
                                GetterSetters.serverLookup project serverUuid
                                    |> Maybe.map (GetterSetters.volumeIsAttachedToServer volumeUuid)
                                    |> Maybe.withDefault False
                        in
                        if volAttachedToServer then
                            { onPress = Nothing
                            , warnText =
                                Just <|
                                    String.join " "
                                        [ "This"
                                        , context.localization.blockDevice
                                        , "is already attached to this"
                                        , context.localization.virtualComputer
                                        ]
                            }

                        else
                            { onPress =
                                Just <|
                                    ProjectMsg project.auth.project.uuid (RequestAttachVolume serverUuid volumeUuid)
                            , warnText = Nothing
                            }

                    _ ->
                        {- User hasn't selected a server and volume yet so we keep the button disabled but don't yell at him/her -}
                        { onPress = Nothing
                        , warnText = Nothing
                        }

            button =
                Element.el [ Element.alignRight ] <|
                    Widget.textButton
                        (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                        { text = "Attach"
                        , onPress = params.onPress
                        }
          in
          Element.row [ Element.width Element.fill ]
            [ case params.warnText of
                Just warnText ->
                    Element.el [ Font.color <| SH.toElementColor context.palette.error ] <| Element.text warnText

                Nothing ->
                    Element.none
            , button
            ]
        ]


mountVolInstructions : View.Types.Context -> Project -> OSTypes.VolumeAttachment -> Element.Element Msg
mountVolInstructions context project attachment =
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 <|
            Element.text <|
                String.join " "
                    [ context.localization.blockDevice
                        |> Helpers.String.stringToTitleCase
                    , "Attached"
                    ]
        , Element.text ("Device: " ++ attachment.device)
        , case Helpers.volDeviceToMountpoint attachment.device of
            Just mountpoint ->
                Element.text ("Mount point: " ++ mountpoint)

            Nothing ->
                Element.none
        , Element.paragraph []
            [ case Helpers.volDeviceToMountpoint attachment.device of
                Just mountpoint ->
                    Element.text <|
                        String.join " "
                            [ "We'll try to mount this"
                            , context.localization.blockDevice
                            , "at"
                            , mountpoint
                            , "on your instance's filesystem. You should be able to access the"
                            , context.localization.blockDevice
                            , "there."
                            , "If it's a completely empty"
                            , context.localization.blockDevice
                            , "we'll also try to format it first."
                            , "This may not work on older operating systems (like CentOS 7 or Ubuntu 16.04)."
                            , "In that case, you may need to format and/or mount the"
                            , context.localization.blockDevice
                            , "manually."
                            ]

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "We attached the"
                            , context.localization.blockDevice
                            , "but couldn't determine a mountpoint from the device path. You may need to format and/or mount the"
                            , context.localization.blockDevice
                            , "manually."
                            ]
            ]
        , Widget.textButton
            (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
            { text = "Go to my " ++ context.localization.virtualComputer
            , onPress =
                Just <|
                    ProjectMsg
                        project.auth.project.uuid
                    <|
                        SetProjectView
                            (ServerDetail
                                attachment.serverUuid
                                Defaults.serverDetailViewParams
                            )
            }
        ]
