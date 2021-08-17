module LegacyView.AttachVolume exposing (mountVolInstructions)

import Element
import Helpers.Helpers as Helpers
import Helpers.String
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Types.Defaults as Defaults
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.View
    exposing
        ( IPInfoLevel(..)
        , PasswordVisibility(..)
        , ProjectViewConstructor(..)
        )
import View.Helpers as VH
import View.Types
import Widget


mountVolInstructions : View.Types.Context -> Project -> OSTypes.VolumeAttachment -> Element.Element OuterMsg
mountVolInstructions context project attachment =
    Element.column VH.exoColumnAttributes
        [ Element.el (VH.heading2 context.palette) <|
            Element.text <|
                String.join " "
                    [ context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    , "Attached"
                    ]
        , Element.column VH.contentContainer
            [ Element.text ("Device: " ++ attachment.device)
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
                (SH.materialStyle context.palette).primaryButton
                { text = "Go to my " ++ context.localization.virtualComputer
                , onPress =
                    Just <|
                        SetProjectView
                            project.auth.project.uuid
                        <|
                            ServerDetail
                                attachment.serverUuid
                                Defaults.serverDetailViewParams
                }
            ]
        ]
