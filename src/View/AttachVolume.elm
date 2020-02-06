module View.AttachVolume exposing (attachVolume, mountVolInstructions)

import Color
import Element
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Color
import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import RemoteData
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
import View.Servers


attachVolume : Project -> Maybe OSTypes.ServerUuid -> Maybe OSTypes.VolumeUuid -> Element.Element Msg
attachVolume project maybeServerUuid maybeVolumeUuid =
    let
        serverChoices =
            RemoteData.withDefault [] project.servers
                |> List.map
                    (\s ->
                        Input.option s.osProps.uuid
                            (Element.text <| VH.possiblyUntitledResource s.osProps.name "server")
                    )

        volumeChoices =
            RemoteData.withDefault [] project.volumes
                |> List.filter (\v -> v.status == OSTypes.Available)
                |> List.map
                    (\v ->
                        Input.option
                            v.uuid
                            (Element.row VH.exoRowAttributes
                                [ Element.text <| VH.possiblyUntitledResource v.name "volume"
                                , Element.text " - "
                                , Element.text <| String.fromInt v.size ++ " GB"
                                ]
                            )
                    )
    in
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 <| Element.text "Attach a Volume"
        , Input.radio []
            { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Select a server")
            , onChange = \new -> ProjectMsg (Helpers.getProjectId project) (SetProjectView (AttachVolumeModal (Just new) maybeVolumeUuid))
            , options = serverChoices
            , selected = maybeServerUuid
            }
        , Input.radio []
            { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Select a volume")
            , onChange = \new -> ProjectMsg (Helpers.getProjectId project) (SetProjectView (AttachVolumeModal maybeServerUuid (Just new)))
            , options = volumeChoices
            , selected = maybeVolumeUuid
            }
        , let
            params =
                case ( maybeServerUuid, maybeVolumeUuid ) of
                    ( Just serverUuid, Just volumeUuid ) ->
                        let
                            volAttachedToServer =
                                Helpers.serverLookup project serverUuid
                                    |> Maybe.map (Helpers.volumeIsAttachedToServer volumeUuid)
                                    |> Maybe.withDefault False
                        in
                        if volAttachedToServer then
                            { attribs = [ Modifier.Disabled ]
                            , onPress = Nothing
                            , warnText = Just "This volume is already attached to this server."
                            }

                        else
                            { attribs = [ Modifier.Primary ]
                            , onPress = Just <| ProjectMsg (Helpers.getProjectId project) (RequestAttachVolume serverUuid volumeUuid)
                            , warnText = Nothing
                            }

                    _ ->
                        {- User hasn't selected a server and volume yet so we keep the button disabled but don't yell at him/her -}
                        { attribs = [ Modifier.Disabled ]
                        , onPress = Nothing
                        , warnText = Nothing
                        }

            button =
                Element.el [ Element.alignRight ] <|
                    Button.button
                        params.attribs
                        params.onPress
                        "Attach"
          in
          Element.row [ Element.width Element.fill ]
            [ case params.warnText of
                Just warnText ->
                    Element.el [ Font.color <| Color.toElementColor Framework.Color.red ] <| Element.text warnText

                Nothing ->
                    Element.none
            , button
            ]
        ]


mountVolInstructions : Project -> OSTypes.VolumeAttachment -> Element.Element Msg
mountVolInstructions project attachment =
    {- Future todo, talk to Cockpit and mount the volume for the user, formatting new volume if needed -}
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading2 <| Element.text "Volume Attached"
        , Element.text ("Device: " ++ attachment.device)
        , Element.paragraph []
            [ Element.text "You have attached a volume. Go to your server, format the volume if needed, and mount it!"
            ]
        , Button.button
            []
            (Just <|
                ProjectMsg
                    (Helpers.getProjectId project)
                <|
                    SetProjectView
                        (ServerDetail
                            attachment.serverUuid
                            { verboseStatus = False
                            , passwordVisibility = PasswordHidden
                            , ipInfoLevel = IPSummary
                            , serverActionStates =
                                Helpers.serverLookup project attachment.serverUuid
                                    |> Maybe.map View.Servers.availableActions
                                    |> Maybe.map (List.map View.Servers.toServerActionState)
                                    |> Maybe.withDefault []
                            }
                        )
            )
            "Go to my server"
        ]
