module Page.VolumeDetail exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import OpenStack.Volumes
import Route
import Set
import Style.Helpers as SH exposing (spacer)
import Style.Widgets.Button as Button
import Style.Widgets.Card
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.DeleteButton exposing (deleteIconButton)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , volumeUuid : OSTypes.VolumeUuid
    , deleteConfirmations : Set.Set OSTypes.VolumeUuid
    }


type Msg
    = GotDeleteNeedsConfirm
    | GotDeleteConfirm
    | GotDeleteCancel
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> OSTypes.VolumeUuid -> Model
init showHeading volumeId =
    Model showHeading volumeId Set.empty


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotDeleteNeedsConfirm ->
            ( { model
                | deleteConfirmations =
                    Set.insert
                        model.volumeUuid
                        model.deleteConfirmations
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotDeleteConfirm ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <| SharedMsg.RequestDeleteVolume model.volumeUuid
            )

        GotDeleteCancel ->
            ( { model
                | deleteConfirmations =
                    Set.remove
                        model.volumeUuid
                        model.deleteConfirmations
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        volumeName =
            GetterSetters.volumeLookup project model.volumeUuid
                |> Maybe.andThen (\vol -> vol.name)
                |> Maybe.withDefault model.volumeUuid
    in
    if model.showHeading then
        Element.column
            (VH.contentContainer ++ [ Element.spacing spacer.px16 ])
            [ Text.heading context.palette
                []
                Element.none
                (String.join
                    " "
                    [ context.localization.blockDevice |> Helpers.String.toTitleCase, volumeName ]
                )
            , volumeDetail context project model
            ]

    else
        volumeDetail context project model


volumeDetail :
    View.Types.Context
    -> Project
    -> Model
    -> Element.Element Msg
volumeDetail context project model =
    OpenStack.Volumes.volumeLookup project model.volumeUuid
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
                Element.column []
                    [ Style.Widgets.Card.exoCard context.palette
                        (Element.column
                            [ Element.padding spacer.px8, Element.spacing spacer.px16 ]
                            [ Text.subheading context.palette
                                []
                                Element.none
                                "Status"
                            , Element.row []
                                [ Element.el [] (Element.text <| OSTypes.volumeStatusToString volume.status) ]
                            , case volume.description of
                                Just "" ->
                                    Element.none

                                Just description ->
                                    VH.compactKVRow "Description:" <|
                                        Element.paragraph [ Element.width Element.fill ] <|
                                            [ Element.text <| description ]

                                Nothing ->
                                    Element.none
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
                            ]
                        )
                    , Element.row [] [ Element.el [] (Element.text " ") ]
                    , renderAttachments context project volume
                    , volumeActionButtons context project model volume
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
                    String.join " "
                        [ "(Could not resolve"
                        , context.localization.virtualComputer
                        , "name)"
                        ]
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.padding 0 ])
        [ VH.compactKVRow ((context.localization.virtualComputer |> Helpers.String.toTitleCase) ++ ":") <|
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix <|
                        Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.ServerDetail attachment.serverUuid
                , label =
                    Element.el [ Font.color (SH.toElementColor context.palette.primary) ] <| Element.text (serverName attachment.serverUuid)
                }
        , VH.compactKVRow "Device:" <| Element.text <| attachment.device
        , VH.compactKVRow "Mount point*:" <| (GetterSetters.volDeviceToMountpoint attachment.device |> Maybe.withDefault "" |> Element.text)
        , Element.el [ Font.size 11 ] <|
            Element.text <|
                String.join " "
                    [ "*"
                    , context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    , "will only be automatically formatted/mounted on operating"
                    ]
        , Element.el [ Font.size 11 ] <| Element.text "systems which use systemd 236 or newer (e.g. Ubuntu 18.04 or newer, Rocky Linux, or AlmaLinux)"
        ]


renderAttachments : View.Types.Context -> Project -> OSTypes.Volume -> Element.Element Msg
renderAttachments context project volume =
    case List.length volume.attachments of
        0 ->
            Element.none

        _ ->
            Element.column [ Element.width Element.fill ]
                [ Style.Widgets.Card.exoCard context.palette
                    (Element.column
                        [ Element.padding spacer.px8, Element.spacing spacer.px16 ]
                        [ Element.column [ Element.width Element.fill ]
                            [ Text.subheading context.palette
                                []
                                Element.none
                                "Attached to"
                            , Element.row [ Element.paddingXY 0 spacer.px16 ]
                                [ Element.row (VH.exoColumnAttributes ++ [ Element.padding 0 ]) <| List.map (renderAttachment context project) volume.attachments
                                ]
                            ]
                        ]
                    )
                , Element.row [] [ Element.el [] (Element.text " ") ]
                ]


volumeActionButtons :
    View.Types.Context
    -> Project
    -> Model
    -> OSTypes.Volume
    -> Element.Element Msg
volumeActionButtons context project model volume =
    let
        volDetachDeleteWarning =
            if GetterSetters.isBootVolume Nothing volume then
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
                    Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix
                                (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                    Route.VolumeAttach Nothing (Just volume.uuid)
                                )
                        , label =
                            Button.default
                                context.palette
                                { text = "Attach"
                                , onPress = Just NoOp
                                }
                        }

                OSTypes.InUse ->
                    if GetterSetters.isBootVolume Nothing volume then
                        Button.default
                            context.palette
                            { text = "Detach"
                            , onPress = Nothing
                            }

                    else
                        Button.default
                            context.palette
                            { text = "Detach"
                            , onPress =
                                Just <|
                                    SharedMsg <|
                                        SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                            SharedMsg.RequestDetachVolume model.volumeUuid
                            }

                _ ->
                    Element.none

        confirmationNeeded =
            Set.member volume.uuid model.deleteConfirmations

        deleteButton =
            case ( volume.status, confirmationNeeded ) of
                ( OSTypes.Deleting, _ ) ->
                    Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing

                ( _, True ) ->
                    Element.row [ Element.spacing spacer.px8 ]
                        [ Element.text "Confirm delete?"
                        , deleteIconButton
                            context.palette
                            False
                            "Delete"
                            (Just <| GotDeleteConfirm)
                        , Button.default
                            context.palette
                            { text = "Cancel"
                            , onPress =
                                Just <| GotDeleteCancel
                            }
                        ]

                ( _, False ) ->
                    if volume.status == OSTypes.InUse then
                        deleteIconButton
                            context.palette
                            False
                            "Delete"
                            Nothing

                    else
                        deleteIconButton
                            context.palette
                            False
                            "Delete"
                            (Just <| GotDeleteNeedsConfirm)
    in
    Style.Widgets.Card.exoCard
        context.palette
        (Element.column
            [ Element.padding spacer.px8
            , Element.spacing spacer.px16
            , Element.width Element.fill
            ]
            [ volDetachDeleteWarning
            , Element.row
                [ Element.alignRight
                , Element.spacing spacer.px12
                ]
                [ attachDetachButton
                , deleteButton
                ]
            ]
        )
