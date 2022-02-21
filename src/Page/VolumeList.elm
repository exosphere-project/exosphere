module Page.VolumeList exposing (Model, Msg, init, update, view)

import DateFormat.Relative
import Element
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Style.Helpers as SH
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , shownDeletePopconfirm : Maybe OSTypes.VolumeUuid
    , dataListModel : DataList.Model
    }


type Msg
    = DetachVolume OSTypes.VolumeUuid
    | GotDeleteConfirm OSTypes.VolumeUuid
    | ShowDeletePopconfirm OSTypes.VolumeUuid Bool
    | DataListMsg DataList.Msg
    | NoOp


init : Bool -> Model
init showHeading =
    Model showHeading
        Nothing
        (DataList.init <| DataList.getDefaultFilterOptions [])


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        DetachVolume volumeUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestDetachVolume volumeUuid
            )

        GotDeleteConfirm volumeUuid ->
            ( { model | shownDeletePopconfirm = Nothing }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestDeleteVolume volumeUuid
            )

        ShowDeletePopconfirm volumeUuid toBeShown ->
            ( { model
                | shownDeletePopconfirm =
                    if toBeShown then
                        Just volumeUuid

                    else
                        Nothing
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        DataListMsg dataListMsg ->
            ( { model
                | dataListModel =
                    DataList.update dataListMsg model.dataListModel
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        NoOp ->
            ( model
            , Cmd.none
            , SharedMsg.NoOp
            )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        renderSuccessCase : List OSTypes.Volume -> Element.Element Msg
        renderSuccessCase volumes_ =
            DataList.view
                model.dataListModel
                DataListMsg
                context.palette
                []
                (volumeView model context project currentTime)
                (volumeRecords volumes_)
                []
                []
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if model.showHeading then
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
            [ Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Volume project.volumeQuota)
            , VH.renderWebData
                context
                project.volumes
                (Helpers.String.pluralize context.localization.blockDevice)
                renderSuccessCase
            ]
        ]


type alias VolumeRecord =
    DataList.DataRecord
        { volume : OSTypes.Volume
        }


volumeRecords : List OSTypes.Volume -> List VolumeRecord
volumeRecords volumes =
    List.map
        (\volume ->
            { id = volume.uuid
            , selectable = False
            , volume = volume
            }
        )
        volumes


volumeView :
    Model
    -> View.Types.Context
    -> Project
    -> Time.Posix
    -> VolumeRecord
    -> Element.Element Msg
volumeView model context project currentTime volumeRecord =
    let
        volumeLink =
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.VolumeDetail volumeRecord.id
                        )
                , label =
                    Element.el
                        [ Font.size 18
                        , Font.color (SH.toElementColor context.palette.primary)
                        ]
                        (Element.text volumeRecord.volume.name)
                }

        volumeAttachment =
            let
                serverName serverUuid =
                    case GetterSetters.serverLookup project serverUuid of
                        Just server ->
                            server.osProps.name

                        Nothing ->
                            "unresolvable " ++ context.localization.virtualComputer ++ " name"
            in
            Element.row [ Element.alignRight ]
                [ Element.text "Attached to "
                , case List.head volumeRecord.volume.attachments of
                    Just volumeAttachment_ ->
                        Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.ServerDetail volumeAttachment_.serverUuid
                                    )
                            , label =
                                Element.el
                                    [ Font.color (SH.toElementColor context.palette.primary)
                                    ]
                                    (Element.text <| serverName volumeAttachment_.serverUuid)
                            }

                    Nothing ->
                        Element.text <| "No " ++ context.localization.virtualComputer
                ]

        volumeActions =
            case volumeRecord.volume.status of
                OSTypes.Detaching ->
                    Element.el [ Font.italic ] (Element.text "Detaching ...")

                OSTypes.Deleting ->
                    Element.el [ Font.italic ] (Element.text "Deleting ...")

                OSTypes.InUse ->
                    if GetterSetters.isBootVolume Nothing volumeRecord.volume then
                        -- Volume cannot be deleted or detached
                        Element.row [ Element.height <| Element.minimum 32 Element.fill ]
                            [ Element.text "as "
                            , Element.el
                                [ Font.color (SH.toElementColor context.palette.on.background) ]
                                (Element.text <| "boot " ++ context.localization.blockDevice)
                            ]

                    else
                        -- Volume can only be detached
                        Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Detach"
                            , onPress =
                                Just <| DetachVolume volumeRecord.id
                            }

                OSTypes.Available ->
                    -- Volume can be either deleted or attached
                    let
                        showDeletePopconfirm =
                            case model.shownDeletePopconfirm of
                                Just shownDeletePopconfirmVolumeId ->
                                    shownDeletePopconfirmVolumeId == volumeRecord.id

                                Nothing ->
                                    False
                    in
                    Element.row [ Element.spacing 12 ]
                        [ Element.el
                            (if showDeletePopconfirm then
                                [ Element.below <|
                                    deletePopconfirm context.palette
                                        { confirmationText =
                                            "Are you sure you want to delete this "
                                                ++ context.localization.blockDevice
                                                ++ "?"
                                        , onConfirm = Just <| GotDeleteConfirm volumeRecord.id
                                        , onCancel = Just <| ShowDeletePopconfirm volumeRecord.id False
                                        }
                                ]

                             else
                                []
                            )
                            (deleteIconButton
                                context.palette
                                False
                                "Delete Volume"
                                (Just <| ShowDeletePopconfirm volumeRecord.id True)
                            )
                        , Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.VolumeAttach Nothing (Just volumeRecord.id)
                                    )
                            , label =
                                Widget.textButton
                                    (SH.materialStyle context.palette).button
                                    { text = "Attach"
                                    , onPress = Just NoOp
                                    }
                            }
                        ]

                _ ->
                    Element.none
    in
    Element.column
        [ Element.spacing 12
        , Element.width Element.fill
        , Font.color (SH.toElementColorWithOpacity context.palette.on.background 0.62)
        ]
        [ Element.row [ Element.spacing 10, Element.width Element.fill ]
            [ volumeLink
            , volumeAttachment
            ]
        , Element.row
            [ Element.spacing 8
            , Element.width Element.fill
            ]
            [ Element.el [] (Element.text <| String.fromInt volumeRecord.volume.size ++ " GB")
            , Element.text "·"
            , Element.paragraph []
                [ Element.text "created "
                , Element.el [ Font.color (SH.toElementColor context.palette.on.background) ]
                    (Element.text <|
                        DateFormat.Relative.relativeTime currentTime
                            volumeRecord.volume.createdAt
                    )
                , Element.text " by "
                , Element.el [ Font.color (SH.toElementColor context.palette.on.background) ]
                    (Element.text
                        (if volumeRecord.volume.userUuid == project.auth.user.uuid then
                            "me"

                         else
                            "other user"
                        )
                    )
                ]
            , Element.el [ Element.alignRight ]
                volumeActions
            ]
        ]
