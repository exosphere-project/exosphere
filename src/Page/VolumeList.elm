module Page.VolumeList exposing (Model, Msg, init, update, view)

import DateFormat.Relative
import Dict
import Element
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.ResourceList exposing (creationTimeFilterOptions, listItemColumnAttribs, onCreationTimeFilter)
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirmPanel)
import Style.Widgets.Popover exposing (popover)
import Style.Widgets.Text as Text
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    }


type Msg
    = DetachVolume OSTypes.VolumeUuid
    | GotDeleteConfirm OSTypes.VolumeUuid
    | ToggleDeletePopconfirm View.Types.PopoverId
    | SharedMsg SharedMsg.SharedMsg
    | DataListMsg DataList.Msg
    | NoOp


init : Bool -> Model
init showHeading =
    Model showHeading
        (DataList.init <| DataList.getDefaultFilterOptions (filters (Time.millisToPosix 0)))


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
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.RequestDeleteVolume volumeUuid
            )

        ToggleDeletePopconfirm popoverId ->
            ( model
            , Cmd.none
            , SharedMsg.TogglePopover popoverId
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

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
                (volumeRecords project volumes_)
                []
                (filters currentTime)
                Nothing
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if model.showHeading then
            Text.heading context.palette
                []
                (FeatherIcons.hardDrive |> FeatherIcons.toHtml [] |> Element.html |> Element.el [])
                (context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

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
        , creator : String
        }


volumeRecords : Project -> List OSTypes.Volume -> List VolumeRecord
volumeRecords project volumes =
    let
        creator volume =
            if volume.userUuid == project.auth.user.uuid then
                "me"

            else
                "other user"
    in
    List.map
        (\volume ->
            { id = volume.uuid
            , selectable = False
            , volume = volume
            , creator = creator volume
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
                        (Element.text <|
                            VH.possiblyUntitledResource
                                volumeRecord.volume.name
                                context.localization.blockDevice
                        )
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
                        Element.el
                            [ Font.color
                                (SH.toElementColorWithOpacity context.palette.on.background 0.86)
                            ]
                            (Element.text <| "no " ++ context.localization.virtualComputer)
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
                        Button.default
                            context.palette
                            { text = "Detach"
                            , onPress =
                                Just <| DetachVolume volumeRecord.id
                            }

                OSTypes.Available ->
                    -- Volume can be either deleted or attached
                    let
                        popoverId =
                            "volumeListDeletePopconfirm-" ++ project.auth.project.uuid ++ volumeRecord.id
                    in
                    Element.row [ Element.spacing 12 ]
                        [ popover context
                            popoverId
                            (\togglePopoverMsg _ ->
                                deleteIconButton
                                    context.palette
                                    False
                                    "Delete Volume"
                                    (Just <| SharedMsg togglePopoverMsg)
                            )
                            { styleAttrs = []
                            , contents =
                                deletePopconfirmPanel context.palette
                                    { confirmationText =
                                        "Are you sure you want to delete this "
                                            ++ context.localization.blockDevice
                                            ++ "?"
                                    , onConfirm = Just <| GotDeleteConfirm volumeRecord.id

                                    -- TODO: remove it? let popover widget handle toggling popover
                                    , onCancel = Just <| ToggleDeletePopconfirm popoverId
                                    }
                            }
                            ST.PositionBottomRight
                            Nothing
                        , Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.VolumeAttach Nothing (Just volumeRecord.id)
                                    )
                            , label =
                                Button.default
                                    context.palette
                                    { text = "Attach"
                                    , onPress = Just NoOp
                                    }
                            }
                        ]

                _ ->
                    Element.none
    in
    Element.column
        (listItemColumnAttribs context.palette)
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
                    (Element.text volumeRecord.creator)
                ]
            , Element.el [ Element.alignRight ]
                volumeActions
            ]
        ]


filters :
    Time.Posix
    ->
        List
            (DataList.Filter
                { record | volume : OSTypes.Volume, creator : String }
            )
filters currentTime =
    [ { id = "creator"
      , label = "Creator"
      , chipPrefix = "Created by "
      , filterOptions =
            \_ ->
                [ "me", "other user" ]
                    |> List.map (\creator -> ( creator, creator ))
                    |> Dict.fromList
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption <| Set.fromList [ "me" ]
      , onFilter =
            \optionValue volume ->
                volume.creator == optionValue
      }
    , { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> creationTimeFilterOptions
      , filterTypeAndDefaultValue =
            DataList.UniselectOption DataList.UniselectNoChoice
      , onFilter =
            \optionValue volume ->
                onCreationTimeFilter optionValue volume.volume.createdAt currentTime
      }
    ]
