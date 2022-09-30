module Page.ServerList exposing (Model, Msg, init, update, view)

import DateFormat.Relative
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Interaction as IHelpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (creationTimeFilterOptions, listItemColumnAttribs, onCreationTimeFilter)
import Helpers.String
import Html.Attributes as HtmlA
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH exposing (spacer)
import Style.Types as ST
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)
import Style.Widgets.Icon as Icon
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.StatusBadge as StatusBadge
import Style.Widgets.Text as Text
import Time
import Types.Interaction as ITypes
import Types.Project exposing (Project)
import Types.Server exposing (Server, ServerOrigin(..), ServerUiStatus(..))
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    }


type Msg
    = GotDeleteConfirm OSTypes.ServerUuid
    | OpenInteraction String
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Project -> Bool -> Model
init project showHeading =
    Model showHeading
        (DataList.init <|
            DataList.getDefaultFilterOptions
                (filters project.auth.user.name (Time.millisToPosix 0))
        )


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotDeleteConfirm serverId ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg serverId <|
                    SharedMsg.RequestDeleteServer False
            )

        OpenInteraction url ->
            ( model
            , Cmd.none
            , SharedMsg.OpenNewWindow url
            )

        DataListMsg dataListMsg ->
            ( { model
                | dataListModel =
                    DataList.update dataListMsg model.dataListModel
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        serverListContents =
            {- Resolve whether we have a loaded list of servers to display; if so, call rendering function serverList_ -}
            case ( project.servers.data, project.servers.refreshStatus ) of
                ( RDPP.DontHave, RDPP.NotLoading Nothing ) ->
                    Element.row [ Element.spacing spacer.px16 ]
                        [ Widget.circularProgressIndicator
                            (SH.materialStyle context.palette).progressIndicator
                            Nothing
                        , Element.text "Please wait..."
                        ]

                ( RDPP.DontHave, RDPP.NotLoading (Just ( httpErrorWithBody, _ )) ) ->
                    Element.paragraph
                        []
                        [ Element.text <|
                            String.concat
                                [ "Cannot display"
                                , context.localization.virtualComputer
                                    |> Helpers.String.pluralize
                                , ". Error message: " ++ Helpers.httpErrorToString httpErrorWithBody.error
                                ]
                        ]

                ( RDPP.DontHave, RDPP.Loading ) ->
                    Element.row [ Element.spacing spacer.px16 ]
                        [ Widget.circularProgressIndicator
                            (SH.materialStyle context.palette).progressIndicator
                            Nothing
                        , Element.text "Loading..."
                        ]

                ( RDPP.DoHave servers _, _ ) ->
                    if List.isEmpty servers then
                        Element.paragraph
                            []
                            [ Element.text <|
                                String.join " "
                                    [ "You don't have any"
                                    , context.localization.virtualComputer
                                        |> Helpers.String.pluralize
                                    , "yet, go create one!"
                                    ]
                            ]

                    else
                        let
                            serversList =
                                serverRecords context currentTime project servers
                        in
                        DataList.view
                            model.dataListModel
                            DataListMsg
                            context
                            []
                            (serverView context currentTime project)
                            serversList
                            [ deletionAction context project ]
                            (Just
                                { filters = filters project.auth.user.name currentTime
                                , dropdownMsgMapper =
                                    \dropdownId ->
                                        SharedMsg <| SharedMsg.TogglePopover dropdownId
                                }
                            )
                            Nothing
    in
    Element.column VH.contentContainer
        [ if model.showHeading then
            Text.heading context.palette
                []
                (FeatherIcons.server
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

          else
            Element.none
        , Element.column [ Element.spacing spacer.px12 ]
            [ Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Compute project.computeQuota)
            , serverListContents
            ]
        ]


type alias ServerRecord msg =
    DataList.DataRecord
        { name : String
        , status : ServerUiStatus
        , size : String
        , floatingIpAddress : Maybe String
        , creationTime : Time.Posix
        , creator : String
        , interactions :
            List
                { interactionStatus : ITypes.InteractionStatus
                , interactionDetails : ITypes.InteractionDetails msg
                }
        }


serverRecords :
    View.Types.Context
    -> Time.Posix
    -> Project
    -> List Server
    -> List (ServerRecord msg)
serverRecords context currentTime project servers =
    let
        creatorName server =
            case server.exoProps.serverOrigin of
                ServerFromExo exoOriginProps ->
                    case exoOriginProps.exoCreatorUsername of
                        Just creatorUsername ->
                            creatorUsername

                        Nothing ->
                            "unknown user"

                _ ->
                    "unknown user"

        floatingIpAddress server =
            List.head (GetterSetters.getServerFloatingIps project server.osProps.uuid)
                |> Maybe.map .address

        flavor server =
            GetterSetters.flavorLookup project server.osProps.details.flavorId
                |> Maybe.map .name
                |> Maybe.withDefault ("unknown " ++ context.localization.virtualComputerHardwareConfig)

        interactions server =
            [ ITypes.GuacTerminal
            , ITypes.GuacDesktop
            , ITypes.Console
            ]
                |> List.map
                    (\interaction ->
                        { interactionStatus =
                            IHelpers.interactionStatus
                                project
                                server
                                interaction
                                context
                                currentTime
                                (GetterSetters.getUserAppProxyFromContext project context)
                        , interactionDetails =
                            IHelpers.interactionDetails
                                interaction
                                context
                        }
                    )
    in
    List.map
        (\server ->
            { id = server.osProps.uuid
            , selectable = server.osProps.details.lockStatus == OSTypes.ServerUnlocked
            , name = server.osProps.name
            , status = VH.getServerUiStatus server
            , size = flavor server
            , floatingIpAddress = floatingIpAddress server
            , creationTime = server.osProps.details.created
            , creator = creatorName server
            , interactions = interactions server
            }
        )
        servers


serverView :
    View.Types.Context
    -> Time.Posix
    -> Project
    -> ServerRecord Never
    -> Element.Element Msg
serverView context currentTime project serverRecord =
    let
        serverLink =
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.ServerDetail serverRecord.id
                        )
                , label =
                    Element.el
                        [ Font.size 18
                        , Font.color (SH.toElementColor context.palette.primary)
                        ]
                        (Element.text serverRecord.name)
                }

        statusColor =
            serverRecord.status
                |> VH.getServerUiStatusBadgeState
                |> StatusBadge.toColors context.palette
                |> .default
                |> SH.toElementColor

        statusText =
            VH.getServerUiStatusStr serverRecord.status

        statusTextToDisplay =
            case serverRecord.status of
                ServerUiStatusDeleting ->
                    -- because server appears for a while in the DataList
                    -- after delete action is taken
                    Element.el [ Font.italic ] <|
                        Element.text (statusText ++ " ...")

                _ ->
                    Element.none

        interactionPopover closePopover =
            Element.column []
                (List.map
                    (\{ interactionStatus, interactionDetails } ->
                        Element.el [ closePopover, Element.width Element.fill ] <|
                            Widget.button
                                (SH.dropdownItemStyle context.palette)
                                { text = interactionDetails.name
                                , icon =
                                    Element.el []
                                        (interactionDetails.icon (SH.toElementColor context.palette.primary) 18)
                                , onPress =
                                    case interactionStatus of
                                        ITypes.Ready url ->
                                            Just <| OpenInteraction url

                                        ITypes.Warn url _ ->
                                            Just <| OpenInteraction url

                                        _ ->
                                            Nothing
                                }
                    )
                    serverRecord.interactions
                )

        interactionButton =
            let
                target togglePopover popoverIsShown =
                    Widget.iconButton
                        (SH.materialStyle context.palette).button
                        { text = "Connect to"
                        , icon =
                            Element.row
                                [ Element.spacing spacer.px4 ]
                                [ Element.text "Connect to"
                                , Element.el []
                                    ((if popoverIsShown then
                                        FeatherIcons.chevronUp

                                      else
                                        FeatherIcons.chevronDown
                                     )
                                        |> FeatherIcons.withSize 18
                                        |> FeatherIcons.toHtml []
                                        |> Element.html
                                    )
                                ]
                        , onPress = Just togglePopover
                        }

                interactionPopoverId =
                    Helpers.String.hyphenate
                        [ "serverListInteractionPopover"
                        , project.auth.project.uuid
                        , serverRecord.id
                        ]
            in
            popover context
                (\interactionPopoverId_ -> SharedMsg <| SharedMsg.TogglePopover interactionPopoverId_)
                { id = interactionPopoverId
                , content = interactionPopover
                , contentStyleAttrs = []
                , position = ST.PositionBottomLeft
                , distanceToTarget = Nothing
                , target = target
                , targetStyleAttrs = []
                }

        deleteServerBtnWithPopconfirm =
            let
                deleteServerBtn togglePopconfirm _ =
                    deleteIconButton context.palette
                        False
                        ("Delete " ++ context.localization.virtualComputer)
                        (if serverRecord.selectable then
                            Just togglePopconfirm

                         else
                            -- to disable it
                            Nothing
                        )

                deletePopconfirmId =
                    Helpers.String.hyphenate
                        [ "serverListDeletePopconfirm"
                        , project.auth.project.uuid
                        , serverRecord.id
                        ]
            in
            deletePopconfirm context
                (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
                deletePopconfirmId
                { confirmationText =
                    "Are you sure you want to delete this "
                        ++ context.localization.virtualComputer
                        ++ "?"
                , onConfirm = Just <| GotDeleteConfirm serverRecord.id
                , onCancel = Just NoOp
                }
                ST.PositionBottomRight
                deleteServerBtn

        floatingIpView =
            case serverRecord.floatingIpAddress of
                Just floatingIpAddress ->
                    Element.row [ Element.spacing spacer.px8 ]
                        [ Icon.ipAddress
                            (SH.toElementColor
                                context.palette.neutral.icon
                            )
                            16
                        , Element.el [] (Element.text floatingIpAddress)
                        ]

                Nothing ->
                    Element.none
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ serverLink
            , Element.el
                [ Element.width (Element.px 12)
                , Element.height (Element.px 12)
                , Border.rounded 6
                , Background.color statusColor
                , Element.htmlAttribute <| HtmlA.title statusText
                ]
                Element.none
            , statusTextToDisplay
            , Element.el [ Element.alignRight ] interactionButton
            , Element.el [ Element.alignRight ] deleteServerBtnWithPopconfirm
            ]
        , Element.row
            [ Element.spacing spacer.px8
            , Element.width Element.fill
            ]
            [ Element.el [] (Element.text serverRecord.size)
            , Element.text "·"
            , Element.paragraph []
                [ Element.text "created "
                , Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
                    (Element.text <|
                        DateFormat.Relative.relativeTime currentTime
                            serverRecord.creationTime
                    )
                , Element.text " by "
                , Element.el [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
                    (Element.text serverRecord.creator)
                ]
            , floatingIpView
            ]
        ]


deletionAction :
    View.Types.Context
    -> Project
    -> Set.Set OSTypes.ServerUuid
    -> Element.Element Msg
deletionAction context project serverIds =
    Element.el [ Element.alignRight ] <|
        deleteIconButton context.palette
            True
            "Delete All"
            (Just <|
                SharedMsg
                    (SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project)
                        (SharedMsg.RequestDeleteServers (Set.toList serverIds))
                    )
            )


filters :
    String
    -> Time.Posix
    ->
        List
            (DataList.Filter
                { record
                    | creator : String
                    , creationTime : Time.Posix
                }
            )
filters currentUser currentTime =
    let
        creatorFilterOptionValues servers =
            List.map .creator servers
                |> Set.fromList
                |> Set.toList
    in
    [ { id = "creator"
      , label = "Creator"
      , chipPrefix = "Created by "
      , filterOptions =
            \servers ->
                creatorFilterOptionValues servers
                    |> List.map
                        (\creator ->
                            ( creator
                            , if creator == currentUser then
                                "me (" ++ creator ++ ")"

                              else
                                creator
                            )
                        )
                    |> Dict.fromList
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption <| Set.fromList [ currentUser ]
      , onFilter =
            \optionValue server ->
                server.creator == optionValue
      }
    , { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> creationTimeFilterOptions
      , filterTypeAndDefaultValue =
            DataList.UniselectOption DataList.UniselectNoChoice
      , onFilter =
            \optionValue server ->
                onCreationTimeFilter optionValue server.creationTime currentTime
      }
    ]
