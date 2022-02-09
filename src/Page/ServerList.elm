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
import Helpers.String
import Html.Attributes as HtmlA
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Widgets.DataList as DataList
import Style.Widgets.Icon as Icon
import Style.Widgets.StatusBadge as StatusBadge
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
    , showInteractionPopover : Dict.Dict OSTypes.ServerUuid Bool
    , showDeletePopconfirm : Dict.Dict OSTypes.ServerUuid Bool
    , dataListModel : DataList.Model
    }


type alias DeleteConfirmation =
    OSTypes.ServerUuid


type Msg
    = GotDeleteConfirm DeleteConfirmation
    | ShowDeletePopconfirm OSTypes.ServerUuid Bool
    | ToggleInteractionPopover OSTypes.ServerUuid
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Project -> Bool -> Model
init project showHeading =
    Model showHeading
        Dict.empty
        Dict.empty
        (DataList.init <| DataList.getDefaultFilterOptions (filters project.auth.user.name))


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotDeleteConfirm serverId ->
            ( { model
                | showDeletePopconfirm =
                    Dict.insert serverId False model.showDeletePopconfirm
              }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg serverId <|
                    SharedMsg.RequestDeleteServer False
            )

        ShowDeletePopconfirm serverId bool ->
            ( { model
                | showDeletePopconfirm =
                    Dict.insert serverId bool model.showDeletePopconfirm
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        ToggleInteractionPopover serverId ->
            ( { model
                | showInteractionPopover =
                    case Dict.get serverId model.showInteractionPopover of
                        Just showPopover ->
                            Dict.insert serverId (not showPopover) model.showInteractionPopover

                        Nothing ->
                            -- no data is saved in dict means 1st click
                            Dict.insert serverId True model.showInteractionPopover
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
                    Element.row [ Element.spacing 15 ]
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
                    Element.row [ Element.spacing 15 ]
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
                            deletionAction : Set.Set String -> Element.Element Msg
                            deletionAction =
                                \serverIds ->
                                    Element.el [ Element.alignRight ]
                                        (Widget.iconButton
                                            (SH.materialStyle context.palette).dangerButton
                                            { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                                            , text = "Delete"
                                            , onPress =
                                                Just <|
                                                    SharedMsg
                                                        (SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project)
                                                            (SharedMsg.RequestDeleteServers
                                                                (List.map stringToUuid
                                                                    (Set.toList serverIds)
                                                                )
                                                            )
                                                        )
                                            }
                                        )

                            serversList =
                                serverRecords context currentTime project servers
                        in
                        DataList.view
                            model.dataListModel
                            DataListMsg
                            context.palette
                            []
                            (serverView model context project)
                            serversList
                            [ deletionAction ]
                            (filters project.auth.user.name)
    in
    Element.column [ Element.width Element.fill ]
        [ if model.showHeading then
            Element.row (VH.heading2 context.palette ++ [ Element.spacing 15 ])
                [ FeatherIcons.server |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                , Element.text <|
                    (context.localization.virtualComputer
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )
                ]

          else
            Element.none
        , Element.column VH.contentContainer
            [ Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Compute project.computeQuota)
            , serverListContents
            ]
        ]


stringToUuid : String -> OSTypes.ServerUuid
stringToUuid =
    identity


type alias ServerRecord msg =
    DataList.DataRecord
        { name : String
        , status : ServerUiStatus
        , size : String
        , floatingIpAddress : Maybe String
        , creationTime : String
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

        creationTimeStr server =
            DateFormat.Relative.relativeTime currentTime
                server.osProps.details.created

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
                                (VH.userAppProxyLookup context project)
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
            , creationTime = creationTimeStr server
            , creator = creatorName server
            , interactions = interactions server
            }
        )
        servers


serverView : Model -> View.Types.Context -> Project -> ServerRecord Never -> Element.Element Msg
serverView model context project serverRecord =
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
                |> Tuple.first

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

        dropdownItemStyle =
            let
                textButtonDefaults =
                    (SH.materialStyle context.palette).textButton
            in
            { textButtonDefaults
                | container =
                    textButtonDefaults.container
                        ++ [ Element.width Element.fill
                           , Font.size 16
                           , Font.medium
                           , Font.letterSpacing 0.8
                           , Element.paddingXY 8 12
                           , Element.height Element.shrink
                           ]
            }

        popoverStyle =
            [ Background.color <| SH.toElementColor context.palette.background
            , Border.width 1
            , Border.color <| SH.toElementColorWithOpacity context.palette.on.background 0.16
            , Border.shadow SH.shadowDefaults
            ]

        interactionPopover =
            Element.el [ Element.paddingXY 0 6 ] <|
                Element.column
                    (popoverStyle ++ [ Element.padding 10 ])
                    (List.map
                        (\{ interactionStatus, interactionDetails } ->
                            Widget.button
                                dropdownItemStyle
                                { text = interactionDetails.name
                                , icon =
                                    Element.el
                                        [ Element.paddingEach
                                            { top = 0
                                            , right = 5
                                            , left = 0
                                            , bottom = 0
                                            }
                                        ]
                                        (interactionDetails.icon (SH.toElementColor context.palette.primary) 18)
                                , onPress =
                                    case interactionStatus of
                                        ITypes.Ready url ->
                                            Just <| SharedMsg <| SharedMsg.OpenNewWindow url

                                        ITypes.Warn url _ ->
                                            Just <| SharedMsg <| SharedMsg.OpenNewWindow url

                                        _ ->
                                            Nothing
                                }
                        )
                        serverRecord.interactions
                    )

        interactionButton =
            let
                showPopover =
                    Dict.get serverRecord.id model.showInteractionPopover
                        |> Maybe.withDefault False

                ( attribs, buttonIcon ) =
                    if showPopover then
                        ( [ Element.below interactionPopover ], FeatherIcons.chevronUp )

                    else
                        ( [], FeatherIcons.chevronDown )
            in
            Element.el
                ([] ++ attribs)
            <|
                Widget.iconButton
                    (SH.materialStyle context.palette).button
                    { text = "Connect to"
                    , icon =
                        Element.row
                            [ Element.spacing 5 ]
                            [ Element.text "Connect to"
                            , Element.el []
                                (buttonIcon
                                    |> FeatherIcons.withSize 18
                                    |> FeatherIcons.toHtml []
                                    |> Element.html
                                )
                            ]
                    , onPress = Just <| ToggleInteractionPopover serverRecord.id
                    }

        deleteServerButton =
            let
                dangerBtnStyleDefaults =
                    (SH.materialStyle context.palette).dangerButtonSecondary

                deleteBtnStyle =
                    { dangerBtnStyleDefaults
                        | container =
                            dangerBtnStyleDefaults.container
                                ++ [ Element.htmlAttribute <| HtmlA.title "Delete"
                                   ]
                        , labelRow =
                            dangerBtnStyleDefaults.labelRow
                                ++ [ Element.width Element.shrink
                                   , Element.paddingXY 4 0
                                   ]
                    }

                showDeletePopconfirm =
                    Dict.get serverRecord.id model.showDeletePopconfirm
                        |> Maybe.withDefault False

                popconfirmAttribs =
                    if showDeletePopconfirm then
                        [ Element.below deletePopconfirm ]

                    else
                        []
            in
            Element.el popconfirmAttribs <|
                Widget.iconButton
                    deleteBtnStyle
                    { icon = FeatherIcons.trash |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] |> Element.html
                    , text = "Delete"
                    , onPress =
                        if serverRecord.selectable then
                            Just <| ShowDeletePopconfirm serverRecord.id True

                        else
                            -- to disable it
                            Nothing
                    }

        deletePopconfirm =
            Element.el [ Element.paddingXY 0 6, Element.alignRight ] <|
                Element.column (popoverStyle ++ [ Element.padding 16, Element.spacing 16 ])
                    [ Element.row [ Element.spacing 8 ]
                        [ FeatherIcons.alertCircle
                            |> FeatherIcons.withSize 20
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                        , Element.text <|
                            "Are you sure to delete this "
                                ++ context.localization.virtualComputer
                                ++ "?"
                        ]
                    , Element.row [ Element.spacing 10, Element.alignRight ]
                        [ Widget.textButton (SH.materialStyle context.palette).button
                            { text = "Cancel"
                            , onPress = Just <| ShowDeletePopconfirm serverRecord.id False
                            }
                        , Widget.textButton (SH.materialStyle context.palette).dangerButton
                            { text = "Delete"
                            , onPress = Just <| GotDeleteConfirm serverRecord.id
                            }
                        ]
                    ]

        floatingIpView =
            case serverRecord.floatingIpAddress of
                Just floatingIpAddress ->
                    Element.row [ Element.spacing 8 ]
                        [ Icon.ipAddress
                            (SH.toElementColorWithOpacity
                                context.palette.on.background
                                0.62
                            )
                            16
                        , Element.el [] (Element.text floatingIpAddress)
                        ]

                Nothing ->
                    Element.none
    in
    Element.column
        [ Element.spacing 12
        , Element.width Element.fill
        , Font.color (SH.toElementColorWithOpacity context.palette.on.background 0.62)
        ]
        [ Element.row [ Element.spacing 10, Element.width Element.fill ]
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
            , Element.el [ Element.alignRight ] deleteServerButton
            ]
        , Element.row
            [ Element.spacing 8
            , Element.width Element.fill
            ]
            [ Element.el [] (Element.text serverRecord.size)
            , Element.text "·"
            , Element.paragraph []
                [ Element.text "created "
                , Element.el [ Font.color (SH.toElementColor context.palette.on.background) ]
                    (Element.text serverRecord.creationTime)
                , Element.text " by "
                , Element.el [ Font.color (SH.toElementColor context.palette.on.background) ]
                    (Element.text serverRecord.creator)
                ]
            , floatingIpView
            ]
        ]


filters :
    String
    ->
        List
            (DataList.Filter
                { record
                    | creator : String
                    , creationTime : String
                }
            )
filters currentUser =
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
    ]
