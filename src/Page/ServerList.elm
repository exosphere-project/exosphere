module Page.ServerList exposing (Model, Msg, init, update, view)

import Dict
import Element
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers exposing (serverCreatorName)
import Helpers.Interaction as IHelpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (creationTimeFilterOptions, listItemColumnAttribs, onCreationTimeFilter)
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton)
import Style.Widgets.HumanTime exposing (relativeTimeElement)
import Style.Widgets.Icon as Icon
import Style.Widgets.Popover.Popover exposing (dropdownItemStyle, popover)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Spinner as Spinner
import Style.Widgets.StatusBadge as StatusBadge
import Style.Widgets.Tag exposing (tag)
import Style.Widgets.Text as Text
import Style.Widgets.Uuid exposing (uuidLabel)
import Time
import Types.Guacamole exposing (LaunchedWithGuacProps)
import Types.Interaction as ITypes
import Types.Interactivity exposing (InteractionLevel(..))
import Types.Project exposing (Project)
import Types.Server exposing (Server, ServerUiStatus)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    , retainFloatingIpsWhenDeleting : Bool
    }


type Msg
    = GotDeleteConfirm OSTypes.ServerUuid
    | GotRetainFloatingIpsWhenDeleting Bool
    | OpenInteraction String
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : View.Types.Context -> Project -> Bool -> Model
init context project showHeading =
    Model showHeading
        (DataList.init <|
            DataList.getDefaultFilterOptions
                (filters context project project.auth.user.name (Time.millisToPosix 0))
        )
        False


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotDeleteConfirm serverId ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg serverId <|
                    SharedMsg.RequestDeleteServer model.retainFloatingIpsWhenDeleting
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

        GotRetainFloatingIpsWhenDeleting retain ->
            ( { model | retainFloatingIpsWhenDeleting = retain }, Cmd.none, SharedMsg.NoOp )

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
                        [ Spinner.medium context.palette
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
                        [ Spinner.medium context.palette
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
                            context.localization.virtualComputer
                            model.dataListModel
                            DataListMsg
                            context
                            []
                            (serverView context currentTime project model.retainFloatingIpsWhenDeleting)
                            serversList
                            [ deletionAction context project ]
                            (Just
                                { filters = filters context project project.auth.user.name currentTime
                                , dropdownMsgMapper =
                                    \dropdownId ->
                                        SharedMsg <| SharedMsg.TogglePopover dropdownId
                                }
                            )
                            (Just <| searchByNameUuidFilter context)
    in
    Element.column (VH.contentContainer ++ [ Element.spacing spacer.px32 ])
        [ if model.showHeading then
            Text.heading context.palette
                []
                (Icon.featherIcon [] Icons.server)
                (context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

          else
            Element.none
        , Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Compute project.computeQuota)
        , serverListContents
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
        , securityGroupIds : List OSTypes.SecurityGroupUuid
        , guacProps : Maybe LaunchedWithGuacProps
        }


serverRecords :
    View.Types.Context
    -> Time.Posix
    -> Project
    -> List Server
    -> List (ServerRecord msg)
serverRecords context currentTime project servers =
    let
        floatingIpAddress server =
            List.head (GetterSetters.getServerFloatingIps project server.osProps.uuid)
                |> Maybe.map .address

        flavor server =
            GetterSetters.flavorLookup project server.osProps.details.flavorId

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

        serverSecurityGroupIds server =
            GetterSetters.getServerSecurityGroups project server.osProps.uuid
                |> RDPP.withDefault []
                |> List.map .uuid
    in
    List.map
        (\server ->
            { id = server.osProps.uuid
            , selectable = server.osProps.details.lockStatus == OSTypes.ServerUnlocked
            , name = VH.resourceName (Just server.osProps.name) server.osProps.uuid
            , status = VH.getServerUiStatus project server
            , size = flavorName context <| flavor server -- comparable flavor name
            , floatingIpAddress = floatingIpAddress server
            , creationTime = server.osProps.details.created
            , creator = serverCreatorName project server
            , interactions = interactions server
            , securityGroupIds = serverSecurityGroupIds server
            , guacProps = IHelpers.getLaunchedWithGaucamoleProps server
            }
        )
        servers


flavorName : View.Types.Context -> Maybe OSTypes.Flavor -> String
flavorName context flavor =
    flavor
        |> Maybe.map .name
        |> Maybe.withDefault ("unknown " ++ context.localization.virtualComputerHardwareConfig)


serverView :
    View.Types.Context
    -> Time.Posix
    -> Project
    -> Bool
    -> ServerRecord Never
    -> Element.Element Msg
serverView context currentTime project retainFloatingIpsWhenDeleting serverRecord =
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
                        (Text.typographyAttrs Text.Emphasized ++ [ Font.color (SH.toElementColor context.palette.primary) ])
                        (Element.text serverRecord.name)
                }

        interactionPopover closePopover =
            Element.column []
                (List.map
                    (\{ interactionStatus, interactionDetails } ->
                        Element.el [ closePopover, Element.width Element.fill ] <|
                            Widget.button
                                (dropdownItemStyle context.palette)
                                { text = interactionDetails.name
                                , icon =
                                    Element.el []
                                        (case interactionStatus of
                                            ITypes.Loading ->
                                                Spinner.sized 18 context.palette

                                            _ ->
                                                interactionDetails.icon 18
                                        )
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
                                , Icon.sizedFeatherIcon 18 <|
                                    if popoverIsShown then
                                        Icons.chevronUp

                                    else
                                        Icons.chevronDown
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
                (\interactionPopoverId_ ->
                    SharedMsg <|
                        SharedMsg.Batch
                            [ SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                SharedMsg.ServerMsg serverRecord.id <|
                                    SharedMsg.SetMinimumServerInteractivity LowInteraction
                            , SharedMsg.TogglePopover interactionPopoverId_
                            ]
                )
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
                renderKeepFloatingIpCheckbox : Element.Element Msg
                renderKeepFloatingIpCheckbox =
                    if not <| List.isEmpty <| GetterSetters.getServerFloatingIps project serverRecord.id then
                        Input.checkbox
                            []
                            { onChange = GotRetainFloatingIpsWhenDeleting
                            , icon = Input.defaultCheckbox
                            , checked = retainFloatingIpsWhenDeleting
                            , label =
                                Input.labelRight []
                                    (String.join " "
                                        [ "Keep the"
                                        , context.localization.floatingIpAddress
                                        , "of this"
                                        , context.localization.virtualComputer
                                        , "for future use"
                                        ]
                                        |> Text.text Text.Small []
                                    )
                            }

                    else
                        Element.none

                deleteServerBtn togglePopconfirm _ =
                    deleteIconButton
                        context.palette
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
            popover context
                (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
                { id = deletePopconfirmId
                , content =
                    \confirmEl ->
                        Element.column [ Element.spacing spacer.px8, Element.padding spacer.px4 ] <|
                            [ Style.Widgets.DeleteButton.deletePopconfirmContent
                                context.palette
                                { confirmation =
                                    Element.text <|
                                        "Are you sure you want to delete this "
                                            ++ context.localization.virtualComputer
                                            ++ "?"
                                , buttonText = Nothing
                                , onCancel = Just NoOp
                                , onConfirm =
                                    Just <| GotDeleteConfirm serverRecord.id
                                }
                                confirmEl
                            , Element.el [ Element.padding spacer.px4 ] renderKeepFloatingIpCheckbox
                            ]
                , contentStyleAttrs = []
                , position = ST.PositionBottomRight
                , distanceToTarget = Nothing
                , target = deleteServerBtn
                , targetStyleAttrs = []
                }

        floatingIpView =
            case serverRecord.floatingIpAddress of
                Just floatingIpAddress ->
                    Element.row [ Element.spacing spacer.px8, Element.alignRight ]
                        [ Icon.ipAddress
                            (SH.toElementColor
                                context.palette.neutral.icon
                            )
                            16
                        , Text.text Text.Small [ Text.fontFamily Text.Mono ] floatingIpAddress
                        ]

                Nothing ->
                    Element.none

        guiTag =
            serverRecord.guacProps
                |> Maybe.map .vncSupported
                |> Maybe.withDefault False
                |> (\vncSupported ->
                        if vncSupported then
                            tag context.palette context.localization.graphicalDesktopEnvironment

                        else
                            Element.none
                   )
    in
    Element.column
        (listItemColumnAttribs context.palette)
        [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
            [ serverLink
            , VH.serverStatusBadgeFromStatus context.palette StatusBadge.Small serverRecord.status
            , Element.el [ Element.alignRight ] guiTag
            , Element.el [ Element.alignRight ] interactionButton
            , Element.el [ Element.alignRight ] deleteServerBtnWithPopconfirm
            ]
        , Element.row
            [ Element.spacingXY spacer.px8 0
            , Element.width Element.fill
            ]
            [ Element.el [ Element.alignTop ] (Element.text serverRecord.size)
            , Text.text Text.Body [ Element.alignTop ] "·"
            , let
                accentColor =
                    SH.toElementColor context.palette.neutral.text.default

                accented : Element.Element msg -> Element.Element msg
                accented inner =
                    Element.el [ Font.color accentColor ] inner
              in
              Element.paragraph [ Element.alignTop ]
                [ Element.text "created "
                , accented (relativeTimeElement currentTime serverRecord.creationTime)
                , Element.text " by "
                , accented (Element.text serverRecord.creator)
                ]
            , Element.column [ Element.spacing spacer.px16, Element.paddingXY 0 spacer.px4 ]
                [ uuidLabel context.palette serverRecord.id
                , floatingIpView
                ]
            ]
        ]


deletionAction :
    View.Types.Context
    -> Project
    -> Set.Set OSTypes.ServerUuid
    -> Element.Element Msg
deletionAction context project serverIds =
    VH.deleteBulkResourcePopconfirm
        context
        project
        (SharedMsg << SharedMsg.TogglePopover)
        { count = Set.size serverIds, word = context.localization.virtualComputer }
        "serverListDeletePopconfirm"
        (Just <|
            SharedMsg
                (SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project)
                    (SharedMsg.RequestDeleteServers (Set.toList serverIds))
                )
        )
        (Just NoOp)


searchByNameUuidFilter : View.Types.Context -> DataList.SearchFilter { record | name : String }
searchByNameUuidFilter context =
    { label = "Search:"
    , placeholder =
        Just <|
            "Enter "
                ++ context.localization.virtualComputer
                ++ " name or UUID"
    , textToSearch = \record -> record.name ++ " " ++ record.id
    }


filters :
    View.Types.Context
    -> Project
    -> String
    -> Time.Posix
    ->
        List
            (DataList.Filter
                { record
                    | creator : String
                    , creationTime : Time.Posix
                    , securityGroupIds : List OSTypes.SecurityGroupUuid
                    , status : ServerUiStatus
                    , size : String
                }
            )
filters context project currentUser currentTime =
    let
        creatorFilterOptionValues servers =
            List.map .creator servers
                |> Set.fromList
                |> Set.toList

        statusFilterOptionValues servers =
            List.map .status servers
                |> List.map VH.getServerUiStatusStr
                |> Set.fromList
                |> Set.toList

        sizeFilterOptionValues servers =
            List.map .size servers
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
            DataList.MultiselectOption <| Set.singleton currentUser
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
    , { id = "status"
      , label = "Status of"
      , chipPrefix = "Status of "
      , filterOptions =
            \servers ->
                statusFilterOptionValues servers
                    |> List.map (\s -> ( s, s ))
                    |> Dict.fromList
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption <| Set.empty
      , onFilter =
            \optionValue server ->
                (server.status |> VH.getServerUiStatusStr) == optionValue
      }
    , { id = "size"
      , label = (context.localization.virtualComputerHardwareConfig |> Helpers.String.toTitleCase) ++ " of"
      , chipPrefix = (context.localization.virtualComputerHardwareConfig |> Helpers.String.toTitleCase) ++ " of "
      , filterOptions =
            \servers ->
                sizeFilterOptionValues servers
                    |> List.map (\s -> ( s, s ))
                    -- TODO: Sort sizes by flavor & not alphabetically.
                    |> Dict.fromList
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption <| Set.empty
      , onFilter =
            \optionValue server ->
                server.size == optionValue
      }
    ]
        ++ (if context.experimentalFeaturesEnabled then
                let
                    securityGroupFilterOptionValues =
                        project.securityGroups
                            |> RDPP.withDefault []
                            |> List.map (\sg -> ( sg.uuid, sg.name ))
                in
                [ { id = "securityGroup"
                  , label = context.localization.securityGroup |> Helpers.String.toTitleCase
                  , chipPrefix = "Member of "
                  , filterOptions =
                        \_ ->
                            securityGroupFilterOptionValues
                                |> Dict.fromList
                  , filterTypeAndDefaultValue =
                        DataList.MultiselectOption <| Set.empty
                  , onFilter =
                        \optionValue server ->
                            server.securityGroupIds
                                |> Set.fromList
                                |> Set.member optionValue
                  }
                ]

            else
                []
           )
