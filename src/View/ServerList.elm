module View.ServerList exposing (serverList)

import Element
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Set
import Style.Helpers as SH
import Style.Widgets.Button
import Style.Widgets.Card
import Style.Widgets.Icon as Icon
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( IPInfoLevel(..)
        , Msg(..)
        , NonProjectViewConstructor(..)
        , PasswordVisibility(..)
        , Project
        , ProjectIdentifier
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerListViewParams
        , ServerOrigin(..)
        , ServerSelection
        , ServerSpecificMsgConstructor(..)
        , ViewState(..)
        )
import View.Helpers as VH exposing (edges)
import View.QuotaUsage
import View.Types
import Widget
import Widget.Style.Material


serverList :
    View.Types.Context
    -> Bool
    -> Project
    -> ServerListViewParams
    -> (ServerListViewParams -> Msg)
    -> Element.Element Msg
serverList context showHeading project serverListViewParams toMsg =
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

                ( RDPP.DontHave, RDPP.Loading _ ) ->
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
                        serverList_
                            context
                            project.auth.project.uuid
                            project.auth.user.uuid
                            serverListViewParams
                            toMsg
                            servers
    in
    Element.column [ Element.width Element.fill ]
        [ if showHeading then
            Element.el (VH.heading2 context.palette)
                (Element.text <|
                    (context.localization.virtualComputer
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )
                )

          else
            Element.none
        , View.QuotaUsage.computeQuotaDetails context project.computeQuota
        , serverListContents
        ]


serverList_ :
    View.Types.Context
    -> ProjectIdentifier
    -> OSTypes.UserUuid
    -> ServerListViewParams
    -> (ServerListViewParams -> Msg)
    -> List Server
    -> Element.Element Msg
serverList_ context projectId userUuid serverListViewParams toMsg servers =
    {- Render a list of servers -}
    let
        ( ownServers, otherUsersServers ) =
            List.partition (ownServer userUuid) servers

        shownServers =
            if serverListViewParams.onlyOwnServers then
                ownServers

            else
                servers

        selectableServers =
            shownServers
                |> List.filter (\s -> s.osProps.details.lockStatus == OSTypes.ServerUnlocked)

        selectedServers =
            List.filter (serverIsSelected serverListViewParams.selectedServers) shownServers

        allServersSelected =
            if List.isEmpty selectableServers then
                False

            else
                selectableServers == selectedServers
    in
    Element.column [ Element.paddingXY 10 0, Element.spacing 10, Element.width Element.fill ] <|
        List.concat
            [ [ renderTableHead
                    context
                    projectId
                    allServersSelected
                    ( selectableServers, selectedServers )
                    serverListViewParams
                    toMsg
              ]
            , List.map (renderServer context projectId serverListViewParams toMsg True) ownServers
            , [ onlyOwnExpander context serverListViewParams toMsg otherUsersServers ]
            , if serverListViewParams.onlyOwnServers then
                []

              else
                List.map (renderServer context projectId serverListViewParams toMsg False) otherUsersServers
            ]


renderTableHead :
    View.Types.Context
    -> ProjectIdentifier
    -> Bool
    -> ( List Server, List Server )
    -> ServerListViewParams
    -> (ServerListViewParams -> Msg)
    -> Element.Element Msg
renderTableHead context projectId allServersSelected ( selectableServers, selectedServers ) serverListViewParams toMsg =
    let
        deleteButtonOnPress =
            if List.isEmpty selectedServers then
                Nothing

            else
                let
                    uuidsToDelete =
                        List.map (\s -> s.osProps.uuid) selectedServers
                in
                Just (ProjectMsg projectId (RequestDeleteServers uuidsToDelete))

        onChecked new =
            let
                newSelection =
                    if new {- == true -} then
                        selectableServers
                            |> List.map (\s -> s.osProps.uuid)
                            |> Set.fromList

                    else
                        Set.empty

                newParams =
                    { serverListViewParams | selectedServers = newSelection }
            in
            toMsg newParams

        extraRowAttrs =
            [ Element.paddingXY 5 0
            , Element.width Element.fill
            ]
    in
    Element.row (VH.exoRowAttributes ++ extraRowAttrs) <|
        [ Element.el [] <|
            Input.checkbox []
                { checked = allServersSelected
                , onChange = onChecked
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [] (Element.text "Select All")
                }
        , Element.el [ Element.alignRight ] <|
            Widget.textButton
                (Style.Widgets.Button.dangerButton context.palette)
                { text = "Delete"
                , onPress = deleteButtonOnPress
                }
        ]


renderServer :
    View.Types.Context
    -> ProjectIdentifier
    -> ServerListViewParams
    -> (ServerListViewParams -> Msg)
    -> Bool
    -> Server
    -> Element.Element Msg
renderServer context projectId serverListViewParams toMsg isMyServer server =
    let
        statusIcon =
            Element.el
                [ Element.paddingEach { edges | right = 15 } ]
                (Icon.roundRect (server |> VH.getServerUiStatus |> VH.getServerUiStatusColor context.palette) 16)

        creatorNameView =
            case ( isMyServer, server.exoProps.serverOrigin ) of
                ( False, ServerFromExo exoOriginProps ) ->
                    case exoOriginProps.exoCreatorUsername of
                        Just creatorUsername ->
                            Element.el
                                [ Element.width Element.shrink
                                , Font.size 12
                                ]
                                (Element.text ("(creator: " ++ creatorUsername ++ ")"))

                        _ ->
                            Element.none

                ( _, _ ) ->
                    Element.none

        checkbox =
            case server.osProps.details.lockStatus of
                OSTypes.ServerUnlocked ->
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = serverIsSelected serverListViewParams.selectedServers server
                        , onChange =
                            \new ->
                                let
                                    action =
                                        if new {- == true -} then
                                            AddServer

                                        else
                                            RemoveServer

                                    newParams =
                                        modifyServerSelection server action serverListViewParams
                                in
                                toMsg newParams
                        , icon = Input.defaultCheckbox
                        , label = Input.labelHidden server.osProps.name
                        }

                OSTypes.ServerLocked ->
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = False
                        , onChange = \_ -> NoOp
                        , icon = \_ -> Icon.lock (SH.toElementColor context.palette.on.surface) 14
                        , label = Input.labelHidden server.osProps.name
                        }

        serverLabelName : Server -> Element.Element Msg
        serverLabelName aServer =
            Element.row [ Element.width Element.fill ]
                [ statusIcon
                , Element.el [ Font.bold ] (Element.text aServer.osProps.name)
                ]

        serverNameClickEvent : Msg
        serverNameClickEvent =
            ProjectMsg projectId <|
                SetProjectView <|
                    ServerDetail
                        server.osProps.uuid
                        Defaults.serverDetailViewParams

        serverLabel : Server -> Element.Element Msg
        serverLabel aServer =
            Element.row
                [ Element.width Element.fill
                , Events.onClick serverNameClickEvent
                , Element.pointer
                , Element.spacing 10
                ]
                [ serverLabelName aServer
                , creatorNameView
                ]

        deletionAttempted =
            server.exoProps.deletionAttempted

        confirmationNeeded =
            List.member server.osProps.uuid serverListViewParams.deleteConfirmations

        deleteWidget =
            case ( deletionAttempted, server.osProps.details.lockStatus, confirmationNeeded ) of
                ( True, _, _ ) ->
                    [ Element.text "Deleting..." ]

                ( False, OSTypes.ServerUnlocked, True ) ->
                    [ Element.text "Confirm delete?"
                    , Widget.iconButton
                        (Style.Widgets.Button.dangerButton context.palette)
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress =
                            Just <|
                                ProjectMsg projectId <|
                                    ServerMsg server.osProps.uuid <|
                                        RequestDeleteServer False
                        }
                    , Widget.iconButton
                        (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                        { icon = Icon.windowClose (SH.toElementColor context.palette.on.surface) 16
                        , text = "Cancel"
                        , onPress =
                            Just <|
                                toMsg
                                    { serverListViewParams
                                        | deleteConfirmations =
                                            serverListViewParams.deleteConfirmations
                                                |> List.filter ((/=) server.osProps.uuid)
                                    }
                        }
                    ]

                ( False, OSTypes.ServerUnlocked, False ) ->
                    [ Widget.iconButton
                        (Style.Widgets.Button.dangerButton context.palette)
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress =
                            Just <|
                                toMsg
                                    { serverListViewParams | deleteConfirmations = [ server.osProps.uuid ] }
                        }
                    ]

                ( False, OSTypes.ServerLocked, _ ) ->
                    [ Widget.iconButton
                        (Style.Widgets.Button.dangerButton context.palette)
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress = Nothing
                        }
                    ]
    in
    Style.Widgets.Card.exoCard
        context.palette
        (Element.row [ Element.spacing 8 ] [ checkbox, serverLabel server ])
        (Element.row [ Element.spacing 8 ] deleteWidget)
        Element.none



-- TODO factor this out with ipsAssignedToServersExpander in ServerList.elm


onlyOwnExpander :
    View.Types.Context
    -> ServerListViewParams
    -> (ServerListViewParams -> Msg)
    -> List Server
    -> Element.Element Msg
onlyOwnExpander context serverListViewParams toMsg otherUsersServers =
    let
        numOtherUsersServers =
            List.length otherUsersServers

        statusText =
            let
                ( serversPluralization, usersPluralization ) =
                    if numOtherUsersServers == 1 then
                        ( context.localization.virtualComputer
                        , "another user"
                        )

                    else
                        ( Helpers.String.pluralize context.localization.virtualComputer
                        , "other users"
                        )
            in
            if serverListViewParams.onlyOwnServers then
                String.concat
                    [ "Hiding "
                    , String.fromInt numOtherUsersServers
                    , " "
                    , serversPluralization
                    , " created by "
                    , usersPluralization
                    ]

            else
                String.join " "
                    [ context.localization.virtualComputer
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "created by other users"
                    ]

        ( ( changeActionVerb, changeActionIcon ), newServerListViewParams ) =
            if serverListViewParams.onlyOwnServers then
                ( ( "Show", FeatherIcons.chevronDown )
                , { serverListViewParams
                    | onlyOwnServers = False
                  }
                )

            else
                -- When hiding other users' servers, ensure that they are de-selected!
                let
                    serverUuidsToDeselect =
                        List.map (\s -> s.osProps.uuid) otherUsersServers

                    newSelectedServers =
                        Set.filter
                            (\u -> not <| List.member u serverUuidsToDeselect)
                            serverListViewParams.selectedServers
                in
                ( ( "Hide", FeatherIcons.chevronUp )
                , { serverListViewParams
                    | onlyOwnServers = True
                    , selectedServers = newSelectedServers
                  }
                )

        changeOnlyOwnMsg : Msg
        changeOnlyOwnMsg =
            toMsg newServerListViewParams

        changeButton =
            Widget.button
                (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                { onPress = Just changeOnlyOwnMsg
                , icon =
                    changeActionIcon
                        |> FeatherIcons.toHtml []
                        |> Element.html
                        |> Element.el []
                , text = changeActionVerb
                }
    in
    if numOtherUsersServers == 0 then
        Element.none

    else
        Element.column [ Element.spacing 3, Element.padding 0, Element.width Element.fill ]
            [ Element.el
                [ Element.centerX, Font.size 14 ]
                (Element.text statusText)
            , Element.el
                [ Element.centerX ]
                changeButton
            ]


ownServer : OSTypes.UserUuid -> Server -> Bool
ownServer userUuid server =
    server.osProps.details.userUuid == userUuid


serverIsSelected : Set.Set ServerSelection -> Server -> Bool
serverIsSelected selectedUuids server =
    Set.member server.osProps.uuid selectedUuids


modifyServerSelection : Server -> ModifyServerSelectionAction -> ServerListViewParams -> ServerListViewParams
modifyServerSelection server action serverListViewParams =
    let
        actionFunc =
            case action of
                AddServer ->
                    Set.insert

                RemoveServer ->
                    Set.remove

        newSelectedServers =
            actionFunc server.osProps.uuid serverListViewParams.selectedServers
    in
    { serverListViewParams | selectedServers = newSelectedServers }


type ModifyServerSelectionAction
    = AddServer
    | RemoveServer
