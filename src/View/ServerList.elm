module View.ServerList exposing (serverList)

import Element
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import Set
import Style.Theme
import Style.Widgets.Button
import Style.Widgets.Icon as Icon
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , IPInfoLevel(..)
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
        , Style
        , ViewState(..)
        )
import View.Helpers as VH exposing (edges)
import Widget
import Widget.Style.Material


serverList : Style -> Project -> ServerListViewParams -> Element.Element Msg
serverList style project serverListViewParams =
    {- Resolve whether we have a loaded list of servers to display; if so, call rendering function serverList_ -}
    case ( project.servers.data, project.servers.refreshStatus ) of
        ( RDPP.DontHave, RDPP.NotLoading Nothing ) ->
            Element.row [ Element.spacing 15 ]
                [ Widget.circularProgressIndicator
                    (Style.Theme.materialStyle style.palette).progressIndicator
                    Nothing
                , Element.text "Please wait..."
                ]

        ( RDPP.DontHave, RDPP.NotLoading (Just _) ) ->
            Element.paragraph [] [ Element.text ("Cannot display servers. Error message: " ++ Debug.toString e) ]

        ( RDPP.DontHave, RDPP.Loading _ ) ->
            Element.row [ Element.spacing 15 ]
                [ Widget.circularProgressIndicator
                    (Style.Theme.materialStyle style.palette).progressIndicator
                    Nothing
                , Element.text "Loading..."
                ]

        ( RDPP.DoHave servers _, _ ) ->
            if List.isEmpty servers then
                Element.paragraph [] [ Element.text "You don't have any servers yet, go create one!" ]

            else
                serverList_
                    style
                    project.auth.project.uuid
                    project.auth.user.uuid
                    serverListViewParams
                    servers


serverList_ : Style -> ProjectIdentifier -> OSTypes.UserUuid -> ServerListViewParams -> List Server -> Element.Element Msg
serverList_ style projectId userUuid serverListViewParams servers =
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
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading2 (Element.text "Servers")
        , Element.column (VH.exoColumnAttributes ++ [ Element.width (Element.fill |> Element.maximum 960) ]) <|
            List.concat
                [ [ renderTableHead
                        style
                        projectId
                        allServersSelected
                        ( selectableServers, selectedServers )
                        serverListViewParams
                  ]
                , List.map (renderServer style projectId serverListViewParams True) ownServers
                , [ onlyOwnExpander style projectId serverListViewParams otherUsersServers ]
                , if serverListViewParams.onlyOwnServers then
                    []

                  else
                    List.map (renderServer style projectId serverListViewParams False) otherUsersServers
                ]
        ]


renderTableHead : Style -> ProjectIdentifier -> Bool -> ( List Server, List Server ) -> ServerListViewParams -> Element.Element Msg
renderTableHead style projectId allServersSelected ( selectableServers, selectedServers ) serverListViewParams =
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
            ListProjectServers newParams
                |> SetProjectView
                |> ProjectMsg projectId

        extraColAttrs =
            [ Element.width Element.fill
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Border.color (Element.rgb255 10 10 10)
            , Element.paddingXY 5 0
            ]

        extraRowAttrs =
            [ Element.padding 5
            , Element.width Element.fill
            ]
    in
    Element.column (VH.exoColumnAttributes ++ extraColAttrs)
        [ Element.row (VH.exoRowAttributes ++ extraRowAttrs) <|
            [ Element.el [] <|
                Input.checkbox []
                    { checked = allServersSelected
                    , onChange = onChecked
                    , icon = Input.defaultCheckbox
                    , label = Input.labelRight [] (Element.text "Select All")
                    }
            , Element.el [ Element.alignRight ] <|
                Widget.textButton
                    (Style.Widgets.Button.dangerButton (Style.Theme.toMaterialPalette style.palette))
                    { text = "Delete"
                    , onPress = deleteButtonOnPress
                    }
            ]
        ]


renderServer : Style -> ProjectIdentifier -> ServerListViewParams -> Bool -> Server -> Element.Element Msg
renderServer style projectId serverListViewParams isMyServer server =
    let
        statusIcon =
            Element.el [ Element.paddingEach { edges | right = 15 } ] (Icon.roundRect (server |> VH.getServerUiStatus |> VH.getServerUiStatusColor) 16)

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
                                ProjectMsg projectId <| SetProjectView <| ListProjectServers newParams
                        , icon = Input.defaultCheckbox
                        , label = Input.labelHidden server.osProps.name
                        }

                OSTypes.ServerLocked ->
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = False
                        , onChange = \_ -> NoOp
                        , icon = \_ -> Icon.lock (Element.rgb255 10 10 10) 14
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
                , Element.el [ Font.size 15 ] (Element.text (server |> VH.getServerUiStatus |> VH.getServerUiStatusStr))
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
                        (Style.Widgets.Button.dangerButton (Style.Theme.toMaterialPalette style.palette))
                        { icon = Icon.remove (Element.rgb255 255 255 255) 16
                        , text = "Delete"
                        , onPress =
                            Just
                                (ProjectMsg projectId (RequestDeleteServer server.osProps.uuid))
                        }
                    , Widget.iconButton
                        (Widget.Style.Material.outlinedButton (Style.Theme.toMaterialPalette style.palette))
                        { icon = Icon.windowClose (Element.rgb255 0 0 0) 16
                        , text = "Cancel"
                        , onPress =
                            Just
                                (ProjectMsg
                                    projectId
                                    (SetProjectView <|
                                        ListProjectServers
                                            { serverListViewParams
                                                | deleteConfirmations =
                                                    serverListViewParams.deleteConfirmations
                                                        |> List.filter ((/=) server.osProps.uuid)
                                            }
                                    )
                                )
                        }
                    ]

                ( False, OSTypes.ServerUnlocked, False ) ->
                    [ Widget.iconButton
                        (Style.Widgets.Button.dangerButton (Style.Theme.toMaterialPalette style.palette))
                        { icon = Icon.remove (Element.rgb255 255 255 255) 16
                        , text = "Delete"
                        , onPress =
                            Just
                                (ProjectMsg projectId
                                    (SetProjectView <|
                                        ListProjectServers
                                            { serverListViewParams | deleteConfirmations = [ server.osProps.uuid ] }
                                    )
                                )
                        }
                    ]

                ( False, OSTypes.ServerLocked, _ ) ->
                    [ Widget.iconButton
                        (Style.Widgets.Button.dangerButton (Style.Theme.toMaterialPalette style.palette))
                        { icon = Icon.remove (Element.rgb255 255 255 255) 16
                        , text = "Delete"
                        , onPress = Nothing
                        }
                    ]
    in
    Element.row (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        ([ checkbox
         , serverLabel server
         ]
            ++ deleteWidget
        )


onlyOwnExpander : Style -> ProjectIdentifier -> ServerListViewParams -> List Server -> Element.Element Msg
onlyOwnExpander style projectId serverListViewParams otherUsersServers =
    let
        numOtherUsersServers =
            List.length otherUsersServers

        statusText =
            let
                ( serversPluralization, usersPluralization ) =
                    if numOtherUsersServers == 1 then
                        ( "server", "another user" )

                    else
                        ( "servers", "other users" )
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
                "Servers created by other users"

        ( ( changeActionVerb, changeActionIcon ), newServerListViewParams ) =
            if serverListViewParams.onlyOwnServers then
                ( ( "Show", Icon.downArrow )
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
                ( ( "Hide", Icon.upArrow )
                , { serverListViewParams
                    | onlyOwnServers = True
                    , selectedServers = newSelectedServers
                  }
                )

        changeOnlyOwnMsg : Msg
        changeOnlyOwnMsg =
            ProjectMsg projectId <|
                SetProjectView <|
                    ListProjectServers
                        newServerListViewParams

        changeButton =
            Widget.button
                (Widget.Style.Material.textButton (Style.Theme.toMaterialPalette style.palette))
                { onPress = Just changeOnlyOwnMsg
                , icon =
                    changeActionIcon (Element.rgb255 0 108 163) 16
                , text = changeActionVerb
                }
    in
    if numOtherUsersServers == 0 then
        Element.none

    else
        Element.column (VH.exoColumnAttributes ++ [ Element.padding 0, Element.width Element.fill ])
            [ Element.el
                [ Element.width Element.fill
                , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                , Border.color (Element.rgb255 10 10 10)
                ]
                Element.none
            , Element.el
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
