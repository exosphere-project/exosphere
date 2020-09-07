module View.ServerList exposing (serverList)

import Element
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import Style.Theme
import Style.Widgets.Button
import Style.Widgets.Icon as Icon
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
        , ViewState(..)
        )
import View.Helpers as VH exposing (edges)
import Widget
import Widget.Style.Material


serverList : Project -> ServerListViewParams -> Element.Element Msg
serverList project serverListViewParams =
    {- Resolve whether we have a loaded list of servers to display; if so, call rendering function serverList_ -}
    case ( project.servers.data, project.servers.refreshStatus ) of
        ( RDPP.DontHave, RDPP.NotLoading Nothing ) ->
            Element.paragraph [] [ Element.text "Please wait..." ]

        ( RDPP.DontHave, RDPP.NotLoading (Just _) ) ->
            Element.paragraph [] [ Element.text ("Cannot display servers. Error message: " ++ Debug.toString e) ]

        ( RDPP.DontHave, RDPP.Loading _ ) ->
            Element.paragraph [] [ Element.text "Loading..." ]

        ( RDPP.DoHave servers _, _ ) ->
            if List.isEmpty servers then
                Element.paragraph [] [ Element.text "You don't have any servers yet, go create one!" ]

            else
                serverList_
                    (Helpers.getProjectId project)
                    project.auth.user.uuid
                    serverListViewParams
                    servers


serverList_ : ProjectIdentifier -> OSTypes.UserUuid -> ServerListViewParams -> List Server -> Element.Element Msg
serverList_ projectId userUuid serverListViewParams servers =
    {- Render a list of servers -}
    let
        ( ownServers, otherUsersServers ) =
            List.partition (ownServer userUuid) servers

        shownServers =
            if serverListViewParams.onlyOwnServers then
                ownServers

            else
                servers

        noServersSelected =
            List.any (\s -> s.exoProps.selected) shownServers |> not

        allServersSelected =
            if List.isEmpty shownServers then
                False

            else
                shownServers
                    |> List.filter (\s -> s.osProps.details.lockStatus == OSTypes.ServerUnlocked)
                    |> List.all (\s -> s.exoProps.selected)

        selectedServers =
            List.filter (\s -> s.exoProps.selected) shownServers

        deleteButtonOnPress =
            if noServersSelected == True then
                Nothing

            else
                let
                    uuidsToDelete =
                        List.map (\s -> s.osProps.uuid) selectedServers
                in
                Just (ProjectMsg projectId (RequestDeleteServers uuidsToDelete))
    in
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading2 (Element.text "My Servers")
        , Element.column (VH.exoColumnAttributes ++ [ Element.padding 5, Border.width 1 ])
            [ Element.text "Bulk Actions"
            , Input.checkbox []
                { checked = allServersSelected
                , onChange = \new -> ProjectMsg projectId (SelectAllServers new)
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [] (Element.text "Select All")
                }
            , Widget.textButton
                (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
                { text = "Delete"
                , onPress = deleteButtonOnPress
                }
            ]
        , Element.column (VH.exoColumnAttributes ++ [ Element.width (Element.fill |> Element.maximum 960) ]) <|
            List.concat
                [ List.map (renderServer projectId serverListViewParams) ownServers
                , [ onlyOwnExpander projectId serverListViewParams (List.length otherUsersServers) ]
                , if serverListViewParams.onlyOwnServers then
                    []

                  else
                    List.map (renderServer projectId serverListViewParams) otherUsersServers
                ]
        ]


renderServer : ProjectIdentifier -> ServerListViewParams -> Server -> Element.Element Msg
renderServer projectId serverListViewParams server =
    let
        statusIcon =
            Element.el [ Element.paddingEach { edges | right = 15 } ] (Icon.roundRect (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusColor) 16)

        checkbox =
            case server.osProps.details.lockStatus of
                OSTypes.ServerUnlocked ->
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = server.exoProps.selected
                        , onChange = \new -> ProjectMsg projectId (SelectServer server new)
                        , icon = Input.defaultCheckbox
                        , label = Input.labelHidden server.osProps.name
                        }

                OSTypes.ServerLocked ->
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = server.exoProps.selected
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
                        { verboseStatus = False
                        , passwordVisibility = PasswordHidden
                        , ipInfoLevel = IPSummary
                        , serverActionNamePendingConfirmation = Nothing
                        }

        serverLabel : Server -> Element.Element Msg
        serverLabel aServer =
            Element.row
                [ Element.width Element.fill
                , Events.onClick serverNameClickEvent
                , Element.pointer
                ]
                [ serverLabelName aServer
                , Element.el [ Font.size 15 ] (Element.text (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusStr))
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
                        (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
                        { icon = Icon.remove (Element.rgb255 255 255 255) 16
                        , text = "Delete"
                        , onPress =
                            Just
                                (ProjectMsg projectId (RequestDeleteServer server.osProps.uuid))
                        }
                    , Widget.iconButton
                        (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
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
                        (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
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
                        (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
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


onlyOwnExpander : ProjectIdentifier -> ServerListViewParams -> Int -> Element.Element Msg
onlyOwnExpander projectId serverListViewParams numOtherUsersServers =
    let
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

        ( changeActionVerb, changeActionIcon ) =
            if serverListViewParams.onlyOwnServers then
                ( "Show", Icon.downArrow )

            else
                ( "Hide", Icon.upArrow )

        changeOnlyOwnMsg : Msg
        changeOnlyOwnMsg =
            ProjectMsg projectId <|
                SetProjectView <|
                    ListProjectServers
                        { serverListViewParams
                            | onlyOwnServers =
                                not serverListViewParams.onlyOwnServers
                        }

        changeButton =
            Widget.button
                (Widget.Style.Material.textButton Style.Theme.exoPalette)
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
