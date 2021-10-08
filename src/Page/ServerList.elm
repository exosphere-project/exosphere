module Page.ServerList exposing (Model, Msg, init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.Formatting exposing (humanCount)
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Widgets.Card
import Style.Widgets.Icon as Icon
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.Project exposing (Project)
import Types.Server exposing (Server, ServerOrigin(..))
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , onlyOwnServers : Bool
    , selectedServers : Set.Set ServerSelection
    , deleteConfirmations : Set.Set DeleteConfirmation
    }


type alias ServerSelection =
    OSTypes.ServerUuid


type alias DeleteConfirmation =
    OSTypes.ServerUuid


type Msg
    = GotShowOnlyOwnServers Bool (Set.Set ServerSelection)
    | GotSelectServer ServerSelection Bool
    | GotNewServerSelection (Set.Set ServerSelection)
    | GotDeleteNeedsConfirm DeleteConfirmation
    | GotDeleteConfirm DeleteConfirmation
    | GotDeleteCancel DeleteConfirmation
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Model
init showHeading =
    Model showHeading True Set.empty Set.empty


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotShowOnlyOwnServers showOnlyOwn newSelection ->
            ( { model
                | onlyOwnServers = showOnlyOwn
                , selectedServers = newSelection
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotSelectServer serverId selected ->
            ( { model
                | selectedServers =
                    if selected then
                        Set.insert serverId model.selectedServers

                    else
                        Set.remove serverId model.selectedServers
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotNewServerSelection newSelection ->
            ( { model | selectedServers = newSelection }, Cmd.none, SharedMsg.NoOp )

        GotDeleteNeedsConfirm serverId ->
            ( { model
                | deleteConfirmations =
                    Set.insert serverId model.deleteConfirmations
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotDeleteConfirm serverId ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid <|
                SharedMsg.ServerMsg serverId <|
                    SharedMsg.RequestDeleteServer False
            )

        GotDeleteCancel serverId ->
            ( { model
                | deleteConfirmations =
                    Set.remove serverId model.deleteConfirmations
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
                        serverList_
                            context
                            project.auth.project.uuid
                            project.auth.user.uuid
                            model
                            servers
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
            [ Page.QuotaUsage.view context (Page.QuotaUsage.Compute project.computeQuota)
            , serverListContents
            ]
        ]


serverList_ :
    View.Types.Context
    -> ProjectIdentifier
    -> OSTypes.UserUuid
    -> Model
    -> List Server
    -> Element.Element Msg
serverList_ context projectId userUuid model servers =
    {- Render a list of servers -}
    let
        ( ownServers, otherUsersServers ) =
            List.partition (ownServer userUuid) servers

        shownServers =
            if model.onlyOwnServers then
                ownServers

            else
                servers

        selectableServers =
            shownServers
                |> List.filter (\s -> s.osProps.details.lockStatus == OSTypes.ServerUnlocked)

        selectedServers =
            List.filter (serverIsSelected model.selectedServers) shownServers

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
              ]
            , List.map (renderServer context projectId model True) ownServers
            , [ onlyOwnExpander context model otherUsersServers ]
            , if model.onlyOwnServers then
                []

              else
                List.map (renderServer context projectId model False) otherUsersServers
            ]


renderTableHead :
    View.Types.Context
    -> ProjectIdentifier
    -> Bool
    -> ( List Server, List Server )
    -> Element.Element Msg
renderTableHead context projectId allServersSelected ( selectableServers, selectedServers ) =
    let
        deleteButtonOnPress =
            if List.isEmpty selectedServers then
                Nothing

            else
                let
                    uuidsToDelete =
                        List.map (\s -> s.osProps.uuid) selectedServers
                in
                Just <| SharedMsg (SharedMsg.ProjectMsg projectId (SharedMsg.RequestDeleteServers uuidsToDelete))

        onChecked new =
            let
                newSelection =
                    if new {- == true -} then
                        selectableServers
                            |> List.map (\s -> s.osProps.uuid)
                            |> Set.fromList

                    else
                        Set.empty
            in
            GotNewServerSelection newSelection

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
            Widget.iconButton
                (SH.materialStyle context.palette).dangerButton
                { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                , text = "Delete"
                , onPress = deleteButtonOnPress
                }
        ]


renderServer :
    View.Types.Context
    -> ProjectIdentifier
    -> Model
    -> Bool
    -> Server
    -> Element.Element Msg
renderServer context projectId model isMyServer server =
    let
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
                        { checked = serverIsSelected model.selectedServers server
                        , onChange =
                            GotSelectServer server.osProps.uuid
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
            Element.row
                [ Element.width Element.fill
                , Element.spacing 10

                -- Overriding Font.bold in ancestor element
                , Font.regular
                ]
                [ VH.serverStatusBadge context.palette aServer
                , Element.el [ Font.bold ] (Element.text aServer.osProps.name)
                ]

        serverLabel : Server -> Element.Element Msg
        serverLabel aServer =
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute projectId <| Route.ServerDetail server.osProps.uuid)
                , label =
                    Element.row
                        [ Element.width Element.fill
                        , Element.pointer
                        , Element.spacing 10
                        ]
                        [ serverLabelName aServer
                        , creatorNameView
                        ]
                }

        deletionAttempted =
            server.exoProps.deletionAttempted

        confirmationNeeded =
            Set.member server.osProps.uuid model.deleteConfirmations

        deleteWidget =
            case ( deletionAttempted, server.osProps.details.lockStatus, confirmationNeeded ) of
                ( True, _, _ ) ->
                    [ Element.text "Deleting..." ]

                ( False, OSTypes.ServerUnlocked, True ) ->
                    [ Element.text "Confirm delete?"
                    , Widget.iconButton
                        (SH.materialStyle context.palette).dangerButton
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress =
                            Just <| GotDeleteConfirm server.osProps.uuid
                        }
                    , Widget.iconButton
                        (SH.materialStyle context.palette).button
                        { icon = Icon.windowClose (SH.toElementColor context.palette.on.surface) 16
                        , text = "Cancel"
                        , onPress =
                            Just <| GotDeleteCancel server.osProps.uuid
                        }
                    ]

                ( False, OSTypes.ServerUnlocked, False ) ->
                    [ Widget.iconButton
                        (SH.materialStyle context.palette).dangerButton
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress =
                            Just <| GotDeleteNeedsConfirm server.osProps.uuid
                        }
                    ]

                ( False, OSTypes.ServerLocked, _ ) ->
                    [ Widget.iconButton
                        (SH.materialStyle context.palette).dangerButton
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress = Nothing
                        }
                    ]
    in
    Style.Widgets.Card.exoCardWithTitleAndSubtitle
        context.palette
        (Element.row [ Element.spacing 8 ] [ checkbox, serverLabel server ])
        (Element.row [ Element.spacing 8 ] deleteWidget)
        Element.none



-- TODO factor this out with ipsAssignedToServersExpander in FloatingIpList.elm


onlyOwnExpander :
    View.Types.Context
    -> Model
    -> List Server
    -> Element.Element Msg
onlyOwnExpander context model otherUsersServers =
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
            if model.onlyOwnServers then
                String.concat
                    [ "Hiding "
                    , humanCount context.locale numOtherUsersServers
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

        ( ( changeActionVerb, changeActionIcon ), ( newOnlyOwnServers, newSelectedServers ) ) =
            if model.onlyOwnServers then
                ( ( "Show", FeatherIcons.chevronDown )
                , ( False, model.selectedServers )
                )

            else
                -- When hiding other users' servers, ensure that they are de-selected!
                let
                    serverUuidsToDeselect =
                        List.map (\s -> s.osProps.uuid) otherUsersServers

                    selectedServers =
                        Set.filter
                            (\u -> not <| List.member u serverUuidsToDeselect)
                            model.selectedServers
                in
                ( ( "Hide", FeatherIcons.chevronUp )
                , ( True, selectedServers )
                )

        changeOnlyOwnMsg : Msg
        changeOnlyOwnMsg =
            GotShowOnlyOwnServers newOnlyOwnServers newSelectedServers

        changeButton =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { onPress = Just changeOnlyOwnMsg
                , icon =
                    Element.row [ Element.spacing 5 ]
                        [ Element.text changeActionVerb
                        , changeActionIcon
                            |> FeatherIcons.withSize 18
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                        ]
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
