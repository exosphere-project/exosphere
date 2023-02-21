module Page.FloatingIpAssign exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Route
import Style.Helpers as SH
import Style.Widgets.Button as Button
import Style.Widgets.Select
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { ipUuid : Maybe OSTypes.IpAddressUuid
    , serverUuid : Maybe OSTypes.ServerUuid
    }


type Msg
    = GotIpUuid (Maybe OSTypes.IpAddressUuid)
    | GotServerUuid (Maybe OSTypes.ServerUuid)
      -- Maybe this should be a PortUuid, eh
    | SharedMsg SharedMsg.SharedMsg


init : Maybe OSTypes.IpAddressUuid -> Maybe OSTypes.ServerUuid -> Model
init =
    Model


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg { viewContext } project model =
    let
        withReplaceUrl ( model_, cmd, sharedMsg ) =
            Route.withReplaceUrl
                viewContext
                (Route.ProjectRoute
                    (GetterSetters.projectIdentifier project)
                    (Route.FloatingIpAssign model_.ipUuid model_.serverUuid)
                )
                ( model, cmd, sharedMsg )
    in
    case msg of
        GotIpUuid maybeIpUuid ->
            ( { model | ipUuid = maybeIpUuid }, Cmd.none, SharedMsg.NoOp )
                |> withReplaceUrl

        GotServerUuid maybeServerUuid ->
            ( { model | serverUuid = maybeServerUuid }, Cmd.none, SharedMsg.NoOp )
                |> withReplaceUrl

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        serverChoices =
            project.servers
                |> RDPP.withDefault []
                |> List.filter
                    (\s ->
                        not <|
                            List.member s.osProps.details.openstackStatus
                                [ OSTypes.ServerSoftDeleted
                                , OSTypes.ServerError
                                , OSTypes.ServerBuild
                                , OSTypes.ServerDeleted
                                ]
                    )
                |> List.filter
                    (\s ->
                        GetterSetters.getServerFloatingIps project s.osProps.uuid |> List.isEmpty
                    )
                |> List.map
                    (\s ->
                        ( s.osProps.uuid
                        , VH.extendedResourceName (Just s.osProps.name) s.osProps.uuid context.localization.virtualComputer
                        )
                    )

        ipChoices =
            -- Show only un-assigned IP addresses
            project.floatingIps
                |> RDPP.withDefault []
                |> List.sortBy .address
                |> List.filter (\ip -> ip.status == OSTypes.IpAddressDown)
                |> List.filter (\ip -> GetterSetters.getFloatingIpServer project ip == Nothing)
                |> List.map
                    (\ip ->
                        ( ip.uuid
                        , ip.address
                        )
                    )

        selectIpText =
            String.join " "
                [ "Select"
                , Helpers.String.indefiniteArticle context.localization.floatingIpAddress
                , context.localization.floatingIpAddress
                ]

        selectServerText =
            String.join " "
                [ "Select"
                , Helpers.String.indefiniteArticle context.localization.virtualComputer
                , context.localization.virtualComputer
                ]
    in
    Element.column VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join " "
                [ "Assign"
                , context.localization.floatingIpAddress
                    |> Helpers.String.toTitleCase
                ]
            )
        , Element.column [ Element.spacing spacer.px16 ]
            [ Text.strong selectServerText
            , if List.isEmpty serverChoices then
                Element.paragraph []
                    [ Element.text <|
                        String.join " "
                            [ "You don't have any"
                            , context.localization.virtualComputer
                                |> Helpers.String.pluralize
                            , "that don't already have a"
                            , context.localization.floatingIpAddress
                            , "assigned."
                            ]
                    ]

              else
                Style.Widgets.Select.select
                    []
                    context.palette
                    { label = selectServerText
                    , onChange = GotServerUuid
                    , options = serverChoices
                    , selected = model.serverUuid
                    }
            , Text.strong selectIpText
            , if List.isEmpty ipChoices then
                -- TODO suggest user create a floating IP and provide link to that view (once we build it)
                Element.text <|
                    String.concat
                        [ "You don't have any "
                        , context.localization.floatingIpAddress
                            |> Helpers.String.pluralize
                        , " that aren't already assigned to a "
                        , context.localization.virtualComputer
                        , "."
                        ]

              else
                Style.Widgets.Select.select
                    []
                    context.palette
                    { label = selectIpText
                    , onChange = GotIpUuid
                    , options = ipChoices
                    , selected = model.ipUuid
                    }
            , let
                params =
                    case ( model.serverUuid, model.ipUuid ) of
                        ( Just serverUuid, Just ipUuid ) ->
                            case ( GetterSetters.serverLookup project serverUuid, GetterSetters.floatingIpLookup project ipUuid ) of
                                ( Just server, Just floatingIp ) ->
                                    let
                                        ipAssignedToServer =
                                            case GetterSetters.getServerFloatingIps project server.osProps.uuid of
                                                [] ->
                                                    False

                                                serverFloatingIps ->
                                                    List.member floatingIp serverFloatingIps

                                        maybePort =
                                            GetterSetters.getServerPorts project server.osProps.uuid
                                                |> List.head
                                    in
                                    case ( ipAssignedToServer, maybePort ) of
                                        ( True, _ ) ->
                                            { onPress = Nothing
                                            , warnText =
                                                Just <|
                                                    String.join " "
                                                        [ "This"
                                                        , context.localization.floatingIpAddress
                                                        , "is already assigned to this"
                                                        , context.localization.virtualComputer
                                                        ]
                                            }

                                        ( _, Nothing ) ->
                                            { onPress = Nothing
                                            , warnText = Just <| "Error: cannot resolve port"
                                            }

                                        ( False, Just port_ ) ->
                                            -- Conditions are perfect
                                            { onPress =
                                                Just <| SharedMsg <| SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) (SharedMsg.RequestAssignFloatingIp port_ ipUuid)
                                            , warnText = Nothing
                                            }

                                _ ->
                                    {- Either server or floating IP cannot be resolved in the model -}
                                    { onPress = Nothing
                                    , warnText =
                                        Just <|
                                            String.join " "
                                                [ "Error: cannot resolve", context.localization.virtualComputer, "or", context.localization.floatingIpAddress ]
                                    }

                        _ ->
                            {- User hasn't selected a server or floating IP yet so we keep the button disabled but don't yell at them -}
                            { onPress = Nothing
                            , warnText = Nothing
                            }

                button =
                    Element.el [ Element.alignRight ] <|
                        Button.primary
                            context.palette
                            { text = "Assign"
                            , onPress = params.onPress
                            }
              in
              Element.row [ Element.width Element.fill ]
                [ case params.warnText of
                    Just warnText ->
                        Element.el [ Font.color <| SH.toElementColor context.palette.danger.textOnNeutralBG ] <| Element.text warnText

                    Nothing ->
                        Element.none
                , button
                ]
            ]
        ]
