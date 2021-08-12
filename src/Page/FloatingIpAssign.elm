module Page.FloatingIpAssign exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Types.HelperTypes
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { ipUuid : Maybe OSTypes.IpAddressUuid
    , serverUuid : Maybe OSTypes.ServerUuid
    }


type Msg
    = GotIpUuid OSTypes.IpAddressUuid
    | GotServerUuid OSTypes.ServerUuid
      -- Maybe this should be a PortUuid, eh
    | RequestAssignFloatingIp OSTypes.Port OSTypes.IpAddressUuid


init : Maybe OSTypes.IpAddressUuid -> Maybe OSTypes.ServerUuid -> Model
init =
    Model


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotIpUuid ipUuid ->
            ( { model | ipUuid = Just ipUuid }, Cmd.none, SharedMsg.NoOp )

        GotServerUuid serverUuid ->
            ( { model | serverUuid = Just serverUuid }, Cmd.none, SharedMsg.NoOp )

        RequestAssignFloatingIp port_ ipUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid (SharedMsg.RequestAssignFloatingIp port_ ipUuid)
            )


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
                                , OSTypes.ServerBuilding
                                , OSTypes.ServerDeleted
                                ]
                    )
                |> List.filter
                    (\s ->
                        GetterSetters.getServerFloatingIps project s.osProps.uuid |> List.isEmpty
                    )
                |> List.map
                    (\s ->
                        Input.option s.osProps.uuid
                            (Element.text <| VH.possiblyUntitledResource s.osProps.name context.localization.virtualComputer)
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
                        Input.option ip.uuid
                            (Element.el [ Font.family [ Font.monospace ] ] <|
                                Element.text ip.address
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
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el (VH.heading2 context.palette) <|
            Element.text <|
                String.join " "
                    [ "Assign"
                    , context.localization.floatingIpAddress
                        |> Helpers.String.toTitleCase
                    ]
        , Element.column VH.formContainer
            [ Element.el [ Font.bold ] <| Element.text selectServerText
            , if List.isEmpty serverChoices then
                Element.text <|
                    String.join " "
                        [ "You don't have any"
                        , context.localization.virtualComputer
                            |> Helpers.String.pluralize
                        , "that don't already have a"
                        , context.localization.floatingIpAddress
                        , "assigned."
                        ]

              else
                Input.radio []
                    { label =
                        Input.labelHidden selectServerText
                    , onChange = GotServerUuid
                    , options = serverChoices
                    , selected = model.serverUuid
                    }
            , Element.el [ Font.bold ] <| Element.text selectIpText
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
                Input.radio []
                    { label = Input.labelHidden selectIpText
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
                                                Just <| RequestAssignFloatingIp port_ ipUuid
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
                        Widget.textButton
                            (SH.materialStyle context.palette).primaryButton
                            { text = "Assign"
                            , onPress = params.onPress
                            }
              in
              Element.row [ Element.width Element.fill ]
                [ case params.warnText of
                    Just warnText ->
                        Element.el [ Font.color <| SH.toElementColor context.palette.error ] <| Element.text warnText

                    Nothing ->
                        Element.none
                , button
                ]
            ]
        ]
