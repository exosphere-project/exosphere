module Page.FloatingIpList exposing (Model, Msg(..), init, update, view)

import Dict
import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.ResourceList exposing (listItemColumnAttribs)
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Alert as Alert
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)
import Style.Widgets.Icon as Icon
import Style.Widgets.Text as Text
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    }


type Msg
    = GotUnassign OSTypes.IpAddressUuid
    | GotDeleteConfirm OSTypes.IpAddressUuid
    | DataListMsg DataList.Msg
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Model
init showHeading =
    { showHeading = showHeading
    , dataListModel = DataList.init <| DataList.getDefaultFilterOptions filters
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotUnassign ipUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <| SharedMsg.RequestUnassignFloatingIp ipUuid
            )

        GotDeleteConfirm ipUuid ->
            let
                errorContext =
                    ErrorContext
                        ("delete floating IP address with UUID " ++ ipUuid)
                        ErrorCrit
                        Nothing
            in
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project)
                (SharedMsg.RequestDeleteFloatingIp errorContext ipUuid)
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


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        renderFloatingIps : List OSTypes.FloatingIp -> Element.Element Msg
        renderFloatingIps ips =
            let
                -- Warn the user when their project has at least this many unassigned floating IPs.
                -- Perhaps in the future this behavior becomes configurable at runtime.
                ipScarcityWarningThreshold =
                    2

                ipsSorted =
                    List.sortBy (.address >> ipToInt) ips

                ipToInt address =
                    address
                        |> String.split "."
                        |> List.map
                            (\string ->
                                case String.length string of
                                    1 ->
                                        "00" ++ string

                                    2 ->
                                        "0" ++ string

                                    _ ->
                                        string
                            )
                        |> String.concat
                        |> String.toInt
                        |> Maybe.withDefault 0

                ipAssignedToAResource ip =
                    case ip.portUuid of
                        Just _ ->
                            True

                        Nothing ->
                            False

                ( _, ipsNotAssignedToResources ) =
                    List.partition ipAssignedToAResource ipsSorted
            in
            if List.isEmpty ipsSorted then
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.paddingXY 10 0 ])
                    [ Element.text <|
                        String.concat
                            [ "You don't have any "
                            , context.localization.floatingIpAddress
                                |> Helpers.String.pluralize
                            , " yet. They will be created when you launch "
                            , context.localization.virtualComputer
                                |> Helpers.String.indefiniteArticle
                            , " "
                            , context.localization.virtualComputer
                            , "."
                            ]
                    ]

            else
                Element.column
                    [ Element.spacing 24, Element.width Element.fill ]
                    [ if List.length ipsNotAssignedToResources >= ipScarcityWarningThreshold then
                        ipScarcityWarning context

                      else
                        Element.none
                    , DataList.view
                        model.dataListModel
                        DataListMsg
                        context
                        []
                        (floatingIpView context project)
                        (floatingIpRecords ipsSorted)
                        []
                        (Just
                            { filters = filters
                            , dropdownMsgMapper =
                                \dropdownId ->
                                    SharedMsg <| SharedMsg.TogglePopover dropdownId
                            }
                        )
                        Nothing
                    ]
    in
    Element.column
        [ Element.spacing 15, Element.width Element.fill ]
        [ if model.showHeading then
            Text.heading context.palette
                []
                (Icon.ipAddress
                    (SH.toElementColor context.palette.neutral.text.default)
                    24
                )
                (context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

          else
            Element.none
        , Element.column VH.contentContainer
            [ Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.FloatingIp project.networkQuota)
            , VH.renderRDPP
                context
                project.floatingIps
                (Helpers.String.pluralize context.localization.floatingIpAddress)
                renderFloatingIps
            ]
        ]


ipScarcityWarning : View.Types.Context -> Element.Element Msg
ipScarcityWarning context =
    Alert.alert []
        context.palette
        { state = Alert.Warning
        , showIcon = False
        , showContainer = True
        , content =
            Element.paragraph []
                [ Element.text <|
                    String.join " "
                        [ context.localization.floatingIpAddress
                            |> Helpers.String.toTitleCase
                            |> Helpers.String.pluralize
                        , "are a scarce resource. Please delete your unassigned"
                        , context.localization.floatingIpAddress
                            |> Helpers.String.pluralize
                        , "to free them up for other cloud users, unless you are saving them for a specific purpose."
                        ]
                ]
        }


type alias FloatingIpRecord =
    DataList.DataRecord
        { ip : OSTypes.FloatingIp }


floatingIpRecords : List OSTypes.FloatingIp -> List FloatingIpRecord
floatingIpRecords floatingIps =
    List.map
        (\floatingIp ->
            { id = floatingIp.uuid
            , selectable = False
            , ip = floatingIp
            }
        )
        floatingIps


floatingIpView : View.Types.Context -> Project -> FloatingIpRecord -> Element.Element Msg
floatingIpView context project floatingIpRecord =
    let
        assignUnassignIpButton =
            case floatingIpRecord.ip.portUuid of
                Nothing ->
                    Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix
                                (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                    Route.FloatingIpAssign (Just floatingIpRecord.ip.uuid) Nothing
                                )
                        , label =
                            Button.default
                                context.palette
                                { text = "Assign"
                                , onPress = Just NoOp
                                }
                        }

                Just _ ->
                    Button.default
                        context.palette
                        { text = "Unassign"
                        , onPress = Just <| GotUnassign floatingIpRecord.ip.uuid
                        }

        deleteIpBtnWithPopconfirm =
            let
                deleteIpButton togglePopconfirmMsg _ =
                    deleteIconButton
                        context.palette
                        False
                        ("Delete " ++ context.localization.floatingIpAddress)
                        (Just togglePopconfirmMsg)

                deletePopconfirmId =
                    Helpers.String.hyphenate
                        [ "floatingIpListDeletePopconfirm"
                        , project.auth.project.uuid
                        , floatingIpRecord.id
                        ]
            in
            deletePopconfirm context
                (\deletePopconfirmId_ -> SharedMsg <| SharedMsg.TogglePopover deletePopconfirmId_)
                deletePopconfirmId
                { confirmationText =
                    "Are you sure you want to delete this "
                        ++ context.localization.floatingIpAddress
                        ++ "?"
                , onConfirm = Just <| GotDeleteConfirm floatingIpRecord.id
                , onCancel = Just NoOp
                }
                ST.PositionBottomRight
                deleteIpButton

        ipAssignment =
            case floatingIpRecord.ip.portUuid of
                Just _ ->
                    case GetterSetters.getFloatingIpServer project floatingIpRecord.ip of
                        Just server ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.text <|
                                    String.join " "
                                        [ "Assigned to"
                                        , context.localization.virtualComputer
                                        ]
                                , Element.link []
                                    { url =
                                        Route.toUrl context.urlPathPrefix
                                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                                Route.ServerDetail server.osProps.uuid
                                            )
                                    , label =
                                        Element.el
                                            [ Font.color (SH.toElementColor context.palette.primary) ]
                                            (Element.text server.osProps.name)
                                    }
                                ]

                        Nothing ->
                            Element.text "Assigned to a resource that Exosphere cannot represent"

                Nothing ->
                    Element.text "Unassigned"
    in
    Element.column (listItemColumnAttribs context.palette)
        [ Element.row [ Element.width Element.fill ]
            [ Element.el []
                (Style.Widgets.CopyableText.copyableText
                    context.palette
                    [ Font.size 18
                    , Font.color (SH.toElementColor context.palette.neutral.text.default)
                    ]
                    floatingIpRecord.ip.address
                )
            , Element.row [ Element.spacing 12, Element.alignRight ]
                [ assignUnassignIpButton, deleteIpBtnWithPopconfirm ]
            ]
        , Element.row [] [ ipAssignment ]
        ]


filters : List (DataList.Filter { record | ip : OSTypes.FloatingIp })
filters =
    [ { id = "assigned"
      , label = "IP is"
      , chipPrefix = "IP is "
      , filterOptions =
            \_ -> Dict.fromList [ ( "yes", "assigned" ), ( "no", "unassigned" ) ]
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption <| Set.fromList [ "no" ]
      , onFilter =
            \optionValue floatingIpRecord ->
                let
                    ipAssignedToAResource =
                        case floatingIpRecord.ip.portUuid of
                            Just _ ->
                                "yes"

                            Nothing ->
                                "no"
                in
                ipAssignedToAResource == optionValue
      }
    ]
