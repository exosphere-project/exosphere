module Page.FloatingIpCreate exposing (Model, Msg(..), init, update, view)

import Element
import Element.Events as Events
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import IP
import OpenStack.Types as OSTypes
import Page.FloatingIpHelpers
import Route
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.Validation as Validation
import Time
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { ipValue : String
    , ipLostFocus : Bool
    , serverUuid : Maybe OSTypes.ServerUuid
    }


type Msg
    = GotIpValue String
    | GotIpLostFocus
    | GotServerUuid (Maybe OSTypes.ServerUuid)
    | GotSubmit


init : Maybe OSTypes.ServerUuid -> Model
init maybeServerUuid =
    Model "" False maybeServerUuid


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg sharedModel project model =
    case msg of
        GotIpValue ip ->
            ( { model | ipValue = ip, ipLostFocus = False }, Cmd.none, SharedMsg.NoOp )

        GotIpLostFocus ->
            ( { model | ipLostFocus = True }, Cmd.none, SharedMsg.NoOp )

        GotServerUuid maybeUuid ->
            ( { model | serverUuid = maybeUuid }, Cmd.none, SharedMsg.NoOp )

        GotSubmit ->
            let
                ipValForMsg =
                    if String.isEmpty model.ipValue then
                        Nothing

                    else
                        Just model.ipValue
            in
            ( model
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute (GetterSetters.projectIdentifier project) Route.FloatingIpList)
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                case model.serverUuid of
                    Just serverUuid ->
                        SharedMsg.ServerMsg serverUuid <| SharedMsg.RequestCreateServerFloatingIp ipValForMsg

                    Nothing ->
                        SharedMsg.RequestCreateProjectFloatingIp ipValForMsg
            )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project _ model =
    let
        renderInvalidReason reason =
            case reason of
                Just r ->
                    r |> Validation.invalidMessage context.palette

                Nothing ->
                    Element.none

        invalidIpReason =
            if model.ipLostFocus && (not <| String.isEmpty model.ipValue) then
                if IP.validate model.ipValue then
                    Nothing

                else
                    Just ("Invalid " ++ context.localization.floatingIpAddress)

            else
                Nothing
    in
    Element.column
        VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join " "
                [ "Create"
                , context.localization.floatingIpAddress |> Helpers.String.toTitleCase
                ]
            )
        , Element.column [ Element.spacing spacer.px16, Element.width Element.fill ]
            [ Input.text
                (VH.inputItemAttributes context.palette
                    ++ [ Events.onLoseFocus GotIpLostFocus ]
                )
                { text = model.ipValue
                , placeholder =
                    Just
                        (Input.placeholder []
                            (Element.text <|
                                String.join " "
                                    [ "1.2.3.4" ]
                            )
                        )
                , onChange = GotIpValue
                , label =
                    Input.labelAbove [] <|
                        Element.column [ Element.spacing spacer.px8 ]
                            [ Element.text <|
                                String.join " "
                                    [ "Specify"
                                    , context.localization.floatingIpAddress
                                    , "(Optional)"
                                    ]
                            , Text.text Text.Small [] <|
                                String.join " "
                                    [ "Not all"
                                    , Helpers.String.pluralize context.localization.openstackWithOwnKeystone
                                    , "allow this. If you get an error, try again with this field empty."
                                    ]
                            ]
                }
            , renderInvalidReason invalidIpReason
            ]
        , Page.FloatingIpHelpers.serverPicker context project model.serverUuid GotServerUuid
        , Element.row [ Element.width Element.fill ]
            [ Element.el [ Element.alignRight ] <|
                Button.primary
                    context.palette
                    { text = "Create"
                    , onPress =
                        case invalidIpReason of
                            Nothing ->
                                Just GotSubmit

                            _ ->
                                Nothing
                    }
            ]
        ]
