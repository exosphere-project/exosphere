module Page.VolumeMountInstructions exposing (Model, Msg, init, update, view)

import Element
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import Route
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.Server exposing (ExoFeature(..))
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    OSTypes.VolumeAttachment


type Msg
    = SharedMsg SharedMsg.SharedMsg


init : OSTypes.VolumeAttachment -> Model
init =
    identity


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        volumeName =
            GetterSetters.volumeLookup project model.attachmentUuid
                |> Maybe.andThen .name

        maybeServer =
            GetterSetters.serverLookup project model.serverUuid

        maybeMountpoint =
            case maybeServer of
                Just server ->
                    if GetterSetters.serverSupportsFeature NamedMountpoints server then
                        volumeName |> Maybe.andThen GetterSetters.volNameToMountpoint

                    else
                        GetterSetters.volDeviceToMountpoint model.device

                Nothing ->
                    GetterSetters.volDeviceToMountpoint model.device
    in
    Element.column VH.contentContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join
                " "
                [ context.localization.blockDevice
                    |> Helpers.String.toTitleCase
                , "Attached"
                ]
            )
        , Element.column [ Element.spacing spacer.px16 ]
            [ case volumeName of
                Just name ->
                    Element.text
                        (String.concat
                            [ Helpers.String.toTitleCase context.localization.blockDevice
                            , ": "
                            , name
                            ]
                        )

                Nothing ->
                    Element.none
            , Element.text ("Device: " ++ model.device)
            , case maybeMountpoint of
                Just mountpoint ->
                    Element.text ("Mount point: " ++ mountpoint)

                Nothing ->
                    Element.none
            , Element.paragraph []
                [ case maybeMountpoint of
                    Just mountpoint ->
                        let
                            markdown =
                                String.concat
                                    [ "The "
                                    , context.localization.virtualComputer
                                    , " will mount your "
                                    , context.localization.blockDevice
                                    , " to "
                                    , mountpoint
                                    , ".\n\nFor example, type `cd "
                                    , mountpoint
                                    , "` in "
                                    , Helpers.String.indefiniteArticle context.localization.commandDrivenTextInterface
                                    , " "
                                    , context.localization.commandDrivenTextInterface
                                    , " on the "
                                    , context.localization.virtualComputer
                                    , ".\n\n(This may not work on older operating systems, like CentOS 7. In that case, you may need to format and/or mount the "
                                    , context.localization.blockDevice
                                    , " manually.)"
                                    ]
                        in
                        Element.column [ Element.spacing spacer.px12 ] (VH.renderMarkdown context.palette markdown)

                    Nothing ->
                        Element.text <|
                            String.join " "
                                [ "We attached the"
                                , context.localization.blockDevice
                                , "but couldn't determine a mountpoint from the device path. You may need to format and/or mount the"
                                , context.localization.blockDevice
                                , "manually."
                                ]
                ]
            , Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.ServerDetail model.serverUuid
                        )
                , label =
                    Button.primary
                        context.palette
                        { text = "Go to my " ++ context.localization.virtualComputer
                        , onPress = Just <| SharedMsg <| SharedMsg.NoOp
                        }
                }
            ]
        ]
