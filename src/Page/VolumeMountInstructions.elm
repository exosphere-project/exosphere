module Page.VolumeMountInstructions exposing (Model, Msg, init, update, view)

import Element
import Helpers.Helpers as Helpers
import Helpers.String
import OpenStack.Types as OSTypes
import Route
import Style.Helpers as SH
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


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
    Element.column VH.exoColumnAttributes
        [ Element.el (VH.heading2 context.palette) <|
            Element.text <|
                String.join " "
                    [ context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    , "Attached"
                    ]
        , Element.column VH.contentContainer
            [ Element.text ("Device: " ++ model.device)
            , case Helpers.volDeviceToMountpoint model.device of
                Just mountpoint ->
                    Element.text ("Mount point: " ++ mountpoint)

                Nothing ->
                    Element.none
            , Element.paragraph []
                [ case Helpers.volDeviceToMountpoint model.device of
                    Just mountpoint ->
                        Element.text <|
                            String.join " "
                                [ "We'll try to mount this"
                                , context.localization.blockDevice
                                , "at"
                                , mountpoint
                                , "on your instance's filesystem. You should be able to access the"
                                , context.localization.blockDevice
                                , "there."
                                , "If it's a completely empty"
                                , context.localization.blockDevice
                                , "we'll also try to format it first."
                                , "This may not work on older operating systems (like CentOS 7 or Ubuntu 16.04)."
                                , "In that case, you may need to format and/or mount the"
                                , context.localization.blockDevice
                                , "manually."
                                ]

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
                        (Route.ProjectRoute project.auth.project.uuid <|
                            Route.ServerDetail model.serverUuid
                        )
                , label =
                    Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Go to my " ++ context.localization.virtualComputer
                        , onPress = Just <| SharedMsg <| SharedMsg.NoOp
                        }
                }
            ]
        ]
