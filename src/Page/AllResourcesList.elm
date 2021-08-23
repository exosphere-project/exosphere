module Page.AllResourcesList exposing (Model, Msg, init, update, view)

import Element
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Helpers.String
import Page.FloatingIpList
import Page.KeypairList
import Page.ServerList
import Page.VolumeList
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { serverListModel : Page.ServerList.Model
    , volumeListModel : Page.VolumeList.Model
    , keypairListModel : Page.KeypairList.Model
    , floatingIpListModel : Page.FloatingIpList.Model
    }


type Msg
    = ServerListMsg Page.ServerList.Msg
    | VolumeListMsg Page.VolumeList.Msg
    | KeypairListMsg Page.KeypairList.Msg
    | FloatingIpListMsg Page.FloatingIpList.Msg
    | SharedMsg SharedMsg.SharedMsg


init : Model
init =
    Model
        (Page.ServerList.init False)
        (Page.VolumeList.init False)
        (Page.KeypairList.init False)
        (Page.FloatingIpList.init False)


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        -- Repetitive dispatch code, unsure if there's a better way
        ServerListMsg msg_ ->
            let
                ( pageModel, pageCmd, sharedMsg ) =
                    Page.ServerList.update msg_ project model.serverListModel
            in
            ( { model | serverListModel = pageModel }, Cmd.map ServerListMsg pageCmd, sharedMsg )

        VolumeListMsg msg_ ->
            let
                ( pageModel, pageCmd, sharedMsg ) =
                    Page.VolumeList.update msg_ project model.volumeListModel
            in
            ( { model | volumeListModel = pageModel }, Cmd.map VolumeListMsg pageCmd, sharedMsg )

        KeypairListMsg msg_ ->
            let
                ( pageModel, pageCmd, sharedMsg ) =
                    Page.KeypairList.update msg_ project model.keypairListModel
            in
            ( { model | keypairListModel = pageModel }, Cmd.map KeypairListMsg pageCmd, sharedMsg )

        FloatingIpListMsg msg_ ->
            let
                ( pageModel, pageCmd, sharedMsg ) =
                    Page.FloatingIpList.update msg_ project model.floatingIpListModel
            in
            ( { model | floatingIpListModel = pageModel }, Cmd.map FloatingIpListMsg pageCmd, sharedMsg )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context p model =
    let
        renderHeaderLink : Element.Element Msg -> String -> Msg -> Element.Element Msg
        renderHeaderLink icon str msg =
            Element.row
                (VH.heading3 context.palette
                    ++ [ Element.spacing 12
                       , Events.onClick msg
                       , Element.mouseOver
                            [ Font.color
                                (context.palette.primary
                                    |> SH.toElementColor
                                )
                            ]
                       , Element.pointer
                       ]
                )
                [ icon
                , Element.text str
                ]
    in
    Element.column
        [ Element.spacing 25, Element.width Element.fill ]
        [ Element.column
            [ Element.width Element.fill ]
            [ renderHeaderLink
                (FeatherIcons.server
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SharedMsg <| SharedMsg.NavigateToView <| SharedMsg.ProjectPage p.auth.project.uuid SharedMsg.ServerList)
            , Page.ServerList.view context
                p
                model.serverListModel
                |> Element.map ServerListMsg
            ]
        , Element.column
            [ Element.width Element.fill ]
            [ renderHeaderLink
                (FeatherIcons.hardDrive
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SharedMsg <| SharedMsg.NavigateToView <| SharedMsg.ProjectPage p.auth.project.uuid SharedMsg.VolumeList)
            , Page.VolumeList.view context
                p
                model.volumeListModel
                |> Element.map VolumeListMsg
            ]
        , Element.column
            [ Element.width Element.fill ]
            [ renderHeaderLink
                (Icon.ipAddress (SH.toElementColor context.palette.on.background) 24)
                (context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SharedMsg <| SharedMsg.NavigateToView <| SharedMsg.ProjectPage p.auth.project.uuid SharedMsg.FloatingIpList)
            , Page.FloatingIpList.view context
                p
                model.floatingIpListModel
                |> Element.map FloatingIpListMsg
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.spacingXY 0 15 -- Because no quota view taking up space
            ]
            [ renderHeaderLink
                (FeatherIcons.key
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.pkiPublicKeyForSsh
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SharedMsg <| SharedMsg.NavigateToView <| SharedMsg.ProjectPage p.auth.project.uuid SharedMsg.KeypairList)
            , Page.KeypairList.view context
                p
                model.keypairListModel
                |> Element.map KeypairListMsg
            ]
        ]
