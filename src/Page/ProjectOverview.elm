module Page.ProjectOverview exposing (Model, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Style.Helpers as SH
import Style.Widgets.Card
import Style.Widgets.Icon as Icon
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    Model


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project _ =
    let
        renderTile : Element.Element Msg -> String -> Route.ProjectRouteConstructor -> Element.Element Msg -> Element.Element Msg -> Element.Element Msg
        renderTile icon str projRouteConstructor quotaMeter contents =
            Element.link []
                { url = Route.toUrl context.urlPathPrefix (Route.ProjectRoute project.auth.project.uuid projRouteConstructor)
                , label =
                    tile context
                        [ Element.column [ Element.padding 10, Element.width Element.fill, Element.spacing 20 ]
                            [ Element.row
                                (VH.heading3 context.palette
                                    ++ [ Element.spacing 12
                                       , Element.paddingEach { bottom = 0, left = 0, right = 0, top = 0 }
                                       , Border.width 0
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
                            , Element.el [ Element.centerX ] quotaMeter
                            , contents
                            ]
                        ]
                }

        renderDescription : String -> Element.Element Msg
        renderDescription description =
            Element.el VH.contentContainer <|
                Element.paragraph [ Font.italic ] [ Element.text description ]

        floatingIpsUsedCount =
            project.floatingIps
                -- Defaulting to 0 if not loaded yet, not the greatest factoring
                |> RDPP.withDefault []
                |> List.length
    in
    Element.column
        [ Element.spacing 15, Element.width Element.fill ]
        [ VH.renderMaybe project.description renderDescription
        , Element.wrappedRow [ Element.spacing 25 ]
            [ renderTile
                (FeatherIcons.server
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.ServerList
                (Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.Compute project.computeQuota))
                (serverTileContents context project)
            , renderTile
                (FeatherIcons.hardDrive
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.VolumeList
                (Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.Volume project.volumeQuota))
                (Element.column []
                    [ Element.text "some contents"
                    ]
                )
            , renderTile
                (Icon.ipAddress (SH.toElementColor context.palette.on.background) 24)
                (context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.FloatingIpList
                (Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.FloatingIp project.computeQuota floatingIpsUsedCount))
                (Element.column []
                    [ Element.text "some contents"
                    ]
                )
            , renderTile
                (FeatherIcons.key
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.pkiPublicKeyForSsh
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.KeypairList
                Element.none
                (Element.text "some contents")
            ]
        ]


serverTileContents : View.Types.Context -> Project -> Element.Element Msg
serverTileContents context project =
    let
        ownServer : OSTypes.UserUuid -> Server -> Bool
        ownServer userUuid server =
            server.osProps.details.userUuid == userUuid

        renderServer : Server -> Element.Element Msg
        renderServer server =
            Element.row [ Element.width Element.fill, Element.height (Element.px 30), Element.spacing 10 ]
                [ VH.serverStatusBadge context.palette server
                , VH.possiblyUntitledResource server.osProps.name context.localization.virtualComputer
                    |> VH.ellipsizedText
                ]

        renderServers : List Server -> Element.Element Msg
        renderServers servers =
            let
                shownServers =
                    servers
                        |> List.filter (ownServer project.auth.user.uuid)
                        |> List.take 5

                numOtherServers =
                    List.length servers - List.length shownServers
            in
            Element.column [ Element.width Element.fill, Element.spacing 15 ] <|
                List.concat
                    [ if List.isEmpty shownServers then
                        [ mutedText context <|
                            String.join " "
                                [ "No"
                                , context.localization.virtualComputer |> Helpers.String.pluralize
                                , "created by you"
                                ]
                        ]

                      else
                        List.map renderServer shownServers
                    , if numOtherServers == 0 then
                        []

                      else
                        [ mutedText context <|
                            String.join " "
                                [ "and"
                                , String.fromInt numOtherServers
                                , "more"
                                , context.localization.virtualComputer |> Helpers.String.pluralize
                                ]
                        ]
                    ]
    in
    VH.renderRDPP
        context
        project.servers
        (context.localization.virtualComputer |> Helpers.String.pluralize)
        renderServers


mutedText : View.Types.Context -> String -> Element.Element Msg
mutedText context text =
    Element.el
        [ Element.centerX
        , context.palette.muted
            |> SH.toElementColor
            |> Font.color
        ]
    <|
        Element.text text


tile : View.Types.Context -> List (Element.Element Msg) -> Element.Element Msg
tile context contents =
    Style.Widgets.Card.exoCardFixedSize context.palette 450 350 contents
