module Page.ProjectOverview exposing (Model, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Html.Attributes
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
                (volumeTileContents context project)
            , renderTile
                (Icon.ipAddress (SH.toElementColor context.palette.on.background) 24)
                (context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.FloatingIpList
                (Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.FloatingIp project.computeQuota floatingIpsUsedCount))
                (floatingIpTileContents context project)
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
                (keypairTileContents context project)
            ]
        ]


serverTileContents : View.Types.Context -> Project -> Element.Element Msg
serverTileContents context project =
    let
        ownServer : Server -> Bool
        ownServer server =
            server.osProps.details.userUuid == project.auth.user.uuid

        renderServer : Server -> List (Element.Element Msg)
        renderServer server =
            [ VH.serverStatusBadge context.palette server
            , VH.possiblyUntitledResource server.osProps.name context.localization.virtualComputer
                |> VH.ellipsizedText
                |> Element.el
                    [ Element.centerY
                    , Element.width Element.fill
                    , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                    ]
            ]
    in
    tileContents
        context
        project.servers
        context.localization.virtualComputer
        VH.renderRDPP
        renderServer
        ownServer


volumeTileContents : View.Types.Context -> Project -> Element.Element Msg
volumeTileContents context project =
    let
        renderVolume : OSTypes.Volume -> List (Element.Element Msg)
        renderVolume volume =
            [ VH.possiblyUntitledResource volume.name context.localization.blockDevice
                |> VH.ellipsizedText
                |> Element.el
                    [ Element.centerY
                    , Element.width Element.fill
                    , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                    ]
            , (String.fromInt volume.size ++ " GB") |> Element.text |> Element.el [ Element.centerY ]
            ]
    in
    tileContents
        context
        project.volumes
        context.localization.blockDevice
        VH.renderWebData
        renderVolume
        (\_ -> True)


floatingIpTileContents : View.Types.Context -> Project -> Element.Element Msg
floatingIpTileContents context project =
    -- TODO hide any floating IPs? check FloatingIpList.elm
    let
        renderFloatingIp : OSTypes.FloatingIp -> List (Element.Element Msg)
        renderFloatingIp floatingIp =
            [ Element.text floatingIp.address ]
    in
    tileContents
        context
        project.floatingIps
        context.localization.floatingIpAddress
        VH.renderRDPP
        renderFloatingIp
        (\_ -> True)


keypairTileContents : View.Types.Context -> Project -> Element.Element Msg
keypairTileContents context project =
    let
        renderKeypair : OSTypes.Keypair -> List (Element.Element Msg)
        renderKeypair keypair =
            [ Element.text keypair.name ]
    in
    tileContents
        context
        project.keypairs
        context.localization.pkiPublicKeyForSsh
        VH.renderWebData
        renderKeypair
        (\_ -> True)


tileContents :
    View.Types.Context
    -> resourceWithAvailabilityMetadata
    -> String
    ->
        (View.Types.Context
         -> resourceWithAvailabilityMetadata
         -> String
         -> (List singleItem -> Element.Element Msg)
         -> Element.Element Msg
        )
    -> (singleItem -> List (Element.Element Msg))
    -> (singleItem -> Bool)
    -> Element.Element Msg
tileContents context resourceWithAvailabilityMetadata resourceWord renderResource renderItemRowContents showItemInPreview =
    let
        renderItems items =
            let
                shownItems =
                    items
                        |> List.filter showItemInPreview
                        |> List.take 3

                numOtherItems =
                    List.length items - List.length shownItems

                renderItemRow : List (Element.Element msg) -> Element.Element msg
                renderItemRow contents =
                    Element.row
                        [ Element.width Element.fill, Element.height (Element.px 30), Element.spacing 10 ]
                        contents
            in
            Element.column [ Element.width Element.fill, Element.spacing 15 ] <|
                List.concat
                    [ if List.isEmpty shownItems then
                        [ mutedText context <|
                            String.join " "
                                [ "No"
                                , Helpers.String.pluralize resourceWord
                                , "to preview"
                                ]
                        ]

                      else
                        shownItems
                            |> List.map renderItemRowContents
                            |> List.map renderItemRow
                    , if numOtherItems == 0 then
                        []

                      else
                        [ mutedText context <|
                            String.join " "
                                [ "and"
                                , String.fromInt numOtherItems
                                , "more"
                                , resourceWord
                                    |> (if numOtherItems /= 1 then
                                            Helpers.String.pluralize

                                        else
                                            identity
                                       )
                                ]
                        ]
                    ]
    in
    renderResource context resourceWithAvailabilityMetadata (Helpers.String.pluralize resourceWord) renderItems


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
