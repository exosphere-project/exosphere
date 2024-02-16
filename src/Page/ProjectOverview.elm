module Page.ProjectOverview exposing (Model, Msg, init, update, view)

import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons as Icons
import Helpers.Formatting
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Html.Attributes
import OpenStack.Types as OSTypes
import Page.Jetstream2Allocation
import Page.QuotaUsage
import Route
import Style.Helpers as SH
import Style.Widgets.Card
import Style.Widgets.Icon as Icon
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Time
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    {}


type Msg
    = SharedMsg SharedMsg.SharedMsg


init : Model
init =
    Model


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime _ =
    let
        renderTile : Element.Element Msg -> String -> Route.ProjectRouteConstructor -> Maybe (Element.Element Msg) -> Element.Element Msg -> Element.Element Msg
        renderTile icon str projRouteConstructor quotaMeter contents =
            Element.link []
                { url = Route.toUrl context.urlPathPrefix (Route.ProjectRoute (GetterSetters.projectIdentifier project) projRouteConstructor)
                , label =
                    tile context
                        [ Element.column
                            [ Element.padding spacer.px24
                            , Element.width Element.fill
                            , Element.spacing spacer.px32
                            ]
                            [ Text.subheading context.palette
                                [ Element.padding 0
                                , Border.width 0
                                , Element.pointer
                                ]
                                icon
                                str
                            , case quotaMeter of
                                Just quotaMeter_ ->
                                    Element.el [ Element.centerX ] quotaMeter_

                                Nothing ->
                                    Element.none
                            , contents
                            ]
                        ]
                }

        renderDescription : String -> Element.Element Msg
        renderDescription description =
            let
                -- view is placed in a container of 24px padding
                viewWidth =
                    context.windowSize.width - 24 * 2

                -- 450 is tile width and 24 is the spacing between tiles
                numTiles =
                    viewWidth // (450 + (24 // 2))

                tilesColumWidth =
                    450 * numTiles + 24 * (numTiles - 1)
            in
            Element.paragraph
                -- ugly hack to constraint description to the tiles column width (until we figure out a better way)
                [ Element.width <| Element.px tilesColumWidth ]
                [ Element.text description ]

        keypairsUsedCount =
            project.keypairs
                |> RDPP.withDefault []
                |> List.length
    in
    Element.column
        [ Element.spacing spacer.px24 ]
        [ Page.Jetstream2Allocation.view context project currentTime
            |> Element.map SharedMsg
        , VH.renderMaybe project.description renderDescription
        , Element.wrappedRow [ Element.spacing spacer.px24 ]
            [ renderTile
                (Icon.featherIcon [] Icons.server)
                (context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.ServerList
                (Just <| Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.Compute project.computeQuota))
                (serverTileContents context project)
            , renderTile
                (Icon.featherIcon [] Icons.hardDrive)
                (context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.VolumeList
                (Just <| Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.Volume ( project.volumeQuota, project.volumeSnapshots )))
                (volumeTileContents context project)
            , case ( context.experimentalFeaturesEnabled, project.endpoints.manila ) of
                ( True, Just _ ) ->
                    renderTile
                        (Icon.featherIcon [] Icons.share2)
                        (context.localization.share
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                        )
                        Route.ShareList
                        (Just <| Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.Share project.shareQuota))
                        (shareTileContents context project)

                _ ->
                    Element.none
            , renderTile
                (Icon.ipAddress (SH.toElementColor context.palette.neutral.text.default) 24)
                (context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.FloatingIpList
                (Just <| Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.FloatingIp project.networkQuota))
                (floatingIpTileContents context project)
            , renderTile
                (Icon.featherIcon [] Icons.key)
                (context.localization.pkiPublicKeyForSsh
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.KeypairList
                (Just <| Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.Keypair project.computeQuota keypairsUsedCount))
                (keypairTileContents context project)
            , renderTile
                (Icon.featherIcon [] Icons.package)
                (context.localization.staticRepresentationOfBlockDeviceContents
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.ImageList
                Nothing
                (imageTileContents context project)
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
            [ VH.extendedResourceName (Just server.osProps.name) server.osProps.uuid context.localization.virtualComputer
                |> VH.ellipsizedText
                |> Element.el
                    [ Element.centerY
                    , Element.width Element.fill
                    , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                    ]
            , VH.serverStatusBadge context.palette server
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
            [ VH.extendedResourceName volume.name volume.uuid context.localization.blockDevice
                |> VH.ellipsizedText
                |> Element.el
                    [ Element.centerY
                    , Element.width Element.fill
                    , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                    ]
            , Helpers.Formatting.usageLabel context.locale Helpers.Formatting.GibiBytes volume.size
                |> Element.text
                |> Element.el [ Element.centerY ]
            ]
    in
    tileContents
        context
        project.volumes
        context.localization.blockDevice
        VH.renderRDPP
        renderVolume
        (\_ -> True)


shareTileContents : View.Types.Context -> Project -> Element.Element Msg
shareTileContents context project =
    let
        renderShare : OSTypes.Share -> List (Element.Element Msg)
        renderShare share =
            [ VH.extendedResourceName share.name share.uuid context.localization.share
                |> VH.ellipsizedText
                |> Element.el
                    [ Element.centerY
                    , Element.width Element.fill
                    , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                    ]
            , Helpers.Formatting.usageLabel context.locale Helpers.Formatting.GibiBytes share.size
                |> Element.text
                |> Element.el [ Element.centerY ]
            ]

        showShare : OSTypes.Share -> Bool
        showShare share =
            share.userUuid == project.auth.user.uuid
    in
    tileContents
        context
        project.shares
        context.localization.share
        VH.renderRDPP
        renderShare
        showShare


floatingIpTileContents : View.Types.Context -> Project -> Element.Element Msg
floatingIpTileContents context project =
    let
        renderFloatingIp : OSTypes.FloatingIp -> List (Element.Element Msg)
        renderFloatingIp floatingIp =
            [ Element.text floatingIp.address
            ]

        showFloatingIp : OSTypes.FloatingIp -> Bool
        showFloatingIp floatingIp =
            case floatingIp.portUuid of
                Just _ ->
                    False

                Nothing ->
                    True
    in
    tileContents
        context
        project.floatingIps
        context.localization.floatingIpAddress
        VH.renderRDPP
        renderFloatingIp
        showFloatingIp


keypairTileContents : View.Types.Context -> Project -> Element.Element Msg
keypairTileContents context project =
    let
        renderKeypair : OSTypes.Keypair -> List (Element.Element Msg)
        renderKeypair keypair =
            [ Element.el [ Element.centerY ] (Element.text keypair.name)
            , Element.el
                [ Element.centerY
                , Element.width Element.fill
                , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                , Font.family [ Font.monospace ]
                , Text.fontSize Text.Small
                ]
                (VH.ellipsizedText keypair.fingerprint)
            ]
    in
    tileContents
        context
        project.keypairs
        context.localization.pkiPublicKeyForSsh
        VH.renderRDPP
        renderKeypair
        (\_ -> True)


imageTileContents : View.Types.Context -> Project -> Element.Element Msg
imageTileContents context project =
    let
        renderImage : OSTypes.Image -> List (Element.Element Msg)
        renderImage image =
            [ VH.extendedResourceName (Just image.name) image.uuid context.localization.staticRepresentationOfBlockDeviceContents
                |> VH.ellipsizedText
                |> Element.el
                    [ Element.centerY
                    , Element.width Element.fill
                    , Element.htmlAttribute <| Html.Attributes.style "min-width" "0"
                    ]
            , OSTypes.imageVisibilityToString image.visibility
                |> Element.text
                |> Element.el
                    [ Element.centerY
                    , context.palette.neutral.text.subdued
                        |> SH.toElementColor
                        |> Font.color
                    ]
            ]
    in
    tileContents
        context
        project.images
        context.localization.staticRepresentationOfBlockDeviceContents
        VH.renderRDPP
        renderImage
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
        subduedText : String -> Element.Element Msg
        subduedText text =
            Element.el
                [ Element.centerX
                , context.palette.neutral.text.subdued
                    |> SH.toElementColor
                    |> Font.color
                ]
            <|
                Element.text text

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
                        [ Element.width Element.fill, Element.height (Element.px 30), Element.spacing spacer.px12 ]
                        contents
            in
            Element.column [ Element.width Element.fill, Element.spacing spacer.px12 ] <|
                List.concat
                    [ if List.isEmpty shownItems then
                        [ subduedText <|
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
                        [ subduedText <|
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


tile : View.Types.Context -> List (Element.Element Msg) -> Element.Element Msg
tile context contents =
    Style.Widgets.Card.clickableCardFixedSize context.palette 450 350 contents
