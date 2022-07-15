module Page.ProjectOverview exposing (Model, Msg, init, update, view)

import DateFormat.Relative
import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales
import Helpers.Formatting
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Time
import Html.Attributes
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import RemoteData
import Route
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Card
import Style.Widgets.Icon as Icon
import Style.Widgets.Meter
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Time
import Types.Jetstream2Accounting
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    {}


type Msg
    = SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Model
init =
    Model


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime _ =
    let
        renderTile : Element.Element Msg -> String -> Route.ProjectRouteConstructor -> Element.Element Msg -> Element.Element Msg -> Element.Element Msg
        renderTile icon str projRouteConstructor quotaMeter contents =
            Element.link []
                { url = Route.toUrl context.urlPathPrefix (Route.ProjectRoute (GetterSetters.projectIdentifier project) projRouteConstructor)
                , label =
                    tile context
                        [ Element.column [ Element.padding 18, Element.width Element.fill, Element.spacing 20 ]
                            [ Text.subheading context.palette
                                [ Element.paddingEach { bottom = 0, left = 0, right = 0, top = 0 }
                                , Border.width 0
                                , Element.pointer
                                ]
                                icon
                                str
                            , Element.el [ Element.centerX ] quotaMeter
                            , contents
                            ]
                        ]
                }

        renderDescription : String -> Element.Element Msg
        renderDescription description =
            Element.el [ Font.italic, Element.width (Element.fill |> Element.maximum 600) ] (VH.ellipsizedText description)

        keypairsUsedCount =
            project.keypairs
                |> RemoteData.withDefault []
                |> List.length
    in
    Element.column
        [ Element.spacing 25, Element.width Element.fill ]
        [ Element.row [ Element.spacing 20 ]
            [ renderJetstream2Allocation context project currentTime
            , VH.renderMaybe project.description renderDescription
            ]
        , Element.wrappedRow [ Element.spacing 25, Element.width Element.fill ]
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
                (Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.FloatingIp project.networkQuota))
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
                (Page.QuotaUsage.view context Page.QuotaUsage.Brief (Page.QuotaUsage.Keypair project.computeQuota keypairsUsedCount))
                (keypairTileContents context project)
            , renderTile
                (FeatherIcons.package
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.staticRepresentationOfBlockDeviceContents
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                Route.ImageList
                Element.none
                (imageTileContents context project)
            ]
        ]


renderJetstream2Allocation : View.Types.Context -> Project -> Time.Posix -> Element.Element Msg
renderJetstream2Allocation context project currentTime =
    let
        meter : Types.Jetstream2Accounting.Allocation -> Element.Element Msg
        meter allocation =
            let
                serviceUnitsUsed =
                    allocation.serviceUnitsUsed |> Maybe.map round |> Maybe.withDefault 0

                subtitle =
                    -- Hard-coding USA locale to work around some kind of bug in elm-format-number where 1000000 is rendered as 10,00,000.
                    -- Don't worry, approximately all Jetstream2 users are USA-based, and nobody else will see this.
                    String.join " "
                        [ serviceUnitsUsed
                            |> Helpers.Formatting.humanCount FormatNumber.Locales.usLocale
                        , "of"
                        , allocation.serviceUnitsAllocated
                            |> round
                            |> Helpers.Formatting.humanCount FormatNumber.Locales.usLocale
                        , "SUs"
                        ]
            in
            Style.Widgets.Meter.meter
                context.palette
                "Allocation usage"
                subtitle
                serviceUnitsUsed
                (round allocation.serviceUnitsAllocated)

        toggleTip : Types.Jetstream2Accounting.Allocation -> Element.Element Msg
        toggleTip allocation =
            let
                contents : Element.Element Msg
                contents =
                    [ String.concat
                        [ "Start: "
                        , DateFormat.Relative.relativeTime currentTime allocation.startDate
                        , " ("
                        , Helpers.Time.humanReadableDate allocation.startDate
                        , ")"
                        ]
                    , String.concat
                        [ "End: "
                        , DateFormat.Relative.relativeTime currentTime allocation.endDate
                        , " ("
                        , Helpers.Time.humanReadableDate allocation.endDate
                        , ")"
                        ]
                    ]
                        |> List.map Element.text
                        |> Element.column []

                toggleTipId =
                    Helpers.String.hyphenate
                        [ "JS2AllocationTip"
                        , project.auth.project.uuid
                        ]
            in
            Style.Widgets.ToggleTip.toggleTip
                context
                (\toggleTipId_ -> SharedMsg <| SharedMsg.TogglePopover toggleTipId_)
                toggleTipId
                contents
                ST.PositionRight

        renderRDPPSuccess : Maybe Types.Jetstream2Accounting.Allocation -> Element.Element Msg
        renderRDPPSuccess maybeAllocation =
            case maybeAllocation of
                Nothing ->
                    Element.text "Jetstream2 allocation information not found."

                Just allocation ->
                    Element.row [ Element.spacing 8 ] [ meter allocation, toggleTip allocation ]
    in
    case project.endpoints.jetstream2Accounting of
        Just _ ->
            -- Is a Jetstream2 project
            VH.renderRDPP context project.jetstream2Allocation "allocation" renderRDPPSuccess

        Nothing ->
            -- Is not a Jetstream2 project
            Element.none


serverTileContents : View.Types.Context -> Project -> Element.Element Msg
serverTileContents context project =
    let
        ownServer : Server -> Bool
        ownServer server =
            server.osProps.details.userUuid == project.auth.user.uuid

        renderServer : Server -> List (Element.Element Msg)
        renderServer server =
            [ VH.possiblyUntitledResource server.osProps.name context.localization.virtualComputer
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
            [ VH.possiblyUntitledResource (Maybe.withDefault "" volume.name) context.localization.blockDevice
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
    let
        renderFloatingIp : OSTypes.FloatingIp -> List (Element.Element Msg)
        renderFloatingIp floatingIp =
            [ Element.text floatingIp.address ]

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
                , Font.size 14
                ]
                (VH.ellipsizedText keypair.fingerprint)
            ]
    in
    tileContents
        context
        project.keypairs
        context.localization.pkiPublicKeyForSsh
        VH.renderWebData
        renderKeypair
        (\_ -> True)


imageTileContents : View.Types.Context -> Project -> Element.Element Msg
imageTileContents context project =
    let
        renderImage : OSTypes.Image -> List (Element.Element Msg)
        renderImage image =
            [ VH.possiblyUntitledResource image.name context.localization.staticRepresentationOfBlockDeviceContents
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
                    , context.palette.muted.textOnNeutralBG
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
        mutedText : String -> Element.Element Msg
        mutedText text =
            Element.el
                [ Element.centerX
                , context.palette.muted.textOnNeutralBG
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
                        [ Element.width Element.fill, Element.height (Element.px 30), Element.spacing 10 ]
                        contents
            in
            Element.column [ Element.width Element.fill, Element.spacing 15 ] <|
                List.concat
                    [ if List.isEmpty shownItems then
                        [ mutedText <|
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
                        [ mutedText <|
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
