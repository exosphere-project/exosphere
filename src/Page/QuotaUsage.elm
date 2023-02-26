module Page.QuotaUsage exposing (Display(..), ResourceType(..), view)

import Element
import Element.Background as Background
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData exposing (RemoteData(..), WebData)
import Style.Helpers as SH
import Style.Widgets.Meter
import Style.Widgets.MultiMeter exposing (multiMeter)
import Style.Widgets.Spacer exposing (spacer)
import View.Helpers as VH
import View.Types


type ResourceType
    = Compute (WebData OSTypes.ComputeQuota)
    | FloatingIp (WebData OSTypes.NetworkQuota)
    | Volume ( WebData OSTypes.VolumeQuota, WebData (List OSTypes.VolumeSnapshot) )
    | Keypair (WebData OSTypes.ComputeQuota) Int


type Display
    = Brief
    | Full


view : View.Types.Context -> Display -> ResourceType -> Element.Element msg
view context display resourceType =
    case resourceType of
        Compute quota ->
            computeQuotaDetails context display quota

        FloatingIp quota ->
            floatingIpQuotaDetails context display quota

        Volume ( quota, snapshotUsage ) ->
            volumeQuotaDetails context display ( quota, snapshotUsage )

        Keypair quota keypairsUsed ->
            keypairQuotaDetails context display quota keypairsUsed


infoItem : View.Types.Context -> { inUse : Int, limit : Maybe Int } -> ( String, Unit ) -> Element.Element msg
infoItem { locale, palette } detail ( label, units ) =
    let
        ( usedCount, usedLabel ) =
            humanNumber { locale | decimals = Exact 0 } units detail.inUse

        ( limitCount, limitLabel ) =
            detail.limit
                |> Maybe.map (humanNumber { locale | decimals = Exact 0 } units)
                |> Maybe.withDefault ( "", "N/A" )

        text =
            String.join " " <|
                List.concat
                    [ [ usedCount ]
                    , if usedLabel == limitLabel then
                        []

                      else
                        [ usedLabel ]
                    , [ "of" ]
                    , [ limitCount ]
                    , [ limitLabel ]
                    ]
    in
    Style.Widgets.Meter.meter palette label text detail.inUse (Maybe.withDefault -1 detail.limit)


computeInfoItems : View.Types.Context -> Display -> OSTypes.ComputeQuota -> Element.Element msg
computeInfoItems context display quota =
    let
        brief =
            infoItem context
                quota.instances
                ( String.join " "
                    [ context.localization.virtualComputer
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "used"
                    ]
                , Count
                )
    in
    case display of
        Brief ->
            brief

        Full ->
            fullQuotaRow
                [ brief
                , infoItem context quota.cores ( "Cores used", Count )
                , infoItem context quota.ram ( "RAM used", MebiBytes )
                ]


quotaDetail : View.Types.Context -> WebData q -> (q -> Element.Element msg) -> Element.Element msg
quotaDetail context quota infoItemsF =
    let
        resourceWord =
            String.join " "
                [ context.localization.maxResourcesPerProject
                    |> Helpers.String.toTitleCase
                , "data"
                ]
    in
    VH.renderWebData context quota resourceWord infoItemsF


computeQuotaDetails : View.Types.Context -> Display -> WebData OSTypes.ComputeQuota -> Element.Element msg
computeQuotaDetails context display quota =
    quotaDetail context quota (computeInfoItems context display)


floatingIpInfoItems : View.Types.Context -> Display -> OSTypes.NetworkQuota -> Element.Element msg
floatingIpInfoItems context display quota =
    let
        brief =
            infoItem context
                quota.floatingIps
                ( String.join " "
                    [ context.localization.floatingIpAddress
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "used"
                    ]
                , Count
                )
    in
    case display of
        Brief ->
            brief

        Full ->
            fullQuotaRow
                [ brief
                ]


floatingIpQuotaDetails : View.Types.Context -> Display -> WebData OSTypes.NetworkQuota -> Element.Element msg
floatingIpQuotaDetails context display quota =
    quotaDetail context quota (floatingIpInfoItems context display)


keypairInfoItems : View.Types.Context -> Display -> Int -> OSTypes.ComputeQuota -> Element.Element msg
keypairInfoItems context display keypairsUsed quota =
    let
        brief =
            infoItem context
                (OSTypes.QuotaItemDetail keypairsUsed (Just quota.keypairsLimit))
                ( String.join " "
                    [ context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "used"
                    ]
                , Count
                )
    in
    case display of
        Brief ->
            brief

        Full ->
            fullQuotaRow
                [ brief ]


keypairQuotaDetails : View.Types.Context -> Display -> WebData OSTypes.ComputeQuota -> Int -> Element.Element msg
keypairQuotaDetails context display quota keypairsUsed =
    quotaDetail context quota (keypairInfoItems context display keypairsUsed)


briefVolumeInfoItems : View.Types.Context -> ( OSTypes.VolumeQuota, Int ) -> Element.Element msg
briefVolumeInfoItems context ( quota, snapshotUsage ) =
    let
        { localization } =
            context

        blockDevice =
            localization.blockDevice
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
    in
    infoItem context quota.volumes ( blockDevice ++ " used", Count )


fullVolumeInfoItems : View.Types.Context -> ( OSTypes.VolumeQuota, Int ) -> Element.Element msg
fullVolumeInfoItems context ( quota, snapshotUsage ) =
    let
        { locale } =
            context

        volumeUsage =
            quota.gigabytes.inUse - snapshotUsage

        usageLabels usage =
            humanNumber { locale | decimals = Exact 0 } CinderGB usage

        join ( a, b ) =
            a ++ " " ++ b

        ( usedCount, usedLabel ) =
            usageLabels quota.gigabytes.inUse

        ( limitCount, limitLabel ) =
            quota.gigabytes.limit
                |> Maybe.map usageLabels
                |> Maybe.withDefault ( "", "N/A" )

        -- No need to display the units on both numbers if they are the same.
        usageDescription =
            if usedLabel == limitLabel then
                -- 1.3 of 2.0 TB
                usedCount ++ " of " ++ limitCount ++ " " ++ limitLabel

            else
                -- 743 GB of 2.0 TB
                usedCount ++ " " ++ usedLabel ++ " of " ++ limitCount ++ " " ++ limitLabel
    in
    fullQuotaRow
        [ briefVolumeInfoItems context ( quota, snapshotUsage )
        , case quota.gigabytes.limit of
            Just limit ->
                multiMeter context
                    "Storage used"
                    usageDescription
                    limit
                    [ ( "Volume Usage: " ++ join (usageLabels volumeUsage)
                      , volumeUsage
                      , [ Background.color (SH.toElementColor context.palette.primary) ]
                      )
                    , ( "Snapshot Usage: " ++ join (usageLabels snapshotUsage)
                      , snapshotUsage
                      , [ Background.color (SH.toElementColorWithOpacity context.palette.primary 0.85) ]
                      )
                    ]

            Nothing ->
                Element.text "No limits"
        ]


volumeInfoItems : View.Types.Context -> Display -> ( OSTypes.VolumeQuota, Int ) -> Element.Element msg
volumeInfoItems context display volumeInfo =
    case display of
        Brief ->
            briefVolumeInfoItems context volumeInfo

        Full ->
            fullVolumeInfoItems context volumeInfo


volumeQuotaDetails : View.Types.Context -> Display -> ( WebData OSTypes.VolumeQuota, WebData (List OSTypes.VolumeSnapshot) ) -> Element.Element msg
volumeQuotaDetails context display ( quota, snapshotData ) =
    let
        sumSizes =
            List.foldl (\{ sizeInGiB } total -> sizeInGiB + total) 0

        snapshotUsage =
            RemoteData.map sumSizes snapshotData

        pairedData =
            RemoteData.map2 Tuple.pair quota snapshotUsage
    in
    quotaDetail context pairedData (volumeInfoItems context display)


fullQuotaRow : List (Element.Element msg) -> Element.Element msg
fullQuotaRow items =
    Element.wrappedRow
        [ Element.centerX
        , Element.spacing spacer.px32
        ]
        items
