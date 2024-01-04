module Page.QuotaUsage exposing (Display(..), ResourceType(..), view)

import Element
import Element.Background as Background
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber, usageComparison, usageLabel)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import OpenStack.VolumeSnapshots exposing (VolumeSnapshot)
import Style.Helpers as SH
import Style.Widgets.Meter
import Style.Widgets.MultiMeter exposing (multiMeter)
import Style.Widgets.Spacer exposing (spacer)
import Types.Error exposing (HttpErrorWithBody)
import View.Helpers as VH
import View.Types


type ResourceType
    = Compute (RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.ComputeQuota)
    | FloatingIp (RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.NetworkQuota)
    | Share (RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.ShareQuota)
    | Volume ( RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.VolumeQuota, RDPP.RemoteDataPlusPlus HttpErrorWithBody (List VolumeSnapshot) )
    | Keypair (RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.ComputeQuota) Int


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

        Share quota ->
            shareQuotaDetails context display quota

        Volume ( quota, snapshotUsage ) ->
            volumeQuotaDetails context display ( quota, snapshotUsage )

        Keypair quota keypairsUsed ->
            keypairQuotaDetails context display quota keypairsUsed


infoItem : View.Types.Context -> { inUse : Int, limit : OSTypes.QuotaItemLimit } -> ( String, Unit ) -> Element.Element msg
infoItem { locale, palette } detail ( label, units ) =
    let
        ( usedCount, usedLabel ) =
            humanNumber { locale | decimals = Exact 0 } units detail.inUse

        ( limitCount, limitLabel ) =
            case detail.limit of
                OSTypes.Limit l ->
                    humanNumber { locale | decimals = Exact 0 } units l

                OSTypes.Unlimited ->
                    ( "", "N/A" )

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
    Style.Widgets.Meter.meter palette
        label
        text
        detail.inUse
        (case detail.limit of
            OSTypes.Limit l ->
                l

            OSTypes.Unlimited ->
                -1
        )


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


quotaDetail : View.Types.Context -> RDPP.RemoteDataPlusPlus HttpErrorWithBody q -> (q -> Element.Element msg) -> Element.Element msg
quotaDetail context quota infoItemsF =
    let
        resourceWord =
            String.join " "
                [ context.localization.maxResourcesPerProject
                    |> Helpers.String.toTitleCase
                , "data"
                ]
    in
    VH.renderRDPP context quota resourceWord infoItemsF


computeQuotaDetails : View.Types.Context -> Display -> RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.ComputeQuota -> Element.Element msg
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


multiMeterPrimaryBackground : View.Types.Context -> Element.Attr aligned attr
multiMeterPrimaryBackground context =
    Background.color (SH.toElementColor context.palette.primary)


multiMeterSecondaryBackground : View.Types.Context -> Element.Attr aligned attr
multiMeterSecondaryBackground context =
    Background.gradient
        { angle = pi / 2
        , steps =
            [ SH.toElementColorWithOpacity context.palette.primary 0.75
            , SH.toElementColor context.palette.primary
            ]
        }


floatingIpQuotaDetails : View.Types.Context -> Display -> RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.NetworkQuota -> Element.Element msg
floatingIpQuotaDetails context display quota =
    quotaDetail context quota (floatingIpInfoItems context display)


shareCountInfoItem : View.Types.Context -> OSTypes.ShareQuota -> Element.Element msg
shareCountInfoItem context projectQuota =
    infoItem context
        projectQuota.shares
        ( String.concat
            [ context.localization.share
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
            , " used"
            ]
        , Count
        )


shareStorageInfoItem : View.Types.Context -> OSTypes.ShareQuota -> Element.Element msg
shareStorageInfoItem context projectQuota =
    infoItem context
        projectQuota.gigabytes
        ( "Storage used", GibiBytes )


shareInfoItems : View.Types.Context -> Display -> OSTypes.ShareQuota -> Element.Element msg
shareInfoItems context display shareQuota =
    case display of
        Brief ->
            shareCountInfoItem context shareQuota

        Full ->
            fullQuotaRow
                [ shareCountInfoItem context shareQuota
                , shareStorageInfoItem context shareQuota
                ]


shareQuotaDetails : View.Types.Context -> Display -> RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.ShareQuota -> Element.Element msg
shareQuotaDetails context display projectQuota =
    quotaDetail context projectQuota (shareInfoItems context display)


keypairInfoItems : View.Types.Context -> Display -> Int -> OSTypes.ComputeQuota -> Element.Element msg
keypairInfoItems context display keypairsUsed quota =
    let
        brief =
            infoItem context
                (OSTypes.QuotaItem keypairsUsed (OSTypes.Limit quota.keypairsLimit))
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


keypairQuotaDetails : View.Types.Context -> Display -> RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.ComputeQuota -> Int -> Element.Element msg
keypairQuotaDetails context display quota keypairsUsed =
    quotaDetail context quota (keypairInfoItems context display keypairsUsed)


briefVolumeInfoItems : View.Types.Context -> ( OSTypes.VolumeQuota, Int ) -> Element.Element msg
briefVolumeInfoItems context ( quota, _ ) =
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
    in
    fullQuotaRow
        [ briefVolumeInfoItems context ( quota, snapshotUsage )
        , case quota.gigabytes.limit of
            OSTypes.Limit l ->
                multiMeter context.palette
                    "Storage used"
                    (usageComparison locale GibiBytes quota.gigabytes.inUse l)
                    l
                    [ ( Helpers.String.toTitleCase context.localization.blockDevice ++ " Usage: " ++ usageLabel locale GibiBytes volumeUsage
                      , volumeUsage
                      , [ multiMeterPrimaryBackground context ]
                      )
                    , ( "Snapshot Usage: " ++ usageLabel locale GibiBytes snapshotUsage
                      , snapshotUsage
                      , [ multiMeterSecondaryBackground context
                        ]
                      )
                    ]

            OSTypes.Unlimited ->
                Element.text "No limits"
        ]


volumeInfoItems : View.Types.Context -> Display -> ( OSTypes.VolumeQuota, Int ) -> Element.Element msg
volumeInfoItems context display volumeInfo =
    case display of
        Brief ->
            briefVolumeInfoItems context volumeInfo

        Full ->
            fullVolumeInfoItems context volumeInfo


volumeQuotaDetails : View.Types.Context -> Display -> ( RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.VolumeQuota, RDPP.RemoteDataPlusPlus HttpErrorWithBody (List VolumeSnapshot) ) -> Element.Element msg
volumeQuotaDetails context display ( quota, snapshotData ) =
    let
        sumSizes =
            List.foldl (\{ sizeInGiB } total -> sizeInGiB + total) 0

        snapshotUsage =
            RDPP.map sumSizes snapshotData

        pairedData =
            RDPP.map2 Tuple.pair quota snapshotUsage
    in
    quotaDetail context pairedData (volumeInfoItems context display)


fullQuotaRow : List (Element.Element msg) -> Element.Element msg
fullQuotaRow items =
    Element.wrappedRow
        [ Element.centerX
        , Element.spacing spacer.px32
        ]
        items
