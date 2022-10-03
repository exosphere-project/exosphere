module Page.QuotaUsage exposing (Display(..), ResourceType(..), view)

import Element
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData exposing (RemoteData(..), WebData)
import Style.Helpers exposing (spacer)
import Style.Widgets.Meter
import View.Helpers as VH
import View.Types


type ResourceType
    = Compute (WebData OSTypes.ComputeQuota)
    | FloatingIp (WebData OSTypes.NetworkQuota)
    | Volume (WebData OSTypes.VolumeQuota)
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

        Volume quota ->
            volumeQuotaDetails context display quota

        Keypair quota keypairsUsed ->
            keypairQuotaDetails context display quota keypairsUsed


infoItem : View.Types.Context -> { inUse : Int, limit : Maybe Int } -> ( String, Unit ) -> Element.Element msg
infoItem context detail ( label, units ) =
    let
        { locale } =
            context

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
    Style.Widgets.Meter.meter context.palette label text detail.inUse (Maybe.withDefault -1 detail.limit)


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
                [ brief
                ]


keypairQuotaDetails : View.Types.Context -> Display -> WebData OSTypes.ComputeQuota -> Int -> Element.Element msg
keypairQuotaDetails context display quota keypairsUsed =
    quotaDetail context quota (keypairInfoItems context display keypairsUsed)


volumeInfoItems : View.Types.Context -> Display -> OSTypes.VolumeQuota -> Element.Element msg
volumeInfoItems context display quota =
    let
        brief =
            infoItem
                context
                quota.volumes
                ( String.join " "
                    [ context.localization.blockDevice
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
                , infoItem context quota.gigabytes ( "Storage used", GibiBytes )
                ]


volumeQuotaDetails : View.Types.Context -> Display -> WebData OSTypes.VolumeQuota -> Element.Element msg
volumeQuotaDetails context display quota =
    quotaDetail context quota (volumeInfoItems context display)


fullQuotaRow : List (Element.Element msg) -> Element.Element msg
fullQuotaRow items =
    Element.wrappedRow
        [ Element.centerX
        , Element.spacing spacer.px32
        ]
        items
