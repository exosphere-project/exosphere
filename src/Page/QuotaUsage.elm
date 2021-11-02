module Page.QuotaUsage exposing (Display(..), ResourceType(..), view)

import Element
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData exposing (RemoteData(..), WebData)
import Style.Widgets.Meter
import View.Helpers as VH
import View.Types


type ResourceType
    = Compute (WebData OSTypes.ComputeQuota)
    | FloatingIp (WebData OSTypes.ComputeQuota) Int
    | Volume (WebData OSTypes.VolumeQuota)


type Display
    = Brief
    | Full


view : View.Types.Context -> Display -> ResourceType -> Element.Element msg
view context display resourceType =
    case resourceType of
        Compute quota ->
            computeQuotaDetails context display quota

        FloatingIp quota floatingIpsUsed ->
            floatingIpQuotaDetails context display quota floatingIpsUsed

        Volume quota ->
            volumeQuotaDetails context display quota


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
            Element.wrappedRow
                (VH.exoRowAttributes
                    ++ [ Element.width Element.fill
                       , Element.spacing 35
                       ]
                )
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
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail context quota (computeInfoItems context display) ]


floatingIpInfoItems : View.Types.Context -> Display -> Int -> OSTypes.ComputeQuota -> Element.Element msg
floatingIpInfoItems context display floatingIpsUsed quota =
    {-
       Compute quota reports incorrect number of floating IPs used (0), so we are overriding it with a count of the floating IPs returned by Neutron.

       Reference: https://access.redhat.com/solutions/3602741

       > openstack limits command report zero number of floating ip(s) used

       > This is not a bug but rather expected out put as the command "openstack limits show" is a part of nova CLI which was earlier used to check the floating ip count of nova-network. That's why it fetch value from nova-network DB and not from actual Neutron DB.For checking floating ip statistics used by neutron then please execute the below command.Raw

       > $openstack floating ip list --project marvel_test -f json | jq  --raw-output '.[] | select(.["Fixed IP Address"] == null ) | .["Floating IP Address"]'
    -}
    let
        incorrectIpsQuota =
            quota.floatingIps

        correctedIpsQuota =
            { incorrectIpsQuota | inUse = floatingIpsUsed }

        brief =
            infoItem context
                correctedIpsQuota
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
            Element.wrappedRow
                (VH.exoRowAttributes ++ [ Element.centerX ])
                [ brief
                ]


floatingIpQuotaDetails : View.Types.Context -> Display -> WebData OSTypes.ComputeQuota -> Int -> Element.Element msg
floatingIpQuotaDetails context display quota floatingIpsUsed =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail context quota (floatingIpInfoItems context display floatingIpsUsed) ]


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
            Element.wrappedRow
                (VH.exoRowAttributes
                    ++ [ Element.centerX
                       , Element.spacing 35
                       ]
                )
                [ brief
                , infoItem context quota.gigabytes ( "Storage used", GibiBytes )
                ]


volumeQuotaDetails : View.Types.Context -> Display -> WebData OSTypes.VolumeQuota -> Element.Element msg
volumeQuotaDetails context display quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail context quota (volumeInfoItems context display) ]
