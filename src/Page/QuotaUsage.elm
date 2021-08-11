module Page.QuotaUsage exposing (ResourceType(..), view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData exposing (RemoteData(..), WebData)
import Style.Helpers as SH
import View.Helpers as VH
import View.Types


type ResourceType
    = Compute (WebData OSTypes.ComputeQuota)
    | FloatingIp (WebData OSTypes.ComputeQuota) Int
    | Volume (WebData OSTypes.VolumeQuota)


view : View.Types.Context -> ResourceType -> Element.Element msg
view context resourceType =
    case resourceType of
        Compute quota ->
            computeQuotaDetails context quota

        FloatingIp quota floatingIpsUsed ->
            floatingIpQuotaDetails context quota floatingIpsUsed

        Volume quota ->
            volumeQuotaDetails context quota


infoItem : View.Types.Context -> { inUse : Int, limit : Maybe Int } -> ( String, String ) -> Element.Element msg
infoItem context detail ( label, units ) =
    let
        labelLimit m_ =
            m_
                |> Maybe.map labelUse
                |> Maybe.withDefault "N/A"

        labelUse i_ =
            String.fromInt i_

        bg =
            Background.color <| SH.toElementColor context.palette.surface

        border =
            Border.rounded 5

        pad =
            Element.paddingXY 4 2
    in
    Element.row
        (VH.exoRowAttributes ++ [ Element.spacing 5, Element.width Element.fill ])
        [ Element.el [ Font.bold ] <|
            Element.text label
        , Element.el [ bg, border, pad ] <|
            Element.text (labelUse detail.inUse)
        , Element.el [] <|
            Element.text "of"
        , Element.el [ bg, border, pad ] <|
            Element.text (labelLimit detail.limit)
        , Element.el [ Font.italic ] <|
            Element.text units
        ]


computeInfoItems : View.Types.Context -> OSTypes.ComputeQuota -> Element.Element msg
computeInfoItems context quota =
    Element.wrappedRow
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ infoItem context
            quota.instances
            ( String.join " "
                [ context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                , "used:"
                ]
            , "total"
            )
        , infoItem context quota.cores ( "Cores used:", "total" )
        , infoItem context quota.ram ( "RAM used:", "MB" )
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


computeQuotaDetails : View.Types.Context -> WebData OSTypes.ComputeQuota -> Element.Element msg
computeQuotaDetails context quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail context quota (computeInfoItems context) ]


floatingIpInfoItems : View.Types.Context -> Int -> OSTypes.ComputeQuota -> Element.Element msg
floatingIpInfoItems context floatingIpsUsed quota =
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
    in
    Element.wrappedRow
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ infoItem context
            correctedIpsQuota
            ( String.join " "
                [ context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                , "used:"
                ]
            , "total"
            )
        ]


floatingIpQuotaDetails : View.Types.Context -> WebData OSTypes.ComputeQuota -> Int -> Element.Element msg
floatingIpQuotaDetails context quota floatingIpsUsed =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail context quota (floatingIpInfoItems context floatingIpsUsed) ]


volumeInfoItems : View.Types.Context -> OSTypes.VolumeQuota -> Element.Element msg
volumeInfoItems context quota =
    Element.wrappedRow
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ infoItem
            context
            quota.volumes
            ( String.join " "
                [ context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                , "used:"
                ]
            , "total"
            )
        , infoItem context quota.gigabytes ( "Storage used:", "GB" )
        ]


volumeQuotaDetails : View.Types.Context -> WebData OSTypes.VolumeQuota -> Element.Element msg
volumeQuotaDetails context quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail context quota (volumeInfoItems context) ]
