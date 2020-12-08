module View.QuotaUsage exposing (dashboard)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import OpenStack.Types as OSTypes
import RemoteData exposing (RemoteData(..), WebData)
import Style.Helpers as SH
import Types.Types
    exposing
        ( Msg(..)
        , Project
        , Style
        )
import View.Helpers as VH


dashboard : Style -> Project -> Element.Element Msg
dashboard style project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading2 <| Element.text "Quota/Usage"
        , quotaSections style project
        ]


quotaSections : Style -> Project -> Element.Element Msg
quotaSections style project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ computeQuota style project
        , volumeQuota style project

        -- networkQuota stuff - whenever I find that
        ]


infoItem : Style -> { inUse : Int, limit : Maybe Int } -> ( String, String ) -> Element.Element Msg
infoItem style detail ( label, units ) =
    let
        labelLimit m_ =
            m_
                |> Maybe.map labelUse
                |> Maybe.withDefault "N/A"

        labelUse i_ =
            String.fromInt i_

        bg =
            Background.color <| SH.toElementColor style.palette.surface

        border =
            Border.rounded 5

        pad =
            Element.paddingXY 4 2
    in
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
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


computeQuota : Style -> Project -> Element.Element Msg
computeQuota style project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading3 <| Element.text "Compute"
        , computeQuotaDetails style project.computeQuota
        ]


computeInfoItems : Style -> OSTypes.ComputeQuota -> Element.Element Msg
computeInfoItems style quota =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ infoItem style quota.cores ( "Cores:", "total" )
        , infoItem style quota.instances ( "Instances:", "total" )
        , infoItem style quota.ram ( "RAM:", "MB" )
        ]


quotaDetail : WebData q -> (q -> Element.Element Msg) -> Element.Element Msg
quotaDetail quota infoItemsF =
    case quota of
        NotAsked ->
            Element.el [] <| Element.text "Quota data loading ..."

        Loading ->
            Element.el [] <| Element.text "Quota data still loading ..."

        Failure _ ->
            Element.el [] <| Element.text "Quota data could not be loaded ..."

        Success quota_ ->
            infoItemsF quota_


computeQuotaDetails : Style -> WebData OSTypes.ComputeQuota -> Element.Element Msg
computeQuotaDetails style quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail quota (computeInfoItems style) ]


volumeQuota : Style -> Project -> Element.Element Msg
volumeQuota style project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading3 <| Element.text "Volumes"
        , volumeQuoteDetails style project.volumeQuota
        ]


volumeInfoItems : Style -> OSTypes.VolumeQuota -> Element.Element Msg
volumeInfoItems style quota =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ infoItem style quota.gigabytes ( "Storage:", "GB" )
        , infoItem style quota.volumes ( "Volumes:", "total" )
        ]


volumeQuoteDetails : Style -> WebData OSTypes.VolumeQuota -> Element.Element Msg
volumeQuoteDetails style quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail quota (volumeInfoItems style) ]
