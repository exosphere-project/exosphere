module View.QuotaUsage exposing (dashboard)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import OpenStack.Types as OSTypes
import RemoteData exposing (RemoteData(..), WebData)
import Style.Helpers as SH
import Style.Types
import Types.Types
    exposing
        ( Msg(..)
        , Project
        )
import View.Helpers as VH


dashboard : Style.Types.ExoPalette -> Project -> Element.Element Msg
dashboard palette project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading2 <| Element.text "Quota/Usage"
        , quotaSections palette project
        ]


quotaSections : Style.Types.ExoPalette -> Project -> Element.Element Msg
quotaSections palette project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ computeQuota palette project
        , volumeQuota palette project

        -- networkQuota stuff - whenever I find that
        ]


infoItem : Style.Types.ExoPalette -> { inUse : Int, limit : Maybe Int } -> ( String, String ) -> Element.Element Msg
infoItem palette detail ( label, units ) =
    let
        labelLimit m_ =
            m_
                |> Maybe.map labelUse
                |> Maybe.withDefault "N/A"

        labelUse i_ =
            String.fromInt i_

        bg =
            Background.color <| SH.toElementColor palette.surface

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


computeQuota : Style.Types.ExoPalette -> Project -> Element.Element Msg
computeQuota palette project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading3 <| Element.text "Compute"
        , computeQuotaDetails palette project.computeQuota
        ]


computeInfoItems : Style.Types.ExoPalette -> OSTypes.ComputeQuota -> Element.Element Msg
computeInfoItems palette quota =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ infoItem palette quota.cores ( "Cores:", "total" )
        , infoItem palette quota.instances ( "Instances:", "total" )
        , infoItem palette quota.ram ( "RAM:", "MB" )
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


computeQuotaDetails : Style.Types.ExoPalette -> WebData OSTypes.ComputeQuota -> Element.Element Msg
computeQuotaDetails exoPalette quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail quota (computeInfoItems exoPalette) ]


volumeQuota : Style.Types.ExoPalette -> Project -> Element.Element Msg
volumeQuota palette project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading3 <| Element.text "Volumes"
        , volumeQuoteDetails palette project.volumeQuota
        ]


volumeInfoItems : Style.Types.ExoPalette -> OSTypes.VolumeQuota -> Element.Element Msg
volumeInfoItems palette quota =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ infoItem palette quota.gigabytes ( "Storage:", "GB" )
        , infoItem palette quota.volumes ( "Volumes:", "total" )
        ]


volumeQuoteDetails : Style.Types.ExoPalette -> WebData OSTypes.VolumeQuota -> Element.Element Msg
volumeQuoteDetails palette quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail quota (volumeInfoItems palette) ]
