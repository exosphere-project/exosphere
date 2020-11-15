module View.QuotaUsage exposing (dashboard)

import Element
import Element.Font as Font
import OpenStack.Types as OSTypes
import RemoteData exposing (RemoteData(..), WebData)
import Types.Types
    exposing
        ( Msg(..)
        , Project
        )
import View.Helpers as VH


dashboard : Project -> Element.Element Msg
dashboard project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading2 <| Element.text "Quota/Usage"
        , quotaSections project
        ]


quotaSections : Project -> Element.Element Msg
quotaSections project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ computeQuota project
        , volumeQuota project

        -- networkQuota stuff - whenever I find that
        ]


infoItem : { inUse : Int, limit : Maybe Int } -> ( String, String ) -> Element.Element Msg
infoItem detail ( label, units ) =
    let
        labelLimit m_ =
            m_
                |> Maybe.map labelUse
                |> Maybe.withDefault "N/A"

        labelUse i_ =
            String.fromInt i_
    in
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ Element.el [ Font.bold ] <|
            Element.text label
        , Element.el [] <|
            Element.text (labelUse detail.inUse)
        , Element.el [] <|
            Element.text " of "
        , Element.el [] <|
            Element.text (labelLimit detail.limit)
        , Element.el [ Font.italic ] <|
            Element.text units
        ]


computeQuota : Project -> Element.Element Msg
computeQuota project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading3 <| Element.text "Compute"
        , computeQuotaDetails project.computeQuota
        ]


computeInfoItems : OSTypes.ComputeQuota -> Element.Element Msg
computeInfoItems quota =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ infoItem quota.cores ( "Cores:", " total" )
        , infoItem quota.instances ( "Instances:", " total" )
        , infoItem quota.ram ( "RAM:", " MB" )
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


computeQuotaDetails : WebData OSTypes.ComputeQuota -> Element.Element Msg
computeQuotaDetails quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail quota computeInfoItems ]


volumeQuota : Project -> Element.Element Msg
volumeQuota project =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el VH.heading3 <| Element.text "Volumes"
        , volumeQuoteDetails project.volumeQuota
        ]


volumeInfoItems : OSTypes.VolumeQuota -> Element.Element Msg
volumeInfoItems quota =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ infoItem quota.gigabytes ( "Storage:", " GB" )
        , infoItem quota.volumes ( "Volumes:", " total" )
        ]


volumeQuoteDetails : WebData OSTypes.VolumeQuota -> Element.Element Msg
volumeQuoteDetails quota =
    Element.row
        (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ quotaDetail quota volumeInfoItems ]
