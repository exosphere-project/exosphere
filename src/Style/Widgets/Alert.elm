module Style.Widgets.Alert exposing (AlertState(..), alert)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import FeatherIcons
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Spacer exposing (spacer)


type AlertState
    = Info
    | Success
    | Warning
    | Danger


alert :
    List (Element.Attribute msg)
    -> ST.ExoPalette
    ->
        { state : AlertState
        , showIcon : Bool
        , showContainer : Bool
        , content : Element.Element msg
        }
    -> Element.Element msg
alert styleAttrs palette { state, showIcon, showContainer, content } =
    let
        ( stateColor, icon ) =
            case state of
                Info ->
                    ( palette.info, FeatherIcons.alertCircle )

                Success ->
                    ( palette.success, FeatherIcons.check )

                Warning ->
                    ( palette.warning, FeatherIcons.alertTriangle )

                Danger ->
                    ( palette.danger, FeatherIcons.alertOctagon )

        alertIcon =
            if showIcon then
                Element.el
                    [ Element.alignTop
                    ]
                    (icon
                        |> FeatherIcons.withSize 1.4
                        |> FeatherIcons.withSizeUnit "em"
                        -- so that it can scale as per custom font size given to the alert (if any)
                        |> FeatherIcons.toHtml []
                        |> Element.html
                    )

            else
                Element.none

        regionAttrs =
            case state of
                Danger ->
                    [ Region.announceUrgently ]

                _ ->
                    []

        containerAttrs =
            if showContainer then
                [ Background.color (stateColor.background |> SH.toElementColor)
                , Font.color (stateColor.textOnColoredBG |> SH.toElementColor)
                , Border.width 1
                , Border.rounded 4
                , Border.color (stateColor.border |> SH.toElementColor)
                ]

            else
                []
    in
    Element.row
        ([ Element.padding spacer.px16
         , Element.spacing spacer.px12
         , Font.size 16
         , Font.color (stateColor.textOnNeutralBG |> SH.toElementColor)
         ]
            ++ containerAttrs
            ++ regionAttrs
            -- to let consumer add/override the alert style
            ++ styleAttrs
        )
        [ alertIcon, content ]
