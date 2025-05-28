module View.Banner exposing (view)

import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input
import FeatherIcons as Icons
import Html.Attributes
import Set
import Style.Helpers as SH
import Style.Types exposing (ExoPalette, UIStateColors)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Spacer exposing (spacer)
import Time
import Time.Extra
import Types.Banner exposing (Banner, BannerLevel(..), BannerModel, bannerId)
import View.Helpers as VH


bannerLevelToIcon : BannerLevel -> Icons.Icon
bannerLevelToIcon bannerLevel =
    case bannerLevel of
        BannerDefault ->
            Icons.messageCircle

        BannerInfo ->
            Icons.alertCircle

        BannerSuccess ->
            Icons.checkCircle

        BannerWarning ->
            Icons.alertTriangle

        BannerDanger ->
            Icons.alertOctagon


bannerLevelToColors : ExoPalette -> BannerLevel -> UIStateColors
bannerLevelToColors palette bannerLevel =
    case bannerLevel of
        BannerDefault ->
            palette.muted

        BannerInfo ->
            palette.info

        BannerSuccess ->
            palette.success

        BannerWarning ->
            palette.warning

        BannerDanger ->
            palette.danger


renderBanner : ExoPalette -> (Banner -> msg) -> Banner -> Element.Element msg
renderBanner palette toMsg banner =
    let
        uiColors =
            bannerLevelToColors palette banner.level
    in
    Element.row
        [ Background.color <| SH.toElementColor uiColors.background
        , Font.color <| SH.toElementColor uiColors.textOnColoredBG
        , Element.width Element.fill
        , Element.padding spacer.px12
        , Element.spacing spacer.px12
        ]
        [ featherIcon [] <|
            bannerLevelToIcon banner.level
        , Element.column [ Element.width Element.fill ] <|
            VH.renderMarkdown palette banner.message
        , Element.Input.button
            [ Element.alignRight
            , Element.htmlAttribute (Html.Attributes.attribute "aria-label" "Dismiss notification")
            , Element.htmlAttribute (Html.Attributes.title "Dismiss")
            , Element.htmlAttribute (Html.Attributes.attribute "role" "button")
            ]
            { onPress = Just (toMsg banner)
            , label = featherIcon [] Icons.x
            }
        ]


view : ExoPalette -> (String -> msg) -> Time.Posix -> BannerModel -> Element.Element msg
view palette onDismiss clientCurrentTime model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px4
        ]
        (model.banners
            |> List.filterMap
                (\b ->
                    let
                        started =
                            b.startsAt
                                |> Maybe.map (\v -> Time.Extra.compare clientCurrentTime v == GT)
                                |> Maybe.withDefault True

                        ended =
                            b.endsAt
                                |> Maybe.map (\v -> Time.Extra.compare clientCurrentTime v == GT)
                                |> Maybe.withDefault False
                    in
                    if started && not ended && not (Set.member (bannerId b) model.dismissedBanners) then
                        Just <| renderBanner palette (onDismiss << bannerId) b

                    else
                        Nothing
                )
        )
