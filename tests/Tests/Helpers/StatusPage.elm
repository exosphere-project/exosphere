module Tests.Helpers.StatusPage exposing (pruneDismissedStatusBannersSuite)

import Expect
import Helpers.StatusPage
import Set
import Test exposing (Test, describe, test)
import Types.Banner exposing (Banner, BannerLevel(..))


statusBanner : String -> Banner
statusBanner id =
    { message = "message"
    , level = BannerDanger
    , startsAt = Nothing
    , endsAt = Nothing
    , id = Just id
    }


pruneDismissedStatusBannersSuite : Test
pruneDismissedStatusBannersSuite =
    describe "Helpers.StatusPage.pruneDismissedStatusBanners"
        [ test "forgets dismissals of banners the cloud no longer shows, and leaves other clouds and the deployer's banners alone" <|
            \_ ->
                Set.fromList
                    [ "statusPage:cloud-a:still shown"
                    , "statusPage:cloud-a:resolved"
                    , "statusPage:cloud-b:other cloud"
                    , "Office hours every Tuesday"
                    ]
                    |> Helpers.StatusPage.pruneDismissedStatusBanners "cloud-a" [ statusBanner "statusPage:cloud-a:still shown" ]
                    |> Expect.equal
                        (Set.fromList
                            [ "statusPage:cloud-a:still shown"
                            , "statusPage:cloud-b:other cloud"
                            , "Office hours every Tuesday"
                            ]
                        )
        , test "tells status page banner ids from the deployer's" <|
            \_ ->
                [ "statusPage:cloud-a:message", "Office hours every Tuesday" ]
                    |> List.map Helpers.StatusPage.isStatusBannerId
                    |> Expect.equal [ True, False ]
        ]
