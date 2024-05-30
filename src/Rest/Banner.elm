module Rest.Banner exposing (requestBanners)

import Http
import Result.Extra
import Types.Banner exposing (BannerModel, decodeBanners, withBanners)
import Types.SharedMsg as SharedMsg exposing (SharedMsg)


requestBanners : (BannerModel -> SharedMsg) -> BannerModel -> Cmd SharedMsg
requestBanners toCmd bannerModel =
    Http.get
        { url = bannerModel.url
        , expect =
            Http.expectJson
                (Result.Extra.unpack
                    (\_ -> SharedMsg.NoOp)
                    (withBanners bannerModel >> toCmd)
                )
                decodeBanners
        }
