module Rest.Banner exposing (receiveBanners, requestBanners)

import Http
import Rest.Helpers exposing (expectJsonWithErrorBody)
import Types.Banner exposing (BannerModel, Banners, decodeBanners)
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.SharedMsg exposing (SharedMsg)


errorContext : ErrorContext
errorContext =
    ErrorContext
        "receive banners"
        ErrorInfo
        (Just "Check the formatting of your banners.json")


requestBanners : (ErrorContext -> Result HttpErrorWithBody Banners -> SharedMsg) -> BannerModel -> Cmd SharedMsg
requestBanners toCmd bannerModel =
    Http.get
        { url = bannerModel.url
        , expect =
            expectJsonWithErrorBody
                (toCmd errorContext)
                decodeBanners
        }


receiveBanners : BannerModel -> Banners -> ( BannerModel, Cmd SharedMsg )
receiveBanners bannerModel result =
    ( { bannerModel | banners = result }
    , Cmd.none
    )
