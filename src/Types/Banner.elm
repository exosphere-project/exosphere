module Types.Banner exposing (Banner, BannerLevel(..), BannerModel, Banners, decodeBanners, empty, withBanners)

import ISO8601
import Json.Decode as Decode
import Time


type BannerLevel
    = BannerDefault
    | BannerInfo
    | BannerSuccess
    | BannerWarning
    | BannerDanger


type alias Banner =
    { message : String
    , level : BannerLevel
    , startsAt : Maybe Time.Posix
    , endsAt : Maybe Time.Posix
    }


type alias Banners =
    List Banner


type alias BannerModel =
    { url : String
    , banners : Banners
    }


empty : String -> BannerModel
empty url =
    { url = url
    , banners = []
    }


withBanners : BannerModel -> List Banner -> BannerModel
withBanners model banners =
    { model
        | banners = banners
    }


stringToBannerLevel : String -> BannerLevel
stringToBannerLevel s =
    case String.toLower s of
        "info" ->
            BannerInfo

        "success" ->
            BannerSuccess

        "warning" ->
            BannerWarning

        "danger" ->
            BannerDanger

        _ ->
            BannerDefault


decodeBannerLevel : Decode.Decoder BannerLevel
decodeBannerLevel =
    Decode.string
        |> Decode.map stringToBannerLevel


decodeBanner : Decode.Decoder Banner
decodeBanner =
    Decode.oneOf
        [ Decode.map (\message -> Banner message BannerDefault Nothing Nothing) Decode.string
        , Decode.map4 Banner
            (Decode.field "message" Decode.string)
            -- Decode a banner level from a string, with a default
            (Decode.maybe (Decode.field "level" decodeBannerLevel)
                |> Decode.map (Maybe.withDefault BannerDefault)
            )
            (Decode.maybe (Decode.field "startsAt" (Decode.map (\v -> ISO8601.toPosix v) ISO8601.decode)))
            (Decode.maybe (Decode.field "endsAt" (Decode.map (\v -> ISO8601.toPosix v) ISO8601.decode)))
        ]


decodeBanners : Decode.Decoder (List Banner)
decodeBanners =
    Decode.list (Decode.maybe decodeBanner)
        |> Decode.map (List.filterMap identity)
