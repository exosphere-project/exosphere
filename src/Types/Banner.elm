module Types.Banner exposing (Banner, BannerLevel(..), BannerModel, Banners, bannerId, decodeBanners, empty)

import ISO8601
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Set exposing (Set)
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
    , id : Maybe String
    }


type alias Banners =
    List Banner


type alias BannerModel =
    { url : String
    , banners : Banners
    , dismissedBanners : Set String
    }


empty : String -> BannerModel
empty url =
    { url = url
    , banners = []
    , dismissedBanners = Set.empty
    }


{-| Identifies a banner, e.g. to remember that it was dismissed. Banners from banners.json are identified by their message.
-}
bannerId : Banner -> String
bannerId banner =
    Maybe.withDefault banner.message banner.id


stringToBannerLevel : String -> Maybe BannerLevel
stringToBannerLevel s =
    case String.toLower s of
        "info" ->
            Just BannerInfo

        "success" ->
            Just BannerSuccess

        "warning" ->
            Just BannerWarning

        "danger" ->
            Just BannerDanger

        "default" ->
            Just BannerDefault

        _ ->
            Nothing


bannerLevelDecoder : Decode.Decoder BannerLevel
bannerLevelDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case stringToBannerLevel str of
                    Just bannerLevel ->
                        Decode.succeed bannerLevel

                    Nothing ->
                        Decode.fail ("Unknown banner level: " ++ str)
            )


iso8601TimeDecoder : Decode.Decoder Time.Posix
iso8601TimeDecoder =
    ISO8601.decode
        |> Decode.map ISO8601.toPosix


decodeBanner : Decode.Decoder Banner
decodeBanner =
    Decode.succeed Banner
        |> required "message" Decode.string
        |> optional "level" bannerLevelDecoder BannerDefault
        |> optional "startsAt" (iso8601TimeDecoder |> Decode.map Just) Nothing
        |> optional "endsAt" (iso8601TimeDecoder |> Decode.map Just) Nothing
        |> hardcoded Nothing


decodeBanners : Decode.Decoder (List Banner)
decodeBanners =
    Decode.list decodeBanner
