module Helpers.Url exposing (buildProxyUrl, dataUrl, hostnameFromUrl, sameHost, textDataUrl)

import Base64
import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Url
import Url.Builder


hostnameFromUrl : HelperTypes.Url -> String
hostnameFromUrl urlStr =
    let
        maybeUrl =
            Url.fromString urlStr
    in
    case maybeUrl of
        Just url ->
            url.host

        Nothing ->
            "placeholder-url-unparseable"


{-| Whether two URLs name the same host. Used to tell one cloud from another, because the URL a
login is made to and the URL that cloud's catalog hands back for the same service routinely differ
in port, path and trailing slash. A URL that cannot be parsed is only ever the same host as itself.
-}
sameHost : HelperTypes.Url -> HelperTypes.Url -> Bool
sameHost urlStrA urlStrB =
    case ( Url.fromString urlStrA, Url.fromString urlStrB ) of
        ( Just urlA, Just urlB ) ->
            urlA.host == urlB.host

        _ ->
            urlStrA == urlStrB


buildProxyUrl : HelperTypes.UserAppProxyHostname -> OSTypes.IpAddressValue -> Int -> Url.Protocol -> HelperTypes.UrlPath -> HelperTypes.UrlParams -> String
buildProxyUrl proxyHostname destinationIp port_ protocol =
    [ "https://"
    , case protocol of
        Url.Http ->
            "http-"

        Url.Https ->
            ""
    , destinationIp |> String.replace "." "-"
    , "-"
    , String.fromInt port_
    , "."
    , proxyHostname
    ]
        |> String.concat
        |> Url.Builder.crossOrigin


dataUrl : String -> String -> String
dataUrl mimetype contents =
    "data:" ++ mimetype ++ ";base64," ++ Base64.encode contents


textDataUrl : String -> String
textDataUrl =
    dataUrl "text/plain;charset=utf-8"
