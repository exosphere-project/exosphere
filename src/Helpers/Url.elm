module Helpers.Url exposing (buildProxyUrl, dataUrl, hostnameFromUrl, textDataUrl)

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
