module Helpers.Url exposing (buildDirectUrl, buildProxyUrl, dataUrl, hostnameFromUrl, textDataUrl)

import Base64
import Helpers.Cidr as Cidr
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


{-| Build an HTTPS URL that addresses a host directly, with no reverse proxy in between.

IPv6 literals are wrapped in square brackets, IPv4 addresses and hostnames are used as-is.
Port 443 is left out because it is the default for HTTPS.

-}
buildDirectUrl : OSTypes.IpAddressValue -> Int -> HelperTypes.UrlPath -> HelperTypes.UrlParams -> String
buildDirectUrl destinationIp port_ =
    [ "https://"
    , if Cidr.isValidIPv6 destinationIp then
        "[" ++ destinationIp ++ "]"

      else
        destinationIp
    , if port_ == 443 then
        ""

      else
        ":" ++ String.fromInt port_
    ]
        |> String.concat
        |> Url.Builder.crossOrigin


dataUrl : String -> String -> String
dataUrl mimetype contents =
    "data:" ++ mimetype ++ ";base64," ++ Base64.encode contents


textDataUrl : String -> String
textDataUrl =
    dataUrl "text/plain;charset=utf-8"
