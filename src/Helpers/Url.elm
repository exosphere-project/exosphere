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


{-| Whether two URLs name the same host, used to tell one cloud from another.

The host is all that is compared, and the port deliberately is not. The URL a login is made to and
the URL that cloud's catalog hands back for the same service routinely differ in port, path and
trailing slash: `https://mycloud.example:5000/v3` against `https://mycloud.example/identity/v3`
is one cloud, and counting the port would split it into two projects for every user. Two different
Keystones behind one hostname would be missed, which is the rarer problem and the smaller one, and
telling clouds fully apart belongs with project identity rather than here.

Hosts are compared without regard to case, since DNS does not have any. A URL that cannot be
parsed, which includes an IPv6 literal, is only ever the same host as a URL written exactly the
same way: two clouds are never mistaken for each other, at the cost of one cloud written two ways
being mistaken for two.

-}
sameHost : HelperTypes.Url -> HelperTypes.Url -> Bool
sameHost urlStrA urlStrB =
    case ( Url.fromString urlStrA, Url.fromString urlStrB ) of
        ( Just urlA, Just urlB ) ->
            String.toLower urlA.host == String.toLower urlB.host

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
