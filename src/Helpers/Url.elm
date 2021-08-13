module Helpers.Url exposing (buildProxyUrl, hostnameFromUrl, urlPathQueryMatches)

import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Url


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


urlPathQueryMatches : Url.Url -> String -> Bool
urlPathQueryMatches urlType urlStr =
    let
        urlTypeQueryStr =
            case urlType.query of
                Just q ->
                    "?" ++ q

                Nothing ->
                    ""
    in
    (urlType.path ++ urlTypeQueryStr) == urlStr


buildProxyUrl : HelperTypes.UserAppProxyHostname -> OSTypes.IpAddressValue -> Int -> String -> Bool -> String
buildProxyUrl proxyHostname destinationIp port_ path https_upstream =
    [ "https://"
    , if https_upstream then
        ""

      else
        "http-"
    , destinationIp |> String.replace "." "-"
    , "-"
    , String.fromInt port_
    , "."
    , proxyHostname
    , path
    ]
        |> String.concat
