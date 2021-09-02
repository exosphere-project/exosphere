module Helpers.Url exposing (buildProxyUrl, hostnameFromUrl)

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
