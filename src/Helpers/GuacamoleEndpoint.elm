module Helpers.GuacamoleEndpoint exposing (Endpoint(..), buildUrl, guacUpstreamPort, resolve)

{-| How the browser reaches the Guacamole server running on an instance. Today it always goes
through the cloud's user application proxy, which terminates TLS and forwards to the instance.
`Direct` describes reaching the instance over HTTPS on its own address instead; nothing constructs
it yet, a follow-up change enables it.
-}

import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Url


{-| A way to reach Guacamole on an instance: through the cloud's user application proxy, or
directly at one of the instance's own addresses.
-}
type Endpoint
    = ViaUserAppProxy HelperTypes.UserAppProxyHostname OSTypes.IpAddressValue
    | Direct OSTypes.IpAddressValue


{-| The port that Guacamole listens on inside an Exosphere-deployed instance.
-}
guacUpstreamPort : Int
guacUpstreamPort =
    49528


{-| Decide how to reach Guacamole from what is known about the cloud and the instance.
Returns `Nothing` when Guacamole is not reachable at all.
-}
resolve : Maybe HelperTypes.UserAppProxyHostname -> Maybe OSTypes.IpAddressValue -> Maybe Endpoint
resolve maybeProxyHostname maybeIpAddress =
    case ( maybeProxyHostname, maybeIpAddress ) of
        ( Just proxyHostname, Just ipAddress ) ->
            Just <| ViaUserAppProxy proxyHostname ipAddress

        _ ->
            Nothing


{-| Build a URL to the given path and query parameters on Guacamole, however it is reached.
-}
buildUrl : Endpoint -> HelperTypes.UrlPath -> HelperTypes.UrlParams -> String
buildUrl endpoint =
    case endpoint of
        ViaUserAppProxy proxyHostname ipAddress ->
            UrlHelpers.buildProxyUrl proxyHostname ipAddress guacUpstreamPort Url.Http

        Direct ipAddress ->
            UrlHelpers.buildDirectUrl ipAddress 443
