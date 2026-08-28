module Helpers.GuacamoleEndpoint exposing (Endpoint(..), Instance, buildUrl, directModeApplies, guacUpstreamPort, resolve)

{-| How the browser reaches the Guacamole server running on an instance. Either through the cloud's
user application proxy, which terminates TLS and forwards to the instance, or straight to the
instance over HTTPS at one of its own addresses.
-}

import Helpers.Cidr as Cidr
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


{-| What resolving an endpoint needs to know about a cloud and one of its instances.

`directGuacamole` is the cloud's opt-in. `tlsSupported` is whether this particular instance was
deployed to serve Guacamole itself, read back from its `exoGuac` metadata; an instance launched
before the cloud opted in does not have it, and keeps using the user application proxy.

-}
type alias Instance =
    { directGuacamole : Bool
    , tlsSupported : Bool
    , userAppProxyHostname : Maybe HelperTypes.UserAppProxyHostname
    , floatingIpAddress : Maybe OSTypes.IpAddressValue
    , fixedIpAddresses : List OSTypes.IpAddressValue
    }


{-| Whether the browser should be talking to this instance directly at all. False means the cloud
has not opted in, or this instance was launched before it did.
-}
directModeApplies : Instance -> Bool
directModeApplies instance =
    instance.directGuacamole && instance.tlsSupported


{-| Decide how to reach Guacamole. Returns `Nothing` when it is not reachable at all.

In direct mode the instance's floating IP address wins, because every project has IPv4 and not
every project has IPv6. A globally routable IPv6 fixed address is the fallback. An instance with
neither, or on a cloud that has not opted in, falls back to the user application proxy, which is
also the only route for instances launched before the cloud opted in.

-}
resolve : Instance -> Maybe Endpoint
resolve instance =
    let
        viaUserAppProxy () =
            Maybe.map2 ViaUserAppProxy instance.userAppProxyHostname instance.floatingIpAddress
    in
    if directModeApplies instance then
        case instance.floatingIpAddress of
            Just floatingIp ->
                Just <| Direct floatingIp

            Nothing ->
                case List.filter Cidr.isGlobalUnicastIPv6 instance.fixedIpAddresses |> List.head of
                    Just ipv6Address ->
                        Just <| Direct ipv6Address

                    Nothing ->
                        -- The proxy needs a floating IP address too, so in practice this is
                        -- `Nothing`. It is written as the fallback anyway so that the rule stays
                        -- "direct where it works, the proxy otherwise" in one place.
                        viaUserAppProxy ()

    else
        viaUserAppProxy ()


{-| Build a URL to the given path and query parameters on Guacamole, however it is reached.
-}
buildUrl : Endpoint -> HelperTypes.UrlPath -> HelperTypes.UrlParams -> String
buildUrl endpoint =
    case endpoint of
        ViaUserAppProxy proxyHostname ipAddress ->
            UrlHelpers.buildProxyUrl proxyHostname ipAddress guacUpstreamPort Url.Http

        Direct ipAddress ->
            UrlHelpers.buildDirectUrl ipAddress 443
