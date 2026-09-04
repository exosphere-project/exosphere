module Helpers.GuacamoleEndpoint exposing (Endpoint(..), Instance, TokenAttempt, buildUrl, directModeApplies, guacUpstreamPort, ipv6Address, isIpv6Endpoint, nextEndpointAfterError, resolve)

{-| How the browser reaches the Guacamole server running on an instance. Either through the cloud's
user application proxy, which terminates TLS and forwards to the instance, or straight to the
instance over HTTPS at one of its own addresses.
-}

import Helpers.Cidr as Cidr
import Helpers.Url as UrlHelpers
import Http
import Maybe.Extra
import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Types.Ipv6Reachability exposing (Ipv6Reachability(..))
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

`ipv6Reachability` is what this browser has learned about its own network so far, which decides
whether an IPv6 address is worth trying first.

-}
type alias Instance =
    { directGuacamole : Bool
    , tlsSupported : Bool
    , userAppProxyHostname : Maybe HelperTypes.UserAppProxyHostname
    , floatingIpAddress : Maybe OSTypes.IpAddressValue
    , fixedIpAddresses : List OSTypes.IpAddressValue
    , ipv6Reachability : Ipv6Reachability
    }


{-| One request for a Guacamole token: where it was sent, and whether it is the IPv4 retry that
follows an IPv6 attempt the browser could not reach. The answer comes back long after the request
went out, so the request carries what the answer means.
-}
type alias TokenAttempt =
    { endpoint : Endpoint
    , retryAfterIpv6Failure : Bool
    }


{-| Whether the browser should be talking to this instance directly at all. False means the cloud
has not opted in, or this instance was launched before it did.
-}
directModeApplies : Instance -> Bool
directModeApplies instance =
    instance.directGuacamole && instance.tlsSupported


{-| Decide how to reach Guacamole. Returns `Nothing` when it is not reachable at all.

In direct mode the browser goes to one of the instance's own addresses and never to the user
application proxy. A globally routable IPv6 fixed address wins, because floating IP addresses are
scarce and a user whose network speaks IPv6 should not spend one. The floating IP address is the
fallback, and it becomes the first choice once this browser has found out that it cannot reach
IPv6 at all.

A cloud that has not opted in, and an instance launched before it did, keep using the user
application proxy exactly as before.

-}
resolve : Instance -> Maybe Endpoint
resolve instance =
    if directModeApplies instance then
        let
            viaIpv6 =
                ipv6Address instance |> Maybe.map Direct

            viaFloatingIp =
                instance.floatingIpAddress |> Maybe.map Direct
        in
        case instance.ipv6Reachability of
            Unreachable ->
                Maybe.Extra.or viaFloatingIp viaIpv6

            _ ->
                Maybe.Extra.or viaIpv6 viaFloatingIp

    else
        Maybe.map2 ViaUserAppProxy instance.userAppProxyHostname instance.floatingIpAddress


{-| The instance's first globally routable IPv6 address, if it has one. Link-local and unique-local
addresses are not reachable from a browser, so they do not count.
-}
ipv6Address : Instance -> Maybe OSTypes.IpAddressValue
ipv6Address instance =
    instance.fixedIpAddresses
        |> List.filter Cidr.isGlobalUnicastIPv6
        |> List.head


{-| Whether this endpoint is the instance's IPv6 address, which is the one a browser on an
IPv4-only network cannot open a connection to.
-}
isIpv6Endpoint : Endpoint -> Bool
isIpv6Endpoint endpoint =
    case endpoint of
        Direct address ->
            Cidr.isGlobalUnicastIPv6 address

        ViaUserAppProxy _ _ ->
            False


{-| Where to send the next token request after one failed, or `Nothing` to keep trying the same
place.

Only a network error says anything about addressing: the browser could not open a connection at
all, so if the instance has a floating IP address as well, that is worth trying right away. Every
other error came back from the instance itself, which means the address is fine and changing it
would prove nothing.

-}
nextEndpointAfterError : Http.Error -> Endpoint -> Maybe OSTypes.IpAddressValue -> Maybe Endpoint
nextEndpointAfterError error attemptedEndpoint maybeFloatingIpAddress =
    case ( error, isIpv6Endpoint attemptedEndpoint, maybeFloatingIpAddress ) of
        ( Http.NetworkError, True, Just floatingIpAddress ) ->
            Just <| Direct floatingIpAddress

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
