module Types.Ipv6Reachability exposing (Ipv6Reachability(..))

{-| Whether this browser can reach an instance over IPv6.

Exosphere learns this by trying. A connection to an instance's IPv6 address either works or fails
with a network error, and that is the whole test; no third party is asked, and nothing is
remembered between sessions. A user who moves between an IPv6 network and an IPv4-only one gets
the right answer on their next visit.

-}


type Ipv6Reachability
    = Unknown
    | Reachable
    | Unreachable
