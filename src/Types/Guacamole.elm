module Types.Guacamole exposing
    ( GuacamoleAuthToken
    , GuacamoleTokenRDPP
    , LaunchedWithGuacProps
    , ServerGuacamoleStatus(..)
    )

import Helpers.RemoteDataPlusPlus as RDPP
import Http


type ServerGuacamoleStatus
    = NotLaunchedWithGuacamole
    | LaunchedWithGuacamole LaunchedWithGuacProps


type alias LaunchedWithGuacProps =
    { sshSupported : Bool
    , vncSupported : Bool
    , tlsSupported : Bool
    , authToken : GuacamoleTokenRDPP

    -- How many token requests in a row have failed to reach the instance's IPv6 address. Enough of
    -- them, on an instance with no floating IP address, means the browser's network has no IPv6.
    , consecutiveIpv6NetworkErrors : Int
    }


type alias GuacamoleTokenRDPP =
    RDPP.RemoteDataPlusPlus Http.Error GuacamoleAuthToken


type alias GuacamoleAuthToken =
    String
