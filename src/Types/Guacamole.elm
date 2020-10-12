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
    , deployComplete : Bool
    , authToken : GuacamoleTokenRDPP
    }


type alias GuacamoleTokenRDPP =
    RDPP.RemoteDataPlusPlus Http.Error GuacamoleAuthToken


type alias GuacamoleAuthToken =
    String
