module Tests.Helpers.Interaction exposing (ipv6NeedsFloatingIpSuite)

import Expect
import Helpers.GuacamoleEndpoint exposing (Instance)
import Helpers.Interaction exposing (ipv6NeedsFloatingIp)
import Helpers.RemoteDataPlusPlus as RDPP
import Test exposing (Test, describe, test)
import Time
import Types.Guacamole exposing (LaunchedWithGuacProps, ServerGuacamoleStatus(..))
import Types.Ipv6Reachability exposing (Ipv6Reachability(..))
import Types.Server exposing (ExoSetupStatus(..), ServerFromExoProps)
import Types.Workflow exposing (ServerCustomWorkflowStatus(..))


{-| An instance serving Guacamole itself, reachable only over IPv6, on a browser that has not
learned anything about its own network yet.
-}
ipv6OnlyInstance : Instance
ipv6OnlyInstance =
    { tlsSupported = True
    , userAppProxyHostname = Nothing
    , floatingIpAddress = Nothing
    , fixedIpAddresses = [ "192.168.1.20", "2001:db8::1" ]
    , ipv6Reachability = Unknown
    }


{-| An instance whose own setup has finished, so anything failing after this is the network.
-}
setUpExoProps : ServerFromExoProps
setUpExoProps =
    { exoServerVersion = 8
    , exoSetupStatus =
        RDPP.RemoteDataPlusPlus
            (RDPP.DoHave ( ExoSetupComplete, Nothing ) (Time.millisToPosix 0))
            (RDPP.NotLoading Nothing)
    , resourceUsage = RDPP.empty
    , guacamoleStatus = NotLaunchedWithGuacamole
    , customWorkflowStatus = NotLaunchedWithCustomWorkflow
    , exoCreatorUsername = Nothing
    }


{-| Guacamole props that have failed to reach IPv6 often enough to say so.
-}
failingGuacProps : LaunchedWithGuacProps
failingGuacProps =
    { sshSupported = True
    , vncSupported = True
    , tlsSupported = True
    , authToken = RDPP.empty
    , consecutiveIpv6NetworkErrors = 3
    }


ipv6NeedsFloatingIpSuite : Test
ipv6NeedsFloatingIpSuite =
    describe "Offering a floating IP address to a browser that cannot reach IPv6"
        [ test "Offers it after enough failures on an instance that only has IPv6" <|
            \() ->
                ipv6NeedsFloatingIp ipv6OnlyInstance setUpExoProps failingGuacProps
                    |> Expect.equal True
        , test "Stays quiet once something else in the session has answered over IPv6" <|
            \() ->
                ipv6NeedsFloatingIp
                    { ipv6OnlyInstance | ipv6Reachability = Reachable }
                    setUpExoProps
                    failingGuacProps
                    |> Expect.equal False
        , test "Still offers it when the session has already found IPv6 unreachable" <|
            \() ->
                ipv6NeedsFloatingIp
                    { ipv6OnlyInstance | ipv6Reachability = Unreachable }
                    setUpExoProps
                    failingGuacProps
                    |> Expect.equal True
        , test "Says nothing until the instance's own setup has finished" <|
            \() ->
                ipv6NeedsFloatingIp
                    ipv6OnlyInstance
                    { setUpExoProps | exoSetupStatus = RDPP.empty }
                    failingGuacProps
                    |> Expect.equal False
        , test "Says nothing before enough failures in a row" <|
            \() ->
                ipv6NeedsFloatingIp
                    ipv6OnlyInstance
                    setUpExoProps
                    { failingGuacProps | consecutiveIpv6NetworkErrors = 2 }
                    |> Expect.equal False
        ]
