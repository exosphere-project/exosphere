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


{-| An instance serving Guacamole itself, reachable only over IPv6, on a browser that has been
found not to have IPv6.
-}
unreachableIpv6Instance : Instance
unreachableIpv6Instance =
    { tlsSupported = True
    , userAppProxyHostname = Nothing
    , floatingIpAddress = Nothing
    , fixedIpAddresses = [ "192.168.1.20", "2001:db8::1" ]
    , ipv6Reachability = Unreachable
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


{-| Guacamole props for an instance that has never got a token.
-}
tokenlessGuacProps : LaunchedWithGuacProps
tokenlessGuacProps =
    { sshSupported = True
    , vncSupported = True
    , tlsSupported = True
    , authToken = RDPP.empty
    }


ipv6NeedsFloatingIpSuite : Test
ipv6NeedsFloatingIpSuite =
    describe "Offering a floating IP address to a browser that cannot reach IPv6"
        [ test "Offers it on an instance that only has IPv6" <|
            \() ->
                ipv6NeedsFloatingIp unreachableIpv6Instance setUpExoProps tokenlessGuacProps
                    |> Expect.equal True
        , test "Stays quiet while nothing is known about this browser's IPv6" <|
            \() ->
                ipv6NeedsFloatingIp
                    { unreachableIpv6Instance | ipv6Reachability = Unknown }
                    setUpExoProps
                    tokenlessGuacProps
                    |> Expect.equal False
        , test "Stays quiet once something else in the session has answered over IPv6" <|
            \() ->
                ipv6NeedsFloatingIp
                    { unreachableIpv6Instance | ipv6Reachability = Reachable }
                    setUpExoProps
                    tokenlessGuacProps
                    |> Expect.equal False
        , test "Says nothing until the instance's own setup has finished" <|
            \() ->
                ipv6NeedsFloatingIp
                    unreachableIpv6Instance
                    { setUpExoProps | exoSetupStatus = RDPP.empty }
                    tokenlessGuacProps
                    |> Expect.equal False
        , test "Says nothing about an instance that is answering anyway" <|
            \() ->
                ipv6NeedsFloatingIp
                    unreachableIpv6Instance
                    setUpExoProps
                    { tokenlessGuacProps
                        | authToken =
                            RDPP.RemoteDataPlusPlus
                                (RDPP.DoHave "token" (Time.millisToPosix 0))
                                (RDPP.NotLoading Nothing)
                    }
                    |> Expect.equal False
        , test "Says nothing about an instance that has a floating IP address already" <|
            \() ->
                ipv6NeedsFloatingIp
                    { unreachableIpv6Instance | floatingIpAddress = Just "10.0.0.5" }
                    setUpExoProps
                    tokenlessGuacProps
                    |> Expect.equal False
        ]
