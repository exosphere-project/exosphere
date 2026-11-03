module Tests.Helpers.Interaction exposing (guacTokenErrorStatusSuite, setupDoneOverIpv6Suite)

import Expect
import Helpers.Interaction
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Test exposing (Test, describe, test)
import Time
import Types.Defaults
import Types.Interaction as ITypes
import Types.Server exposing (ExoSetupStatus(..))


guacTokenErrorStatusSuite : Test
guacTokenErrorStatusSuite =
    describe "showing a failed Guacamole token request"
        [ test "without a floating IP, the proxy could not reach the instance over IPv6" <|
            \() ->
                Helpers.Interaction.guacTokenErrorStatus Types.Defaults.localization "2001:18e8:c02:110:0:1a::5" Http.Timeout
                    |> Expect.equal
                        (ITypes.Unavailable
                            "Instance does not have a floating IP address, and the proxy could not reach it over IPv6. Assign a floating IP address, or allow port 49528 over IPv6 in its firewall ruleset."
                        )
        , test "with a floating IP, the error is shown as it was" <|
            \() ->
                Helpers.Interaction.guacTokenErrorStatus Types.Defaults.localization "149.165.153.239" (Http.BadStatus 500)
                    |> Expect.equal
                        (ITypes.Error
                            "Exosphere tried to authenticate to the Guacamole API, and received this error: BadStatus: 500"
                        )
        ]


setupDoneOverIpv6Suite : Test
setupDoneOverIpv6Suite =
    let
        complete =
            RDPP.DoHave ( ExoSetupComplete, Nothing ) (Time.millisToPosix 0)

        running =
            RDPP.DoHave ( ExoSetupRunning, Nothing ) (Time.millisToPosix 0)
    in
    describe "whether a failed token over IPv6 is worth waiting out"
        [ test "not once setup is complete" <|
            \() ->
                Helpers.Interaction.setupDoneOverIpv6 (Just "2001:18e8:c02:110:0:1a::5") complete
                    |> Expect.equal True
        , test "yes while setup is still running" <|
            \() ->
                Helpers.Interaction.setupDoneOverIpv6 (Just "2001:18e8:c02:110:0:1a::5") running
                    |> Expect.equal False
        , test "a floating IP keeps waiting as before" <|
            \() ->
                Helpers.Interaction.setupDoneOverIpv6 (Just "149.165.153.239") complete
                    |> Expect.equal False
        ]
