module Tests.Rest.Swift exposing (cacheBusterSuite)

import Expect
import Rest.Swift
import Test exposing (Test, describe, test)
import Time


cacheBusterSuite : Test
cacheBusterSuite =
    describe "Rest.Swift.cacheBuster"
        [ test "same millis with different nonce yields different strings" <|
            \_ ->
                Expect.notEqual
                    (Rest.Swift.cacheBuster (Time.millisToPosix 1000) 1)
                    (Rest.Swift.cacheBuster (Time.millisToPosix 1000) 2)
        , test "different millis with same nonce yields different strings" <|
            \_ ->
                Expect.notEqual
                    (Rest.Swift.cacheBuster (Time.millisToPosix 1000) 1)
                    (Rest.Swift.cacheBuster (Time.millisToPosix 2000) 1)
        ]
