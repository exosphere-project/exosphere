module Tests.Helpers.Url exposing (sameHostSuite)

import Expect
import Helpers.Url
import Test exposing (Test, describe, test)


sameHostSuite : Test
sameHostSuite =
    describe "deciding whether two URLs name the same host"
        [ test "the same URL twice" <|
            \() ->
                Helpers.Url.sameHost
                    "https://cell.alliance.rebel:5000/v3"
                    "https://cell.alliance.rebel:5000/v3"
                    |> Expect.equal True
        , test "a login URL and the catalog URL for the same cloud, differing in port and path" <|
            \() ->
                Helpers.Url.sameHost
                    "https://cell.alliance.rebel:5000/v3"
                    "https://cell.alliance.rebel/identity/v3/"
                    |> Expect.equal True
        , test "two clouds that hand out the same project UUID" <|
            \() ->
                Helpers.Url.sameHost
                    "https://cell.alliance.rebel:5000/v3"
                    "https://rogue.squadron:5000/v3"
                    |> Expect.equal False
        , test "a host is not a subdomain of itself" <|
            \() ->
                Helpers.Url.sameHost
                    "https://alliance.rebel:5000/v3"
                    "https://cell.alliance.rebel:5000/v3"
                    |> Expect.equal False
        , test "hosts written in different cases, which DNS does not distinguish" <|
            \() ->
                Helpers.Url.sameHost
                    "https://Cell.Alliance.Rebel:5000/v3"
                    "https://cell.alliance.rebel/identity/v3"
                    |> Expect.equal True
        , test "an IPv6 literal, which does not parse, matches itself written the same way" <|
            \() ->
                Helpers.Url.sameHost
                    "https://[2001:db8::1]:5000/v3"
                    "https://[2001:db8::1]:5000/v3"
                    |> Expect.equal True
        , test "an IPv6 literal written two ways falls back to comparing the whole string" <|
            \() ->
                Helpers.Url.sameHost
                    "https://[2001:db8::1]:5000/v3"
                    "https://[2001:db8::1]/identity/v3"
                    |> Expect.equal False
        , test "an unparseable URL matches only itself" <|
            \() ->
                Helpers.Url.sameHost "cell.alliance.rebel" "cell.alliance.rebel"
                    |> Expect.equal True
        , test "an unparseable URL does not match a parseable one" <|
            \() ->
                Helpers.Url.sameHost "cell.alliance.rebel" "https://cell.alliance.rebel:5000/v3"
                    |> Expect.equal False
        ]
