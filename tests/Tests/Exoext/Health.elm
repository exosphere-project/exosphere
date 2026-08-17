module Tests.Exoext.Health exposing (chromeSuite, detailSuite, dimmingSuite, discoverySuite, readSuite, reloadSuite, stateSuite)

{-| The `exoext.v1.health.*` read path and the chrome drawn from it.

The chrome is pinned by its rendered text rather than by its structure, because the whole point of
this surface is what it SAYS about a publisher that has not managed to say anything for itself: the
sentences are the feature, and a publisher's own `.detail` string reaching the screen unedited is
the one thing that made the incident this exists for diagnosable.

-}

import Element
import Exoext.Health as Health
import Expect
import Html.Attributes
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Style.Types as ST
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time


palette : ST.ExoPalette
palette =
    SH.toExoPalette ST.defaultColors { theme = ST.System, systemPreference = Nothing }


metadata : List ( String, String ) -> List OSTypes.MetadataItem
metadata pairs =
    pairs |> List.map (\( key, value ) -> { key = key, value = value })


{-| A health record straight off the wire. `Nothing` would mean the fixture itself was wrong, so
the tests unwrap through an empty record rather than threading a Maybe through every assertion.
-}
health : List ( String, String ) -> Health.Health
health pairs =
    Health.read (metadata pairs)
        |> Maybe.withDefault { seq = Nothing, checks = [], version = Nothing }


{-| Every check reporting ok, stamped at a fixed epoch second.
-}
allOkPairs : List ( String, String )
allOkPairs =
    [ ( "exoext.v1.health.seq", "1000000" )
    , ( "exoext.v1.health.objectstore", "ok" )
    , ( "exoext.v1.health.webports", "ok" )
    , ( "exoext.v1.health.fetch", "ok" )
    , ( "exoext.v1.health.network", "ok" )
    , ( "exoext.v1.health.tls", "ok" )
    , ( "exoext.v1.health.stack", "ok" )
    , ( "exoext.v1.health.bridge", "ok" )
    , ( "exoext.v1.health.store", "ok" )
    ]


{-| Ten seconds after the fixtures' `seq`, i.e. fresh.
-}
now : Time.Posix
now =
    Time.millisToPosix (1000000 * 1000 + 10 * 1000)


statusOf : Health.CheckId -> Health.Health -> Maybe Health.Status
statusOf id record =
    record.checks |> List.filter (\check -> check.id == id) |> List.head |> Maybe.map .status


detailOf : Health.CheckId -> Health.Health -> Maybe String
detailOf id record =
    record.checks |> List.filter (\check -> check.id == id) |> List.head |> Maybe.andThen .detail


render : Element.Element () -> Query.Single ()
render element =
    element |> Element.layout [] |> Query.fromHtml


{-| The host chrome for a publisher whose server Nova reports as running, with nothing in flight and
the reload control wired to a stand-in message. Override with record update.
-}
chrome : Health.Chrome ()
chrome =
    { sourceNoun = "instance"
    , running = True
    , checking = False
    , onReload = Just ()
    }


{-| The same chrome with the one thing the host can corroborate: the publishing server is not
running.
-}
stoppedChrome : Health.Chrome ()
stoppedChrome =
    { chrome | running = False }


readSuite : Test
readSuite =
    describe "reading the exoext.v1.health.* keys"
        [ test "no health key at all is not a health signal" <|
            \_ ->
                Expect.equal Nothing
                    (Health.read (metadata [ ( "exoext.v1.kind", "cloudshield" ) ]))
        , test "one health key is enough — a VM that failed before publishing a sentinel still reports" <|
            \_ ->
                Expect.notEqual Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.objectstore", "fail" ) ]))
        , test "seq is read as unix epoch SECONDS" <|
            \_ ->
                Expect.equal (Just (Time.millisToPosix 1000000000))
                    (health allOkPairs).seq
        , test "a fresher exoext.v1.published stamp dates the record (the bridge refreshes it every 5 min, health.seq only on change)" <|
            \_ ->
                -- health.seq = 1000000 s = 1970-01-12T13:46:40Z; published is 20 min later.
                Expect.equal (Just (Time.millisToPosix ((1000000 + 1200) * 1000)))
                    (health (( "exoext.v1.published", "1970-01-12T14:06:40Z" ) :: allOkPairs)).seq
        , test "an older published stamp does not push the record back in time" <|
            \_ ->
                Expect.equal (Just (Time.millisToPosix 1000000000))
                    (health (( "exoext.v1.published", "1970-01-12T13:00:00Z" ) :: allOkPairs)).seq
        , test "published alone dates a record that has no health.seq" <|
            \_ ->
                Expect.equal (Just (Time.millisToPosix ((1000000 + 1200) * 1000)))
                    (Health.read (metadata [ ( "exoext.v1.health.bridge", "ok" ), ( "exoext.v1.published", "1970-01-12T14:06:40Z" ) ])
                        |> Maybe.andThen .seq
                    )
        , test "every check is always present, in boot order" <|
            \_ ->
                Expect.equal
                    [ Health.ObjectStore, Health.WebPorts, Health.Fetch, Health.Network, Health.Tls, Health.Stack, Health.Bridge, Health.Store ]
                    ((health allOkPairs).checks |> List.map .id)
        , test "the two checks the publisher added since the first release are read, not ignored" <|
            \_ ->
                -- `network` and `fetch` were written by the bridge for two releases before this
                -- host knew their names, and an unknown name is silently pending — so a VM that
                -- had failed to download its software reported a wholly green strip.
                Expect.equal ( Just Health.CheckFail, Just Health.CheckWarn )
                    ( statusOf Health.Fetch (health [ ( "exoext.v1.health.fetch", "fail" ) ])
                    , statusOf Health.Network (health [ ( "exoext.v1.health.network", "warn" ) ])
                    )
        , test "an unreported check is pending, not ok" <|
            \_ ->
                Expect.equal (Just Health.CheckPending)
                    (statusOf Health.Bridge (health [ ( "exoext.v1.health.objectstore", "ok" ) ]))
        , test "an unrecognized value is pending too (fail-quiet, never invented as ok)" <|
            \_ ->
                Expect.equal (Just Health.CheckPending)
                    (statusOf Health.Tls
                        (health
                            [ ( "exoext.v1.health.objectstore", "ok" )
                            , ( "exoext.v1.health.tls", "probably fine" )
                            ]
                        )
                    )
        , test "warn and fail are read as themselves" <|
            \_ ->
                Expect.equal ( Just Health.CheckWarn, Just Health.CheckFail )
                    (let
                        record =
                            health
                                [ ( "exoext.v1.health.store", "warn" )
                                , ( "exoext.v1.health.objectstore", "fail" )
                                ]
                     in
                     ( statusOf Health.Store record, statusOf Health.ObjectStore record )
                    )
        , test "a check's .detail rides along with it" <|
            \_ ->
                Expect.equal (Just (Just "HTTP 409"))
                    (health
                        [ ( "exoext.v1.health.objectstore", "fail" )
                        , ( "exoext.v1.health.objectstore.detail", "HTTP 409" )
                        ]
                        |> .checks
                        |> List.filter (\check -> check.id == Health.ObjectStore)
                        |> List.head
                        |> Maybe.map .detail
                    )
        , test "the version key is read when present" <|
            \_ ->
                Expect.equal (Just "v0.2.5")
                    (health (( "exoext.v1.version", "v0.2.5" ) :: allOkPairs)).version
        , test "no version key means no version, never a placeholder" <|
            \_ ->
                Expect.equal Nothing (health allOkPairs).version
        ]


{-| `read` doubles as a discovery signal — a `Just` opens extension chrome on the instance page —
so what does and does not count as one is a gate, not a parsing detail.
-}
discoverySuite : Test
discoverySuite =
    describe "what counts as a health signal at all"
        [ test "an orphan .detail with no check value is not a signal" <|
            \_ ->
                Expect.equal Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.objectstore.detail", "HTTP 409" ) ]))
        , test "an unknown check name is not a signal" <|
            \_ ->
                Expect.equal Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.quantumflux", "ok" ) ]))
        , test "a misspelled known check is not a signal" <|
            \_ ->
                Expect.equal Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.objectstor", "ok" ) ]))
        , test "a known check with an unrecognized value is not a signal on its own" <|
            \_ ->
                Expect.equal Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.tls", "probably fine" ) ]))
        , test "one recognized check value IS a signal — the whole point is reporting before a sentinel" <|
            \_ ->
                Expect.notEqual Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.objectstore", "fail" ) ]))
        , test "a parseable seq alone IS a signal — the publisher is speaking, it just has nothing yet" <|
            \_ ->
                Expect.notEqual Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.seq", "1000000" ) ]))
        , test "an unparseable seq alone is not a signal" <|
            \_ ->
                Expect.equal Nothing
                    (Health.read (metadata [ ( "exoext.v1.health.seq", "yesterday" ) ]))
        ]


{-| The `.detail` string is the only publisher-authored text Exosphere draws inside its own trust
chrome, so it is normalized as hostile input rather than trusted as a message.
-}
detailSuite : Test
detailSuite =
    describe "normalizing a publisher's .detail before it reaches the screen"
        [ test "control characters and newlines collapse to single spaces" <|
            \_ ->
                Expect.equal (Just "HTTP 409 retrying")
                    (detailOf Health.ObjectStore
                        (health
                            [ ( "exoext.v1.health.objectstore", "fail" )
                            , ( "exoext.v1.health.objectstore.detail", "HTTP 409\n\n\tretrying\u{0000}" )
                            ]
                        )
                    )
        , test "bidi overrides and zero-width characters are removed outright" <|
            \_ ->
                Expect.equal (Just "gnitrats")
                    (detailOf Health.ObjectStore
                        (health
                            [ ( "exoext.v1.health.objectstore", "fail" )
                            , ( "exoext.v1.health.objectstore.detail", "\u{202E}gnitrats\u{202C}\u{200B}" )
                            ]
                        )
                    )
        , test "a detail that normalizes away entirely is no detail at all" <|
            \_ ->
                Expect.equal Nothing
                    (detailOf Health.ObjectStore
                        (health
                            [ ( "exoext.v1.health.objectstore", "fail" )
                            , ( "exoext.v1.health.objectstore.detail", "\u{202E}\u{200B}\t\n " )
                            ]
                        )
                    )
        , test "the 255-char cap is applied AFTER normalization, so padding can't push content out" <|
            \_ ->
                Expect.equal (Just 255)
                    (detailOf Health.ObjectStore
                        (health
                            [ ( "exoext.v1.health.objectstore", "fail" )
                            , ( "exoext.v1.health.objectstore.detail"
                              , String.repeat 40 "\u{202E}" ++ String.repeat 1000 "A"
                              )
                            ]
                        )
                        |> Maybe.map String.length
                    )
        , test "a hostile detail reaches the screen declawed, inside its bounded box" <|
            \_ ->
                let
                    hostile =
                        "\u{202E}"
                            ++ "Everything is fine"
                            ++ "\u{0000}\u{0007}\n"
                            ++ String.repeat 1000 "A"
                in
                render
                    (Health.placeholder palette
                        now
                        chrome
                        (health
                            [ ( "exoext.v1.health.seq", "1000000" )
                            , ( "exoext.v1.health.objectstore", "fail" )
                            , ( "exoext.v1.health.objectstore.detail", hostile )
                            ]
                        )
                    )
                    |> Expect.all
                        [ -- no raw override survives into the DOM
                          Query.hasNot [ Selector.text "\u{202E}" ]

                        -- and what does survive is the capped, collapsed, still-readable text
                        , Query.has [ Selector.text ("objectstore: Everything is fine " ++ String.repeat 236 "A") ]
                        ]
        ]


stateSuite : Test
stateSuite =
    describe "the derived overall state and staleness"
        [ test "every check ok is healthy" <|
            \_ ->
                Expect.equal Health.AllHealthy (Health.overall (health allOkPairs))
        , test "an unreported check reads as still starting" <|
            \_ ->
                Expect.equal Health.StillStarting
                    (Health.overall (health [ ( "exoext.v1.health.objectstore", "ok" ) ]))
        , test "a warn outranks a pending" <|
            \_ ->
                Expect.equal Health.SomeDegraded
                    (Health.overall (health [ ( "exoext.v1.health.store", "warn" ) ]))
        , test "a fail outranks everything" <|
            \_ ->
                Expect.equal Health.SomeFailing
                    (Health.overall
                        (health
                            [ ( "exoext.v1.health.store", "warn" )
                            , ( "exoext.v1.health.objectstore", "fail" )
                            ]
                        )
                    )
        , test "a health record written just now is not stale" <|
            \_ ->
                Expect.equal False (Health.isStale now (health allOkPairs))
        , test "one second past the threshold is stale" <|
            \_ ->
                Expect.equal True
                    (Health.isStale
                        (Time.millisToPosix (1000000 * 1000 + Health.staleAfterMillis + 1000))
                        (health allOkPairs)
                    )
        , test "a record with NO seq fails closed — undatable values are never presented as current" <|
            \_ ->
                Expect.equal True
                    (Health.isStale now (health [ ( "exoext.v1.health.objectstore", "ok" ) ]))
        , test "an unparseable seq fails closed the same way" <|
            \_ ->
                Expect.equal True
                    (Health.isStale now
                        (health
                            [ ( "exoext.v1.health.seq", "yesterday" )
                            , ( "exoext.v1.health.objectstore", "ok" )
                            ]
                        )
                    )
        , test "a seq slightly ahead of the browser clock is ordinary skew, not staleness" <|
            \_ ->
                Expect.equal False
                    (Health.isStale
                        (Time.millisToPosix (1000000 * 1000 - 60 * 1000))
                        (health allOkPairs)
                    )
        , test "a seq implausibly far in the future is undatable, so it fails closed" <|
            \_ ->
                Expect.equal True
                    (Health.isStale
                        (Time.millisToPosix (1000000 * 1000 - 60 * 60 * 1000))
                        (health allOkPairs)
                    )
        , test "the threshold is three missed five-minute envelope refreshes, not two" <|
            \_ ->
                Expect.equal (3 * 5 * 60 * 1000) Health.staleAfterMillis
        , test "twelve minutes of silence is a slow publisher, not a dead one" <|
            \_ ->
                -- Two missed refreshes plus jitter. Under the old ten-minute threshold this was
                -- stale, and it greyed out extensions that were working.
                Expect.equal False
                    (Health.isStale
                        (Time.millisToPosix (1000000 * 1000 + 12 * 60 * 1000))
                        (health allOkPairs)
                    )
        , test "sixteen minutes of silence is stale" <|
            \_ ->
                Expect.equal True
                    (Health.isStale
                        (Time.millisToPosix (1000000 * 1000 + 16 * 60 * 1000))
                        (health allOkPairs)
                    )
        ]


{-| The dimming rule: staleness proposes, the host's Nova status disposes.
-}
dimmingSuite : Test
dimmingSuite =
    let
        longSilence =
            Time.millisToPosix (1000000 * 1000 + 40 * 60 * 1000)
    in
    describe "when the chrome dims the values"
        [ test "a stale record from a RUNNING server is not dimmed — silence alone is not evidence" <|
            \_ ->
                Expect.equal False (Health.dimmed longSilence chrome (health allOkPairs))
        , test "a stale record from a server the host knows is not running IS dimmed" <|
            \_ ->
                Expect.equal True (Health.dimmed longSilence stoppedChrome (health allOkPairs))
        , test "a fresh record from a stopped server is not dimmed: the values really are current" <|
            \_ ->
                -- The instance stopped seconds ago and the last write is still inside the window.
                -- Nothing on screen is out of date yet, so nothing is greyed out yet.
                Expect.equal False (Health.dimmed now stoppedChrome (health allOkPairs))
        , test "an undated record from a running server is not dimmed, but is still called stale" <|
            \_ ->
                -- The two decisions stay separate: the sentence always fires, the dimming does not.
                Expect.equal ( True, False )
                    ( Health.isStale now (health (allOkPairs |> List.drop 1))
                    , Health.dimmed now chrome (health (allOkPairs |> List.drop 1))
                    )
        ]


chromeSuite : Test
chromeSuite =
    describe "the chrome drawn from the health keys"
        [ test "a healthy strip is one quiet line" <|
            \_ ->
                render (Health.strip palette now chrome (health allOkPairs))
                    |> Query.has [ Selector.text "Extension healthy" ]
        , test "a healthy strip draws no per-check chips — a row of chips for no information" <|
            \_ ->
                render (Health.strip palette now chrome (health allOkPairs))
                    |> Query.hasNot [ Selector.text "storage" ]
        , test "a degraded results store says what is reduced, not that something broke" <|
            \_ ->
                render
                    (Health.strip palette
                        now
                        chrome
                        (health (allOkPairs ++ [ ( "exoext.v1.health.store", "warn" ) ]))
                    )
                    |> Query.has [ Selector.text "Limited storage — results are using the fallback transport" ]
        , test "a non-ok check expands the strip to the per-check chips" <|
            \_ ->
                render
                    (Health.strip palette
                        now
                        chrome
                        (health (allOkPairs ++ [ ( "exoext.v1.health.store", "warn" ) ]))
                    )
                    |> Expect.all
                        [ Query.has [ Selector.text "storage" ]
                        , Query.has [ Selector.text "results" ]
                        ]
        , test "the publisher version rides at the end of the expanded chip row" <|
            \_ ->
                render
                    (Health.strip palette
                        now
                        chrome
                        (health
                            (allOkPairs
                                ++ [ ( "exoext.v1.health.store", "warn" )
                                   , ( "exoext.v1.version", "v0.2.5" )
                                   ]
                            )
                        )
                    )
                    |> Query.has [ Selector.text "v0.2.5" ]
        , test "staleness rewrites the strip's sentence to when it last spoke" <|
            \_ ->
                render
                    (Health.strip palette
                        (Time.millisToPosix (1000000 * 1000 + 23 * 60 * 1000))
                        chrome
                        (health allOkPairs)
                    )
                    |> Expect.all
                        [ Query.has [ Selector.text "Last heard from this extension 23 minutes ago — values may be out of date" ]
                        , Query.hasNot [ Selector.text "Extension healthy" ]
                        ]
        , test "with no manifest and nothing failing, the placeholder is a boot checklist" <|
            \_ ->
                render
                    (Health.placeholder palette
                        now
                        chrome
                        (health
                            [ ( "exoext.v1.health.seq", "1000000" )
                            , ( "exoext.v1.health.objectstore", "ok" )
                            , ( "exoext.v1.health.webports", "ok" )
                            ]
                        )
                    )
                    |> Expect.all
                        [ Query.has [ Selector.text "Extension starting…" ]
                        , Query.has [ Selector.text "Object storage reachable" ]
                        , Query.has [ Selector.text "Results store ready" ]
                        , Query.has [ Selector.text "2 of 8 checks passed" ]
                        ]
        , test "all checks passing with no manifest says what it is actually waiting for" <|
            \_ ->
                render (Health.placeholder palette now chrome (health allOkPairs))
                    |> Expect.all
                        [ Query.has [ Selector.text "Extension interface loading…" ]
                        , Query.has [ Selector.text "All checks passed — waiting for the extension's interface" ]

                        -- neither of the two sentences that contradict this state
                        , Query.hasNot [ Selector.text "Extension starting…" ]
                        , Query.hasNot [ Selector.text "Extension healthy" ]
                        ]
        , test "an undated record is never presented as current, whatever its checks say" <|
            \_ ->
                render
                    (Health.strip palette
                        now
                        chrome
                        (health (allOkPairs |> List.drop 1))
                    )
                    |> Expect.all
                        [ Query.has [ Selector.text "This extension did not report when it last checked — values may be out of date" ]
                        , Query.hasNot [ Selector.text "Extension healthy" ]
                        ]
        , test "a publisher whose clock runs slightly fast reads as current, never as reporting from the future" <|
            \_ ->
                render
                    (Health.strip palette
                        (Time.millisToPosix (1000000 * 1000 - 60 * 1000))
                        chrome
                        (health allOkPairs)
                    )
                    |> Expect.all
                        [ Query.has [ Selector.text "Extension healthy" ]
                        , Query.has [ Selector.text "checked right now" ]
                        ]
        , test "with no manifest and a failing check, the placeholder is the failure piece" <|
            \_ ->
                render
                    (Health.placeholder palette
                        now
                        chrome
                        (health
                            [ ( "exoext.v1.health.seq", "1000000" )
                            , ( "exoext.v1.health.objectstore", "fail" )
                            , ( "exoext.v1.health.objectstore.detail", "HTTP 409" )
                            , ( "exoext.v1.health.store", "fail" )
                            , ( "exoext.v1.health.store.detail", "no results container" )
                            ]
                        )
                    )
                    |> Expect.all
                        [ Query.has [ Selector.text "Extension failed to start" ]
                        , Query.has [ Selector.text "objectstore: HTTP 409" ]
                        , Query.has [ Selector.text "store: no results container" ]
                        , Query.has [ Selector.text "2 of 8 checks failing" ]
                        , Query.has [ Selector.text "The extension's instance reported an error during startup." ]
                        ]
        , test "a stale record from a stopped server says WHY it went quiet, not how long ago" <|
            \_ ->
                render
                    (Health.strip palette
                        (Time.millisToPosix (1000000 * 1000 + 40 * 60 * 1000))
                        stoppedChrome
                        (health allOkPairs)
                    )
                    |> Expect.all
                        [ Query.has [ Selector.text "This instance is not running — values are from before it stopped" ]
                        , Query.hasNot [ Selector.text "Last heard from this extension" ]
                        , Query.hasNot [ Selector.text "Extension healthy" ]
                        ]
        , test "the not-running sentence speaks the deployer's own noun too" <|
            \_ ->
                render
                    (Health.strip palette
                        (Time.millisToPosix (1000000 * 1000 + 40 * 60 * 1000))
                        { stoppedChrome | sourceNoun = "cloud server" }
                        (health allOkPairs)
                    )
                    |> Query.has [ Selector.text "This cloud server is not running — values are from before it stopped" ]
        , test "a stopped server whose record is still fresh keeps the ordinary sentence" <|
            \_ ->
                render (Health.strip palette now stoppedChrome (health allOkPairs))
                    |> Expect.all
                        [ Query.has [ Selector.text "Extension healthy" ]
                        , Query.hasNot [ Selector.text "is not running" ]
                        ]
        , test "the placeholder's failure piece carries the not-running sentence as well" <|
            \_ ->
                render
                    (Health.placeholder palette
                        (Time.millisToPosix (1000000 * 1000 + 40 * 60 * 1000))
                        stoppedChrome
                        (health
                            [ ( "exoext.v1.health.seq", "1000000" )
                            , ( "exoext.v1.health.objectstore", "fail" )
                            ]
                        )
                    )
                    |> Query.has [ Selector.text "This instance is not running — values are from before it stopped" ]
        , test "the failure piece speaks the deployer's own noun for a server" <|
            \_ ->
                render
                    (Health.placeholder palette
                        now
                        { chrome | sourceNoun = "cloud server" }
                        (health
                            [ ( "exoext.v1.health.seq", "1000000" )
                            , ( "exoext.v1.health.objectstore", "fail" )
                            ]
                        )
                    )
                    |> Query.has [ Selector.text "The extension's cloud server reported an error during startup." ]
        ]


{-| The "Check now" control. It is host chrome for the host's own reads, so the only thing under
test here is that it is offered whenever there is something to press and that pressing it says so.
-}
reloadSuite : Test
reloadSuite =
    let
        checkNow =
            Selector.attribute (Html.Attributes.attribute "aria-label" "Check now")
    in
    describe "the reload control on the strip line"
        [ test "a healthy, entirely unremarkable strip still offers it" <|
            \_ ->
                -- The point of the decision: it is not a stale-only affordance. A control that only
                -- appears once something is wrong teaches people it is an error button.
                render (Health.strip palette now chrome (health allOkPairs))
                    |> Query.has [ checkNow ]
        , test "a stale strip offers it too" <|
            \_ ->
                render
                    (Health.strip palette
                        (Time.millisToPosix (1000000 * 1000 + 40 * 60 * 1000))
                        chrome
                        (health allOkPairs)
                    )
                    |> Query.has [ checkNow ]
        , test "the no-manifest placeholder offers it, which is where waiting is most likely" <|
            \_ ->
                render (Health.placeholder palette now chrome (health allOkPairs))
                    |> Query.has [ checkNow ]
        , test "pressing it emits the host's message" <|
            \_ ->
                render (Health.strip palette now chrome (health allOkPairs))
                    |> Query.find [ checkNow ]
                    |> Event.simulate Event.click
                    |> Event.expect ()
        , test "a caller with nothing to refresh hides the control entirely" <|
            \_ ->
                render (Health.strip palette now { chrome | onReload = Nothing } (health allOkPairs))
                    |> Query.hasNot [ checkNow ]
        , test "while the host's refresh is in flight the press is disabled" <|
            \_ ->
                -- The spinner replaces the icon and the button stops accepting presses, so a second
                -- click cannot queue a second read behind the one already running.
                render (Health.strip palette now { chrome | checking = True } (health allOkPairs))
                    |> Query.find [ checkNow ]
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.err
        ]
