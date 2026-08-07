module Tests.Exoext.Transport exposing
    ( capBodySuite
    , historyRefreshKeySuite
    , reqCancelSuite
    , resolveResultBodySuite
    , resultBodySuite
    , runStatusSuite
    )

{-| The generic exoext envelope: §7.1 request-slot framing, the §4.3 run status, the cancel
channel, the §5.5 caps and the result-pointer plumbing. Nothing here knows what a request is for;
the payloads that fill it are covered by `Tests.CloudShield.Wire`.
-}

import Exoext.Transport as Transport
import Expect
import Test exposing (Test, describe, test)


meta : List ( String, String ) -> List { key : String, value : String }
meta pairs =
    List.map (\( k, v ) -> { key = k, value = v }) pairs


capBodySuite : Test
capBodySuite =
    describe "capBody fails closed above a byte cap (phase-0-spec.md §5.5)"
        [ test "a body at exactly the cap is accepted" <|
            \_ ->
                Expect.equal (Ok "abc") (Transport.capBody 3 "abc")
        , test "a body one byte over the cap is rejected" <|
            \_ ->
                Expect.notEqual (Ok "abcd") (Transport.capBody 3 "abcd")
        , test "manifestCapBytes is 16 KB" <|
            \_ ->
                Expect.equal (16 * 1024) Transport.manifestCapBytes
        , test "resultCapBytes is 1 MB" <|
            \_ ->
                Expect.equal (1024 * 1024) Transport.resultCapBytes
        ]


runStatusSuite : Test
runStatusSuite =
    let
        requiredKeys =
            [ ( "exoext.v1.run.seq", "7" ), ( "exoext.v1.run.state", "running" ) ]

        descriptorKeys =
            [ ( "exoext.v1.run.target", "i-1" )
            , ( "exoext.v1.run.requestId", "exo-cs-req-7" )
            , ( "exoext.v1.run.batchId", "exo-cs-batch-7" )
            , ( "exoext.v1.run.phase", "cloning" )
            , ( "exoext.v1.run.pct", "40" )
            ]

        descriptors status =
            ( status.target, status.requestId, status.batchId )
    in
    describe "runStatusFromMetadata reads the §4.3 run slot, descriptors optional"
        [ test "the two required keys alone decode, with every descriptor Nothing" <|
            \_ ->
                Expect.equal
                    (Just ( ( 7, "running" ), ( Nothing, Nothing, Nothing ), ( Nothing, Nothing ) ))
                    (Transport.runStatusFromMetadata (meta requiredKeys)
                        |> Maybe.map (\s -> ( ( s.seq, s.state ), descriptors s, ( s.phase, s.pct ) ))
                    )
        , test "a publisher writing every descriptor decodes all of them" <|
            \_ ->
                Expect.equal
                    (Just ( ( Just "i-1", Just "exo-cs-req-7", Just "exo-cs-batch-7" ), ( Just "cloning", Just 40 ) ))
                    (Transport.runStatusFromMetadata (meta (requiredKeys ++ descriptorKeys))
                        |> Maybe.map (\s -> ( descriptors s, ( s.phase, s.pct ) ))
                    )
        , test "a missing state fails the read even with descriptors present" <|
            \_ ->
                Expect.equal Nothing
                    (Transport.runStatusFromMetadata (meta (( "exoext.v1.run.seq", "7" ) :: descriptorKeys)))
        , test "an unparseable seq fails the read" <|
            \_ ->
                Expect.equal Nothing
                    (Transport.runStatusFromMetadata (meta [ ( "exoext.v1.run.seq", "later" ), ( "exoext.v1.run.state", "running" ) ]))
        , test "an empty descriptor value is Nothing, never an identity that matches nothing" <|
            \_ ->
                Expect.equal (Just ( Nothing, Nothing, Nothing ))
                    (Transport.runStatusFromMetadata
                        (meta
                            (requiredKeys
                                ++ [ ( "exoext.v1.run.target", "" )
                                   , ( "exoext.v1.run.requestId", "" )
                                   , ( "exoext.v1.run.batchId", "" )
                                   ]
                            )
                        )
                        |> Maybe.map descriptors
                    )
        , test "a stringified Python None is Nothing, not a phantom target id" <|
            \_ ->
                Expect.equal (Just ( Nothing, Nothing, Nothing ))
                    (Transport.runStatusFromMetadata
                        (meta
                            (requiredKeys
                                ++ [ ( "exoext.v1.run.target", "None" )
                                   , ( "exoext.v1.run.requestId", "None" )
                                   , ( "exoext.v1.run.batchId", "None" )
                                   ]
                            )
                        )
                        |> Maybe.map descriptors
                    )
        , test "pct outside 0-100 and a non-numeric pct both read as absent" <|
            \_ ->
                Expect.equal [ Just 0, Just 100, Nothing, Nothing, Nothing ]
                    ([ "0", "100", "101", "-1", "half" ]
                        |> List.map
                            (\value ->
                                Transport.runStatusFromMetadata (meta (( "exoext.v1.run.pct", value ) :: requiredKeys))
                                    |> Maybe.andThen .pct
                            )
                    )
        ]


reqCancelSuite : Test
reqCancelSuite =
    describe "the exoext.v1.req.cancel channel"
        [ test "a cancel names the requestId to stop, and touches nothing else" <|
            \_ ->
                Expect.equal [ { key = "exoext.v1.req.cancel", value = "exo-cs-req-7" } ]
                    (Transport.reqCancelMetadata "exo-cs-req-7")
        , test "cancelling nothing writes the cleared value, which names no request" <|
            \_ ->
                Expect.equal [ { key = "exoext.v1.req.cancel", value = "" } ]
                    (Transport.reqCancelMetadata "")
        , test "writing a request slot clears the channel, so a stale stop cannot kill the new run" <|
            \_ ->
                Expect.equal (Just "")
                    (Transport.reqSlotMetadata 9 "{}"
                        |> List.filter (\item -> item.key == Transport.reqCancelKey)
                        |> List.head
                        |> Maybe.map .value
                    )
        , test "the channel reads back, which is what makes a pending stop survive a reload" <|
            \_ ->
                Expect.equal (Just "exo-cs-req-7")
                    (Transport.reqCancelFromMetadata (Transport.reqCancelMetadata "exo-cs-req-7"))
        , test "the cleared, absent and stringified-None channels all name no request" <|
            \_ ->
                -- None of the three can equal a real requestId, so none can make a live run look
                -- stopped. The cleared value is the one a superseding request write leaves behind.
                Expect.equal [ Nothing, Nothing, Nothing ]
                    ([ Transport.reqCancelMetadata ""
                     , []
                     , meta [ ( "exoext.v1.req.cancel", "None" ) ]
                     ]
                        |> List.map Transport.reqCancelFromMetadata
                    )
        ]


resolveResultBodySuite : Test
resolveResultBodySuite =
    describe "resolveResultBody distinguishes an inline result from a {\"ref\": ...} pointer"
        [ test "a plain result object resolves to InlineResult with the body untouched" <|
            \_ ->
                let
                    body =
                        """{"schemaVersion":"1.0","findings":[]}"""
                in
                Expect.equal (Transport.InlineResult body) (Transport.resolveResultBody body)
        , test "a {\"ref\": ...} pointer resolves to ResultRef with the referenced object name" <|
            \_ ->
                Expect.equal
                    (Transport.ResultRef "results/abc-123.json")
                    (Transport.resolveResultBody """{"ref":"results/abc-123.json"}""")
        , test "malformed JSON is not a ref, so it resolves to InlineResult (the renderer's own validation rejects it)" <|
            \_ ->
                Expect.equal (Transport.InlineResult "not json") (Transport.resolveResultBody "not json")
        ]


resultBodySuite : Test
resultBodySuite =
    describe "resultRefObjectName + resultBody: the two-step ref-pointer follow-up fetch"
        [ test "an inline result names no ref to follow up" <|
            \_ ->
                Expect.equal Nothing (Transport.resultRefObjectName (Transport.InlineResult "{}"))
        , test "a ref names the object to fetch next" <|
            \_ ->
                Expect.equal (Just "results/abc-123.json") (Transport.resultRefObjectName (Transport.ResultRef "results/abc-123.json"))
        , test "resultBody returns the inline body directly, ignoring the fetched-ref argument" <|
            \_ ->
                Expect.equal (Just "{}") (Transport.resultBody (Transport.InlineResult "{}") (Just "should be ignored"))
        , test "resultBody returns Nothing for a ref while its fetch is still in flight" <|
            \_ ->
                Expect.equal Nothing (Transport.resultBody (Transport.ResultRef "results/abc-123.json") Nothing)
        , test "resultBody returns the fetched body once a ref's follow-up fetch completes" <|
            \_ ->
                Expect.equal (Just "{\"findings\":[]}") (Transport.resultBody (Transport.ResultRef "results/abc-123.json") (Just "{\"findings\":[]}"))
        ]


historyRefreshKeySuite : Test
historyRefreshKeySuite =
    describe "historyRefreshKey composes etag + run.seq + run.state"
        [ test "it joins the three keys so a run-state transition changes it" <|
            \_ ->
                Expect.equal "d9d0b3c250c3:42:done"
                    (Transport.historyRefreshKey
                        (meta
                            [ ( "exoext.v1.etag", "d9d0b3c250c3" )
                            , ( "exoext.v1.run.seq", "42" )
                            , ( "exoext.v1.run.state", "done" )
                            ]
                        )
                    )
        , test "the etag alone does not change the key across a scan (run slot does)" <|
            \_ ->
                let
                    key state =
                        Transport.historyRefreshKey
                            (meta
                                [ ( "exoext.v1.etag", "d9d0b3c250c3" )
                                , ( "exoext.v1.run.seq", "42" )
                                , ( "exoext.v1.run.state", state )
                                ]
                            )
                in
                Expect.notEqual (key "running") (key "done")
        , test "missing keys render as empty segments" <|
            \_ ->
                Expect.equal "::" (Transport.historyRefreshKey (meta []))
        ]
