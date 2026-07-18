module Tests.CloudShield.Transport exposing (capBodySuite, resolveResultBodySuite, resultBodySuite)

import CloudShield.Transport as Transport
import Expect
import Test exposing (Test, describe, test)


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
