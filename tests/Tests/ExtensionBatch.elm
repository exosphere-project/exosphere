module Tests.ExtensionBatch exposing
    ( decodeSuite
    , matchingSuite
    , mutationSuite
    )

{-| Unit tests for the persisted extension batch-tail standard (`exoext.batch.v1`): the tolerant
encode/decode, the cloud+project+instance matching rule, and the record/forget list mutations. These
pin the wire contract of the batch store, the sibling of `Tests.ExtensionApproval`.

The store holds resumable WORK rather than a preference, so two properties matter more here than
they do for an approval: a record must never come back naming a batch with nothing left to do, and a
record must never be adopted against the wrong project.

-}

import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)
import Types.ExtensionBatch as ExtensionBatch exposing (ExtensionBatch)


sample : ExtensionBatch
sample =
    { cloudUrl = "https://keystone.example/v3"
    , projectUuid = "proj-1"
    , instanceUuid = "vm-abc"
    , batchId = "exo-cs-batch-1000"
    , remaining = [ "i-2", "i-3" ]
    }


identityOf : ExtensionBatch -> { cloudUrl : String, projectUuid : String, instanceUuid : String }
identityOf batch =
    { cloudUrl = batch.cloudUrl, projectUuid = batch.projectUuid, instanceUuid = batch.instanceUuid }


decodeSuite : Test
decodeSuite =
    describe "ExtensionBatch encode/decode"
        [ test "a record round-trips through encode -> decode unchanged" <|
            \_ ->
                Expect.equal (Ok sample)
                    (Decode.decodeValue ExtensionBatch.decoder (ExtensionBatch.encode sample))
        , test "the tail's ORDER survives the round trip" <|
            \_ ->
                -- The tail is a queue, not a set: resuming out of order would scan the wrong target
                -- next and, with a batch id shared across siblings, do it invisibly.
                Expect.equal (Ok [ "i-2", "i-3" ])
                    (Decode.decodeValue ExtensionBatch.decoder (ExtensionBatch.encode sample)
                        |> Result.map .remaining
                    )
        , test "unknown fields are ignored (forward-compatible)" <|
            \_ ->
                let
                    json =
                        """
                        { "cloudUrl": "https://keystone.example/v3"
                        , "projectUuid": "proj-1"
                        , "instanceUuid": "vm-abc"
                        , "batchId": "exo-cs-batch-1000"
                        , "remaining": ["i-2", "i-3"]
                        , "profile": "quick"
                        , "somethingNew": { "nested": 1 }
                        }
                        """
                in
                Expect.equal (Ok sample) (Decode.decodeString ExtensionBatch.decoder json)
        , test "missing optional fields default: batchId empty, remaining empty" <|
            \_ ->
                Expect.equal
                    (Ok { cloudUrl = "", projectUuid = "", instanceUuid = "vm-abc", batchId = "", remaining = [] })
                    (Decode.decodeString ExtensionBatch.decoder """{ "instanceUuid": "vm-abc" }""")
        , test "listDecoder drops records with no instanceUuid, keeps the rest" <|
            \_ ->
                let
                    json =
                        """
                        [ { "instanceUuid": "vm-good", "remaining": ["i-1"] }
                        , { "remaining": ["i-2"] }
                        , { "instanceUuid": "", "remaining": ["i-3"] }
                        , { "instanceUuid": "vm-good-2", "remaining": ["i-4"] }
                        ]
                        """
                in
                Expect.equal (Ok [ "vm-good", "vm-good-2" ])
                    (Decode.decodeString ExtensionBatch.listDecoder json
                        |> Result.map (List.map .instanceUuid)
                    )
        , test "listDecoder drops a record with an empty tail — there is no batch left in it" <|
            \_ ->
                -- Self-cleaning: a record that outlived its batch cannot come back as work.
                Expect.equal (Ok [])
                    (Decode.decodeString ExtensionBatch.listDecoder
                        """[ { "instanceUuid": "vm-abc", "remaining": [] } ]"""
                        |> Result.map (List.map .instanceUuid)
                    )
        ]


matchingSuite : Test
matchingSuite =
    describe "ExtensionBatch matching is by cloud + project + instance"
        [ test "find returns the record for a matching identity" <|
            \_ ->
                Expect.equal (Just sample) (ExtensionBatch.find (identityOf sample) [ sample ])
        , test "the same instance in a different project does NOT match" <|
            \_ ->
                -- A batch is an instruction to write requests, so adopting one against the wrong
                -- project would write scans the researcher never asked for.
                Expect.equal Nothing
                    (ExtensionBatch.find { cloudUrl = sample.cloudUrl, projectUuid = "other-proj", instanceUuid = "vm-abc" } [ sample ])
        , test "the same project on a different cloud does NOT match" <|
            \_ ->
                Expect.equal Nothing
                    (ExtensionBatch.find { cloudUrl = "https://elsewhere.example/v3", projectUuid = "proj-1", instanceUuid = "vm-abc" } [ sample ])
        , test "a different instance does NOT match" <|
            \_ ->
                Expect.equal Nothing
                    (ExtensionBatch.find { cloudUrl = sample.cloudUrl, projectUuid = "proj-1", instanceUuid = "vm-other" } [ sample ])
        , test "find against an empty store is Nothing" <|
            \_ ->
                Expect.equal Nothing (ExtensionBatch.find (identityOf sample) [])
        ]


mutationSuite : Test
mutationSuite =
    describe "ExtensionBatch record/forget"
        [ test "record stores a new tail" <|
            \_ ->
                Expect.equal [ "vm-abc" ]
                    (ExtensionBatch.record sample [] |> List.map .instanceUuid)
        , test "record replaces the instance's previous tail — one batch in flight per VM" <|
            \_ ->
                let
                    popped =
                        { sample | remaining = [ "i-3" ] }
                in
                Expect.equal ( 1, Just [ "i-3" ] )
                    (ExtensionBatch.record popped [ sample ]
                        |> (\batches -> ( List.length batches, List.head batches |> Maybe.map .remaining ))
                    )
        , test "record does not disturb another instance's tail" <|
            \_ ->
                let
                    otherVm =
                        { sample | instanceUuid = "vm-other" }
                in
                Expect.equal [ "vm-abc", "vm-other" ]
                    (ExtensionBatch.record sample [ otherVm ]
                        |> List.map .instanceUuid
                        |> List.sort
                    )
        , test "forget removes the matching record" <|
            \_ ->
                Expect.equal []
                    (ExtensionBatch.forget "vm-abc" [ sample ] |> List.map .instanceUuid)
        , test "forget leaves other instances' records intact" <|
            \_ ->
                let
                    otherVm =
                        { sample | instanceUuid = "vm-other" }
                in
                Expect.equal [ "vm-other" ]
                    (ExtensionBatch.forget "vm-abc" [ sample, otherVm ] |> List.map .instanceUuid)
        ]
