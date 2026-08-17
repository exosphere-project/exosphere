module Tests.ExtensionApproval exposing
    ( decodeSuite
    , matchingSuite
    , mutationSuite
    )

{-| Unit tests for the persisted extension-approval standard (`exoext.approval.v1`): the
tolerant encode/decode, the instanceUuid-only matching rule, and the grant/forget list
mutations. These pin the wire contract of the approval store.
-}

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Types.ExtensionApproval as ExtensionApproval exposing (ExtensionApproval)


sample : ExtensionApproval
sample =
    { cloudUrl = "https://keystone.example/v3"
    , projectUuid = "proj-1"
    , instanceUuid = "vm-abc"
    , nameAtApproval = "my-vm"
    , approvedAt = "2026-07-17T12:00:00Z"
    , manifestEtagAtApproval = "etag-xyz"
    }


decodeSuite : Test
decodeSuite =
    describe "ExtensionApproval encode/decode"
        [ test "a record round-trips through encode -> decode unchanged" <|
            \_ ->
                Expect.equal (Ok sample)
                    (Decode.decodeValue ExtensionApproval.decoder (ExtensionApproval.encode sample))
        , test "an old blob without the field decodes to [] (missing exoextApprovals)" <|
            \_ ->
                -- The list decoder is only reached under the "9" key; a top-level object without
                -- an array there is handled at the LocalStorage layer. Here we assert the list
                -- decoder itself yields [] for an empty array (the encoded empty store).
                Expect.equal (Ok [])
                    (Decode.decodeValue ExtensionApproval.listDecoder (Encode.list ExtensionApproval.encode []))
        , test "unknown fields are ignored (forward-compatible, e.g. a future disabled flag)" <|
            \_ ->
                let
                    json =
                        """
                        { "cloudUrl": "https://keystone.example/v3"
                        , "projectUuid": "proj-1"
                        , "instanceUuid": "vm-abc"
                        , "nameAtApproval": "my-vm"
                        , "approvedAt": "2026-07-17T12:00:00Z"
                        , "manifestEtagAtApproval": "etag-xyz"
                        , "disabled": true
                        , "somethingNew": { "nested": 1 }
                        }
                        """
                in
                Expect.equal (Ok sample) (Decode.decodeString ExtensionApproval.decoder json)
        , test "missing optional string fields default to empty strings" <|
            \_ ->
                Expect.equal
                    (Ok
                        { cloudUrl = ""
                        , projectUuid = ""
                        , instanceUuid = "vm-abc"
                        , nameAtApproval = ""
                        , approvedAt = ""
                        , manifestEtagAtApproval = ""
                        }
                    )
                    (Decode.decodeString ExtensionApproval.decoder """{ "instanceUuid": "vm-abc" }""")
        , test "listDecoder drops records with a missing or empty instanceUuid, keeps the rest" <|
            \_ ->
                let
                    json =
                        """
                        [ { "instanceUuid": "vm-good", "nameAtApproval": "keep" }
                        , { "nameAtApproval": "no-uuid" }
                        , { "instanceUuid": "", "nameAtApproval": "empty-uuid" }
                        , { "instanceUuid": "vm-good-2" }
                        ]
                        """
                in
                Expect.equal (Ok [ "vm-good", "vm-good-2" ])
                    (Decode.decodeString ExtensionApproval.listDecoder json
                        |> Result.map (List.map .instanceUuid)
                    )
        ]


matchingSuite : Test
matchingSuite =
    describe "ExtensionApproval matching is by instanceUuid only"
        [ test "isApproved is True for a stored instanceUuid" <|
            \_ ->
                Expect.equal True (ExtensionApproval.isApproved "vm-abc" [ sample ])
        , test "the same extension name on a different UUID is NOT approved" <|
            \_ ->
                let
                    -- Same display name, different instance: gets its own opt-in prompt.
                    other =
                        { sample | instanceUuid = "vm-different" }
                in
                Expect.equal False (ExtensionApproval.isApproved other.instanceUuid [ sample ])
        , test "isApproved is False against an empty store" <|
            \_ ->
                Expect.equal False (ExtensionApproval.isApproved "vm-abc" [])
        ]


mutationSuite : Test
mutationSuite =
    describe "ExtensionApproval grant/forget"
        [ test "grant appends a new approval" <|
            \_ ->
                Expect.equal [ "vm-abc" ]
                    (ExtensionApproval.grant sample [] |> List.map .instanceUuid)
        , test "grant replaces an existing approval for the same instanceUuid (refresh)" <|
            \_ ->
                let
                    refreshed =
                        { sample | nameAtApproval = "renamed", approvedAt = "2026-08-01T00:00:00Z" }

                    result =
                        ExtensionApproval.grant refreshed [ sample ]
                in
                Expect.equal ( 1, Just "renamed" )
                    ( List.length result
                    , List.head result |> Maybe.map .nameAtApproval
                    )
        , test "grant does not disturb approvals for other instances" <|
            \_ ->
                let
                    otherVm =
                        { sample | instanceUuid = "vm-other" }
                in
                Expect.equal [ "vm-abc", "vm-other" ]
                    (ExtensionApproval.grant sample [ otherVm ]
                        |> List.map .instanceUuid
                        |> List.sort
                    )
        , test "forget removes the matching approval" <|
            \_ ->
                Expect.equal []
                    (ExtensionApproval.forget "vm-abc" [ sample ] |> List.map .instanceUuid)
        , test "forget leaves other instances' approvals intact" <|
            \_ ->
                let
                    otherVm =
                        { sample | instanceUuid = "vm-other" }
                in
                Expect.equal [ "vm-other" ]
                    (ExtensionApproval.forget "vm-abc" [ sample, otherVm ] |> List.map .instanceUuid)
        ]
