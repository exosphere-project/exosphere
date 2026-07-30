module Types.ExtensionBatch exposing
    ( ExtensionBatch
    , decoder
    , encode
    , find
    , forget
    , listDecoder
    , record
    )

{-| The undrained tail of a multi-request extension batch, persisted so a page reload does not
abandon it. This is the `exoext` mechanism's own standard, the sibling of
[`Types.ExtensionApproval`](Types-ExtensionApproval): CloudShield is merely its first consumer, so
nothing here imports a CloudShield module.

**Why it has to be persisted at all.** A batch is N sibling requests paced through the ONE request
slot the wire admits per publishing VM, so the requests that have not been written yet exist nowhere
but the browser. The wire cannot hold them: it has room for the request in flight and nothing else.
Reload mid-batch and every target after the current one was simply forgotten, sitting on an
optimistic badge with nothing left to advance it.

**What is NOT persisted.** The tail and the shared batch id, and no more:

  - the in-flight request itself is recovered from the wire (`run.target`), which is the truthful
    source and needs no local copy;
  - the "a decided write has not been issued yet" guard is deliberately absent. It exists to close a
    window between two messages within one session; after a reload no write is in flight, so it must
    read as False, and storing it could only ever wedge a restored batch.

Matching is by cloud + project + instance, all three. The instance UUID alone would almost certainly
do, but a batch is an instruction to write requests, and the cost of adopting one against the wrong
project is writing requests somewhere the researcher did not ask for.

The record decodes tolerantly (unknown fields ignored, missing strings default to `""`) so a later
addition needs no storage version break.

-}

import Json.Decode as Decode
import Json.Encode as Encode


type alias ExtensionBatch =
    { cloudUrl : String
    , projectUuid : String
    , instanceUuid : String

    -- the id every sibling request of this batch carries, `""` for a lone request (the wire field
    -- is null there). Persisted because a resumed sibling has to carry the SAME id, or the archive
    -- ends up with two batches where the researcher started one.
    , batchId : String

    -- the subjects whose requests have not been written yet, in the order they were selected.
    , remaining : List String
    }


encode : ExtensionBatch -> Encode.Value
encode batch =
    Encode.object
        [ ( "cloudUrl", Encode.string batch.cloudUrl )
        , ( "projectUuid", Encode.string batch.projectUuid )
        , ( "instanceUuid", Encode.string batch.instanceUuid )
        , ( "batchId", Encode.string batch.batchId )
        , ( "remaining", Encode.list Encode.string batch.remaining )
        ]


{-| Decode a single batch record. `instanceUuid` is the one required field (matching depends on it);
every other string field is optional and defaults to `""`, and a missing `remaining` decodes to `[]`.
Unknown fields are ignored, which is what keeps the encoding forward-compatible.
-}
decoder : Decode.Decoder ExtensionBatch
decoder =
    Decode.map5 ExtensionBatch
        (optionalString "cloudUrl")
        (optionalString "projectUuid")
        (Decode.field "instanceUuid" Decode.string)
        (optionalString "batchId")
        (Decode.maybe (Decode.field "remaining" (Decode.list Decode.string))
            |> Decode.map (Maybe.withDefault [])
        )


{-| Decode a stored list defensively: an element that fails to decode, carries an empty
`instanceUuid`, or has nothing left in `remaining` is dropped rather than failing the whole list. So
one corrupt record cannot wipe the store, and a record that survived its own batch draining cannot
come back as a batch with no work in it.
-}
listDecoder : Decode.Decoder (List ExtensionBatch)
listDecoder =
    Decode.list (Decode.maybe decoder)
        |> Decode.map (List.filterMap identity)
        |> Decode.map (List.filter (\batch -> batch.instanceUuid /= "" && not (List.isEmpty batch.remaining)))


optionalString : String -> Decode.Decoder String
optionalString key =
    Decode.maybe (Decode.field key Decode.string)
        |> Decode.map (Maybe.withDefault "")


{-| Store a batch's tail. A prior record for the same `instanceUuid` is replaced: the wire admits one
request in flight per publishing VM, so there is one batch to remember per instance and the newest
state of it is the only interesting one.
-}
record : ExtensionBatch -> List ExtensionBatch -> List ExtensionBatch
record batch batches =
    batch :: List.filter (\b -> b.instanceUuid /= batch.instanceUuid) batches


{-| Drop an instance's record — the batch drained, or was stopped. Nothing else refers to it, so
this is the whole of forgetting.
-}
forget : String -> List ExtensionBatch -> List ExtensionBatch
forget instanceUuid batches =
    List.filter (\b -> b.instanceUuid /= instanceUuid) batches


{-| The stored batch for one publishing instance in one project on one cloud, if any. All three parts
of the identity must match; see the module docs for why the project is checked rather than trusting
the UUID to be unique.
-}
find : { cloudUrl : String, projectUuid : String, instanceUuid : String } -> List ExtensionBatch -> Maybe ExtensionBatch
find identity batches =
    batches
        |> List.filter
            (\b ->
                b.cloudUrl
                    == identity.cloudUrl
                    && b.projectUuid
                    == identity.projectUuid
                    && b.instanceUuid
                    == identity.instanceUuid
            )
        |> List.head
