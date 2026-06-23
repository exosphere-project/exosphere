module CloudShield.Transport exposing
    ( RunStatus
    , ScanRequest
    , chunkString
    , readChunkedBody
    , reqSlotMetadata
    , resultBodyFromMetadata
    , runStatusFromMetadata
    , scanRequestJson
    )

{-| CloudShield POC wire transport over Nova server metadata (Phase 0 spec §4.1 / §7.1).

This is the **throwaway framing layer** for the no-Jetstream2 POC: it carries the §4.1 scan
request and the §4.3 status / §4.2 result over server metadata instead of object storage.
Per the spec, the request/result _JSON_ and the run _states_ are identical to the Jetstream2
path; only the `seq`/`claimed`/chunk framing here is POC-specific and is dropped when
`store=swift` lands (Phase 1b).

All functions are pure and string-in/string-out so they unit-test without a live cloud.

Wire layout (on the publishing CloudShield VM's own metadata):

  - **Request slot (Exosphere → VM):** `exoext.v1.req.seq` = monotonic integer; the §4.1
    request JSON chunked across `exoext.v1.req.body.0..N` (≤255 chars/value, §3.1 / D5).
  - **Status / result slot (VM → Exosphere):** `exoext.v1.run.seq` echoes the request seq;
    `exoext.v1.run.state` ∈ queued|running|done|error|cancelled|expired (§4.4); an optional
    small result summary is chunked across `exoext.v1.res.body.0..N` (§4.2 shape).

-}

import Dict exposing (Dict)
import Json.Encode as Encode
import OpenStack.Types as OSTypes



-- REQUEST (Exosphere -> VM)


{-| The host-resolved scan request (§4.1). `target` is the re-resolved real instance
(§5.4); `createdAt` is an ISO-8601 string supplied by the host (Phase 0 §4.1).
-}
type alias ScanRequest =
    { requestId : String
    , batchId : Maybe String
    , createdAt : String
    , projectId : String
    , target : { instanceId : String, instanceName : String }
    , profile : String
    }


{-| Encode a §4.1 scan-request object to a compact JSON string (the body that gets chunked
into the metadata req-slot, or written object-side in Phase 1b — same bytes either way).
-}
scanRequestJson : ScanRequest -> String
scanRequestJson req =
    Encode.encode 0 <|
        Encode.object
            [ ( "schemaVersion", Encode.string "1.0" )
            , ( "requestId", Encode.string req.requestId )
            , ( "batchId"
              , case req.batchId of
                    Just b ->
                        Encode.string b

                    Nothing ->
                        Encode.null
              )
            , ( "createdAt", Encode.string req.createdAt )
            , ( "requestedBy"
              , Encode.object
                    [ ( "source", Encode.string "exosphere" )
                    , ( "projectId", Encode.string req.projectId )
                    ]
              )
            , ( "target"
              , Encode.object
                    [ ( "instanceId", Encode.string req.target.instanceId )
                    , ( "instanceName", Encode.string req.target.instanceName )
                    ]
              )
            , ( "scan"
              , Encode.object
                    [ ( "profile", Encode.string req.profile )
                    , ( "method", Encode.string "snapshot-clone" )
                    ]
              )
            ]


{-| Build the §7.1 request-slot metadata key/value items for a request: the monotonic `seq`,
a **chunk count** `exoext.v1.req.body.n`, and the request JSON chunked across
`exoext.v1.req.body.0..N-1` (≤255 chars/value). Each item is written with one
`requestSetServerMetadata` call on the CloudShield VM.

The explicit count is what makes a re-write safe: Nova metadata POST **merges** keys and never
deletes, so a later, shorter request would otherwise leave a stale trailing `body.N` chunk that
a gapless reader would concatenate into corrupt JSON. The reader (`readChunkedBody`) honors the
count and reads exactly `n` chunks, ignoring any orphan.

-}
reqSlotMetadata : Int -> String -> List OSTypes.MetadataItem
reqSlotMetadata seq requestJson =
    let
        chunks =
            chunkString 255 requestJson
    in
    { key = "exoext.v1.req.seq", value = String.fromInt seq }
        :: { key = "exoext.v1.req.body.n", value = String.fromInt (List.length chunks) }
        :: List.indexedMap
            (\i chunk ->
                { key = "exoext.v1.req.body." ++ String.fromInt i, value = chunk }
            )
            chunks



-- STATUS / RESULT (VM -> Exosphere)


{-| The coarse, UI-facing run status read back from the VM's metadata (§4.3 `state`), with
the `seq` it corresponds to (§7.1 correlation).
-}
type alias RunStatus =
    { seq : Int
    , state : String
    }


{-| Read the §7.1 status slot from metadata: `exoext.v1.run.seq` + `exoext.v1.run.state`.
`Nothing` unless both are present and `seq` parses.
-}
runStatusFromMetadata : List OSTypes.MetadataItem -> Maybe RunStatus
runStatusFromMetadata metadata =
    let
        dict =
            toDict metadata
    in
    Maybe.map2 RunStatus
        (Dict.get "exoext.v1.run.seq" dict |> Maybe.andThen String.toInt)
        (Dict.get "exoext.v1.run.state" dict)


{-| Reassemble the small result summary (§4.2) from `exoext.v1.res.body.*` chunks.
-}
resultBodyFromMetadata : List OSTypes.MetadataItem -> Maybe String
resultBodyFromMetadata metadata =
    readChunkedBody "exoext.v1.res.body." metadata


{-| Reassemble a chunked body written under `<prefix>0..N-1`. Prefers an explicit
`<prefix>n` count (so a stale orphan chunk from an earlier, longer write is ignored — Nova
metadata never deletes keys); falls back to gapless concatenation stopping at the first
missing index for bodies written without a count. `Nothing` when no body is present.
-}
readChunkedBody : String -> List OSTypes.MetadataItem -> Maybe String
readChunkedBody prefix metadata =
    let
        dict =
            toDict metadata

        gaplessFrom index acc =
            case Dict.get (prefix ++ String.fromInt index) dict of
                Just chunk ->
                    gaplessFrom (index + 1) (acc ++ chunk)

                Nothing ->
                    acc
    in
    case Dict.get (prefix ++ "n") dict |> Maybe.andThen String.toInt of
        Just count ->
            List.range 0 (count - 1)
                |> List.filterMap (\i -> Dict.get (prefix ++ String.fromInt i) dict)
                |> String.concat
                |> Just

        Nothing ->
            case Dict.get (prefix ++ "0") dict of
                Just _ ->
                    Just (gaplessFrom 0 "")

                Nothing ->
                    Nothing



-- HELPERS


{-| Split a string into chunks of at most `size` characters, in order. An empty string
yields a single empty chunk so the body is always representable as `body.0`.
-}
chunkString : Int -> String -> List String
chunkString size str =
    if size <= 0 then
        [ str ]

    else if String.length str <= size then
        [ str ]

    else
        String.left size str :: chunkString size (String.dropLeft size str)


toDict : List OSTypes.MetadataItem -> Dict String String
toDict metadata =
    metadata |> List.map (\item -> ( item.key, item.value )) |> Dict.fromList
