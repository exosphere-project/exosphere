module CloudShield.Discovery exposing
    ( Sentinel
    , Store(..)
    , eligibleInstances
    , manifestBodyFromMetadata
    , manifestObjectLocation
    , readSentinel
    , storeFromString
    )

{-| CloudShield extension discovery (Phase 0 spec §3.1 / §3.3, POC slice §7).

Pure, host-side discovery logic for the browser. Two read concerns, both fed from data
Exosphere already has on the model (no new fetch):

1.  **Sentinel detection** — is an extension published on the instance being viewed? This is
    the presence of the `exoext.v1.kind` key in the instance's Nova server metadata
    (`server.osProps.details.metadata`). When present, the other `exoext.v1.*` keys describe
    the schema, the body store, and capability flags.

2.  **Manifest-body resolution (POC, `store=metadata`)** — when the body lives in metadata
    (the no-Jetstream2 POC transport), it is chunked across ordered keys
    `exoext.v1.man.body.0`, `exoext.v1.man.body.1`, … (+ a `exoext.v1.man.body.n` count;
    the §7.1 chunk-framing pattern), concatenated in index order. The `man.` namespace
    matches the agent's publisher and the sibling `req.body`/`res.body` slots.
    `store=swift` (object storage) and `store=console` are resolved
    elsewhere (Phase 1b / a console-log handler); this module covers the metadata path.

Also exposes `eligibleInstances` — the §2.4 host-applied eligibility filter that turns the
project's real server list into the `$instances` the card iterates: ACTIVE only, excluding
the publishing instance itself.

-}

import CloudShield.Transport as Transport
import Dict exposing (Dict)
import OpenStack.Types as OSTypes



-- SENTINEL


{-| Where the manifest body lives (the `exoext.v1.store` value).
-}
type Store
    = StoreMetadata
    | StoreConsole
    | StoreSwift
    | StoreUnknown


{-| The host-trusted envelope read from server metadata. Display/gating only — never
rendered as card content.

`container` / `prefix` / `manifest` are only meaningful when `store == StoreSwift` (§3.1): the
object-store container, the per-instance key prefix, and the manifest object key relative to
that prefix.

-}
type alias Sentinel =
    { kind : String
    , schema : Maybe String
    , store : Store
    , flags : List String
    , published : Maybe String
    , container : Maybe String
    , prefix : Maybe String
    , manifest : Maybe String
    }


storeFromString : String -> Store
storeFromString raw =
    case raw of
        "metadata" ->
            StoreMetadata

        "console" ->
            StoreConsole

        "swift" ->
            StoreSwift

        _ ->
            StoreUnknown


{-| Read the discovery sentinel from a server's metadata. Returns `Just` only when the
`exoext.v1.kind` key is present (the §3.1 discovery sentinel); the store defaults to
`metadata` for the POC when unspecified.
-}
readSentinel : List OSTypes.MetadataItem -> Maybe Sentinel
readSentinel metadata =
    let
        dict =
            toDict metadata
    in
    Dict.get "exoext.v1.kind" dict
        |> Maybe.map
            (\kind ->
                { kind = kind
                , schema = Dict.get "exoext.v1.schema" dict
                , store =
                    Dict.get "exoext.v1.store" dict
                        |> Maybe.map storeFromString
                        |> Maybe.withDefault StoreMetadata
                , flags =
                    Dict.get "exoext.v1.flags" dict
                        |> Maybe.map (String.split "," >> List.map String.trim >> List.filter (not << String.isEmpty))
                        |> Maybe.withDefault []
                , published = Dict.get "exoext.v1.published" dict
                , container = Dict.get "exoext.v1.container" dict
                , prefix = Dict.get "exoext.v1.prefix" dict
                , manifest = Dict.get "exoext.v1.manifest" dict
                }
            )



-- MANIFEST BODY (store=metadata)


{-| Reassemble the manifest body from metadata chunks `exoext.v1.man.body.*` (§7.1 chunk
framing), honoring an explicit `exoext.v1.man.body.n` count when present and falling back to
gapless concatenation otherwise. `Nothing` when no body is present. The `man.body.` key
matches what the CloudShield agent publishes (`store/metadata.py` MAN_BODY) and the sibling
`req.body`/`res.body` slots.
-}
manifestBodyFromMetadata : List OSTypes.MetadataItem -> Maybe String
manifestBodyFromMetadata metadata =
    Transport.readChunkedBody "exoext.v1.man.body." metadata



-- MANIFEST LOCATION (store=swift)


{-| Resolve the manifest's `(container, objectName)` object-storage location from the sentinel
(§3.1 / §3.3: `store=swift` → fetch `<container>/<prefix><manifest>`). `Nothing` unless
`store == StoreSwift` and both `container` and `manifest` are present in the envelope; `prefix`
defaults to `""` (no prefix) when absent. The caller (`Rest.Swift.requestGetObjectCapped`)
fetches this object and hands the body to `CloudShield.Transport.capBody` before parsing.
-}
manifestObjectLocation : Sentinel -> Maybe { container : String, objectName : String }
manifestObjectLocation sentinel =
    case ( sentinel.store, sentinel.container, sentinel.manifest ) of
        ( StoreSwift, Just container, Just manifest ) ->
            Just
                { container = container
                , objectName = Maybe.withDefault "" sentinel.prefix ++ manifest
                }

        _ ->
            Nothing



-- ELIGIBILITY ($instances projection source, §2.4)


{-| The §2.4 host-applied eligibility filter: include only ACTIVE instances and exclude the
publishing instance itself (`selfUuid`). Reachability is a VM-side runtime check, not a list
filter, so it is intentionally not applied here.
-}
eligibleInstances :
    String
    -> List { id : String, name : String, status : OSTypes.ServerStatus }
    -> List { id : String, name : String, status : String }
eligibleInstances selfUuid servers =
    servers
        |> List.filter (\s -> s.status == OSTypes.ServerActive)
        |> List.filter (\s -> s.id /= selfUuid)
        |> List.map
            (\s ->
                { id = s.id
                , name = s.name
                , status = OSTypes.serverStatusToString s.status
                }
            )



-- HELPERS


toDict : List OSTypes.MetadataItem -> Dict String String
toDict metadata =
    metadata |> List.map (\item -> ( item.key, item.value )) |> Dict.fromList
