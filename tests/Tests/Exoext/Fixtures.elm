module Tests.Exoext.Fixtures exposing (advance, afterExpiry, archivedFindingsJson, beforeExpiry, cancelSlot, clockAfter, embedResultMetadata, errorEmbedBody, expiresAtIso, expiresMillis, modelAfterStartScan, modelPendingEmbed, modelRecording, modelWithArchivedFindings, modelWithManifest, modelWithResultRef, objectFor, okEmbedBody, okEmbedBodyWithResultId, pollTime, polledAt, project, projectPublishing, projectWithTargets, receivedAt, rowStates, runSlot, runSlotFor, serverPublishing, storedBatch, writeRequestAt)

{-| Shared fixtures for the `exoext` host tests: the wire as a publisher writes it, and the host
model at the points a test wants to start from.

They live in one place because the generic host suites (`Tests.Exoext.Host`) and the CloudShield
adapter suites (`Tests.CloudShield.ServerDetail`) drive the SAME host through different surfaces,
and a second copy of "what a §7.1 run slot looks like" is how the two would quietly drift apart.

-}

import Dict
import Helpers.RemoteDataPlusPlus as RDPP
import ISO8601
import OpenStack.Types as OSTypes
import Page.ServerDetail as ServerDetail
import Time
import Types.ExtensionBatch exposing (ExtensionBatch)
import Types.HelperTypes as HelperTypes
import Types.Interactivity as Interactivity
import Types.Project exposing (Project, ProjectSecret(..))
import Types.Server exposing (Server, ServerOrigin(..))


receivedAt : Time.Posix
receivedAt =
    Time.millisToPosix 0


{-| The client clock a poll reads. Every fixture run `seq` in this module is a small number below
it, so no fixture run is anywhere near `Exoext.Lifecycle.staleRunAfterMillis` old — the safety valve
is exercised deliberately in `cloudShieldStaleRunSuite` and never fires by accident elsewhere.
-}
pollTime : Time.Posix
pollTime =
    Time.millisToPosix 10000


modelWithManifest : String -> String -> ServerDetail.Model
modelWithManifest etag body =
    let
        model =
            ServerDetail.init "self"
    in
    { model
        | exoextManifest =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave { etag = etag, body = body } receivedAt)
                (RDPP.NotLoading Nothing)
    }


modelWithResultRef : String -> String -> String -> ServerDetail.Model
modelWithResultRef etag objectName body =
    let
        model =
            ServerDetail.init "self"
    in
    { model
        | exoextResultRef =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave { etag = etag, objectName = objectName, body = body } receivedAt)
                (RDPP.NotLoading Nothing)
    }


{-| A minimal project: `update` needs one to build the Nova metadata write, but nothing here reads
its contents (an empty server list makes §5.4 target re-resolution fall back to the raw id).
-}
project : Project
project =
    { secret = NoProjectSecret
    , auth =
        { catalog = []
        , project = { name = "Project One", uuid = "project-uuid" }
        , projectDomain = { name = "Default", uuid = "project-domain-uuid" }
        , user = { name = "user-one", uuid = "user-uuid" }
        , userDomain = { name = "Default", uuid = "user-domain-uuid" }
        , expiresAt = Time.millisToPosix 0
        , tokenValue = "token"
        }
    , region = Just { id = "RegionOne", description = "Region One" }
    , endpoints =
        { cinder = "https://openstack.example/cinder"
        , glance = "https://openstack.example/glance"
        , keystone = "https://openstack.example/keystone/v3"
        , manila = Nothing
        , nova = "https://openstack.example/nova"
        , neutron = "https://openstack.example/neutron"
        , placement = Nothing
        , jetstream2Accounting = Nothing
        , designate = Nothing
        , swift = Nothing
        }
    , description = Nothing
    , images = RDPP.empty
    , servers = RDPP.empty
    , serverEvents = Dict.empty
    , serverExoActions = Dict.empty
    , serverSecurityGroups = Dict.empty
    , serverVolumeAttachments = Dict.empty
    , serverVolumeActions = Dict.empty
    , serverActionRequestQueue = Dict.empty
    , shares = RDPP.empty
    , shareAccessRules = Dict.empty
    , shareExportLocations = Dict.empty
    , shareTypes = RDPP.empty
    , objectStorageUploads = []
    , flavors = RDPP.empty
    , keypairs = RDPP.empty
    , volumes = RDPP.empty
    , volumeSnapshots = RDPP.empty
    , networks = RDPP.empty
    , autoAllocatedNetworkUuid = RDPP.empty
    , floatingIps = RDPP.empty
    , dnsRecordSets = RDPP.empty
    , ports = RDPP.empty
    , securityGroups = RDPP.empty
    , securityGroupActions = Dict.empty
    , registeredLimits = RDPP.empty
    , projectLimits = RDPP.empty
    , projectUsages = RDPP.empty
    , computeQuota = RDPP.empty
    , volumeQuota = RDPP.empty
    , networkQuota = RDPP.empty
    , shareQuota = RDPP.empty
    , serverImages = []
    , jetstream2Allocations = RDPP.empty
    , knownUsernames = Dict.empty
    }


{-| The viewed CloudShield VM, publishing `metadata`. Only the uuid and the metadata are read by
anything under test; the rest is the inert filler a `Server` demands.
-}
serverPublishing : List OSTypes.MetadataItem -> Server
serverPublishing metadata =
    { osProps =
        { name = "cloudshield"
        , uuid = "self"
        , details =
            { openstackStatus = OSTypes.ServerActive
            , created = Time.millisToPosix 0
            , powerState = OSTypes.PowerRunning
            , imageUuid = "image-uuid"
            , flavorId = "flavor-id"
            , keypairName = Nothing
            , metadata = metadata
            , userUuid = "user-uuid"
            , volumesAttached = []
            , tags = []
            , lockStatus = OSTypes.ServerUnlocked
            , fault = Nothing
            }
        , consoleUrl = RDPP.empty
        }
    , exoProps =
        { floatingIpCreationOption = HelperTypes.DoNotUseFloatingIp
        , deletionAttempted = False
        , serverOrigin = ServerNotFromExo
        , receivedTime = Nothing
        , loadingSeparately = False
        }
    , interaction = Interactivity.NoInteraction
    }


{-| The project as the guard reads it: the viewed instance is present and its metadata carries the
given §7.1 status slot, so `exoextScanRequestPending` resolves against real wire state.
-}
projectPublishing : List OSTypes.MetadataItem -> Project
projectPublishing metadata =
    { project
        | servers =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave [ serverPublishing metadata ] receivedAt)
                (RDPP.NotLoading Nothing)
    }


{-| The §7.1 status slot as the VM would publish it for run `seq`.
-}
runSlot : Int -> String -> List { key : String, value : String }
runSlot seq state =
    [ { key = "exoext.v1.run.seq", value = String.fromInt seq }
    , { key = "exoext.v1.run.state", value = state }
    ]


{-| The §7.1 status slot as a WP8-or-later publisher writes it: the required seq/state plus the
§4.3 descriptors that name the run's target and request.
-}
runSlotFor : Int -> String -> String -> List { key : String, value : String }
runSlotFor seq state target =
    runSlot seq state
        ++ [ { key = "exoext.v1.run.target", value = target }
           , { key = "exoext.v1.run.requestId", value = "exoext-req-" ++ String.fromInt seq }
           ]


{-| The model exactly as the `CloudShieldMsg`/`ScanRequested` branch leaves it for a confirmed
3-target scan: all three rows optimistically `queued`, the card tracking the first target, and the
undrained tail parked in `exoextBatch` (no `batchId` yet — it is minted on the first write).
-}
modelAfterStartScan : ServerDetail.Model
modelAfterStartScan =
    let
        base =
            ServerDetail.init "self"

        card =
            base.exoextCard
    in
    { base
        | exoextCard =
            { card
                | scanState = Dict.fromList [ ( "i-1", "queued" ), ( "i-2", "queued" ), ( "i-3", "queued" ) ]
                , seq = 1
                , pending = Just { seq = 1, requestId = "", kind = "scan", subject = "i-1", since = Time.millisToPosix 0 }
            }
        , exoextBatch = Just { batchId = Nothing, remaining = [ "i-2", "i-3" ], awaitingWrite = True }
    }


{-| Issue the §7.1 request write the host would issue for `req` at wall-clock `millis`.

The request `kind` is taken from the model's own tracker, which is where the host takes it from for
a batch continuation (`ServerDetail.exoextTrackedKind`) and where the press that started the batch
put it. So a fixture never states a verb, and a test that sets up a tracker with an unusual kind
gets that kind on the wire.

-}
writeRequestAt : Int -> { subject : String, batchId : Maybe String } -> ServerDetail.Model -> ServerDetail.Model
writeRequestAt millis req model =
    let
        ( updated, _, _ ) =
            ServerDetail.update
                (ServerDetail.ExoextWriteRequest
                    { kind = model.exoextCard.pending |> Maybe.map .kind |> Maybe.withDefault ""
                    , subject = req.subject
                    , batchId = req.batchId
                    }
                    (Time.millisToPosix millis)
                )
                project
                model
    in
    updated


advance : Int -> String -> ServerDetail.Model -> ServerDetail.Model
advance seq state model =
    ServerDetail.advanceExoextBatch (runSlot seq state) model |> Tuple.first


{-| The tracked subject/seq and the durable per-row badges — what a reviewer would read off the
card while a batch drains.
-}
rowStates : ServerDetail.Model -> ( Maybe ( String, Int ), List (Maybe String) )
rowStates model =
    ( model.exoextCard.pending |> Maybe.map (\p -> ( p.subject, p.seq ))
    , [ "i-1", "i-2", "i-3" ] |> List.map (\id -> Dict.get id model.exoextCard.scanState)
    )


{-| The whole host path a poll takes: `GotExoextSync` against a project publishing `metadata`,
carrying the client clock. This is how the safety valve actually fires in the app, so it is what the
stale-run suite drives rather than calling the pieces directly.
-}
polledAt : Time.Posix -> List OSTypes.MetadataItem -> ServerDetail.Model -> ServerDetail.Model
polledAt now metadata model =
    let
        ( updated, _, _ ) =
            ServerDetail.update (ServerDetail.GotExoextSync now) (projectPublishing metadata) model
    in
    updated


{-| The clock as it reads when a run written at wall-clock `seq` is `ageMillis` old.
-}
clockAfter : Int -> Int -> Time.Posix
clockAfter seq ageMillis =
    Time.millisToPosix (seq + ageMillis)


{-| A project publishing `metadata` on the viewed VM, plus ACTIVE scan targets with the given ids —
what `exoextInstances` sees, and therefore what the eligibility half of the batch stale check reads.
-}
projectWithTargets : List String -> List OSTypes.MetadataItem -> Project
projectWithTargets targetIds metadata =
    let
        target id =
            let
                base =
                    serverPublishing []

                osProps =
                    base.osProps
            in
            { base | osProps = { osProps | uuid = id, name = id } }
    in
    { project
        | servers =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave (serverPublishing metadata :: List.map target targetIds) receivedAt)
                (RDPP.NotLoading Nothing)
    }


{-| A stored batch record as an earlier session left it, keyed to the fixture project + instance.
-}
storedBatch : List String -> ExtensionBatch
storedBatch remaining =
    { cloudUrl = "https://openstack.example/keystone/v3"
    , projectUuid = "project-uuid"
    , instanceUuid = "self"
    , batchId = "exoext-batch-1000"
    , remaining = remaining
    }


{-| The §7.1 cancel channel as the VM would see it after this host wrote a stop for `requestId`.
-}
cancelSlot : String -> List OSTypes.MetadataItem
cancelSlot requestId =
    [ { key = "exoext.v1.req.cancel", value = requestId } ]


{-| The archived-result object a given result id lives at (§4.2), the way
`exoextResultObjectName` builds it. A cached body is bound only when its object name is THIS one.
-}
objectFor : String -> Maybe String
objectFor resultId =
    Just ("results/" ++ resultId ++ ".json")


{-| Res-slot metadata carrying a chunked `kind:"embed"` result body, plus the manifest etag the
archived-findings fetch is keyed on.
-}
embedResultMetadata : String -> List { key : String, value : String }
embedResultMetadata bodyJson =
    [ { key = "exoext.v1.etag", value = "etag-1" }
    , { key = "exoext.v1.res.body.n", value = "1" }
    , { key = "exoext.v1.res.body.0", value = bodyJson }
    ]


{-| An `status:"ok"` embed result body for requestId `exoext-req-100`, expiring at `expiresAt`.
-}
okEmbedBody : String -> String
okEmbedBody expiresAt =
    "{\"kind\":\"embed\",\"requestId\":\"exoext-req-100\",\"batchId\":\"b1\",\"status\":\"ok\",\"embedUrl\":\"https://vm.example/embed\",\"embedExpiresAt\":\"" ++ expiresAt ++ "\"}"


{-| The same ok embed result, but from a publisher that echoes the §4.2 `resultId` the session was
minted for. `batchId` stays `b1` — the id its siblings share.
-}
okEmbedBodyWithResultId : String -> String
okEmbedBodyWithResultId resultId =
    "{\"kind\":\"embed\",\"requestId\":\"exoext-req-100\",\"batchId\":\"b1\",\"resultId\":\""
        ++ resultId
        ++ "\",\"status\":\"ok\",\"embedUrl\":\"https://vm.example/embed\",\"embedExpiresAt\":\""
        ++ expiresAtIso
        ++ "\"}"


{-| An `status:"error"` embed result body with a plain-string error.
-}
errorEmbedBody : String
errorEmbedBody =
    "{\"kind\":\"embed\",\"requestId\":\"exoext-req-100\",\"batchId\":\"b1\",\"status\":\"error\",\"embedUrl\":\"\",\"embedExpiresAt\":\"\",\"error\":\"remint failed\"}"


{-| A fixed embed-token expiry, and the client clock set just before / just after it.
-}
expiresAtIso : String
expiresAtIso =
    "2026-07-20T21:00:00.000Z"


expiresMillis : Int
expiresMillis =
    ISO8601.fromString expiresAtIso
        |> Result.map (ISO8601.toPosix >> Time.posixToMillis)
        |> Result.withDefault 0


beforeExpiry : Time.Posix
beforeExpiry =
    Time.millisToPosix (expiresMillis - 60000)


afterExpiry : Time.Posix
afterExpiry =
    Time.millisToPosix (expiresMillis + 60000)


{-| A model that has fetched the archived findings AND still remembers which result its getEmbed
`requestId` was for — the pre-reload state, and the only source for a publisher that echoes no
`resultId` of its own.
-}
modelRecording : String -> String -> ServerDetail.Model
modelRecording requestId resultId =
    let
        base =
            modelWithArchivedFindings
    in
    { base | exoextEmbedResultId = Just { requestId = requestId, resultId = resultId } }


modelPendingEmbed : Time.Posix -> ServerDetail.Model
modelPendingEmbed since =
    let
        model =
            ServerDetail.init "self"
    in
    { model
        | exoextPendingEmbed =
            Just { seq = 0, requestId = "exoext-req-200", kind = "getEmbed", subject = "b1", since = since }
    }


{-| The archived scan body's `findings`, as `/results` should carry them.
-}
archivedFindingsJson : String
archivedFindingsJson =
    """[{"severity":"low"}]"""


{-| A model that has fetched the archived scan body for `etag-1` (a history pick's findings). The
cached object is the one `objectFor "b1"` names.
-}
modelWithArchivedFindings : ServerDetail.Model
modelWithArchivedFindings =
    modelWithResultRef "etag-1" "results/b1.json" """{"findings":[{"severity":"low"}]}"""
