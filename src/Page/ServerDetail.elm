module Page.ServerDetail exposing (Model, Msg(..), PassphraseVisibility, VerboseStatus, adoptRestoredExoextBatch, adoptStoredExoextBatch, advanceExoextBatch, clearResolvedPendingEmbed, effectiveExoextResultBody, exoextAbandonStaleRun, exoextBatchSharedMsg, exoextCancelRequested, exoextCancellableRun, exoextCardTitle, exoextDismissSession, exoextDropFromTail, exoextManifestBodyForEtag, exoextManifestNeedsFetch, exoextNavigation, exoextReaderProjection, exoextRemovalState, exoextRequestBlocked, exoextRequestsPending, exoextRunControl, exoextScanRequestPending, exoextStatusOverride, exoextStopRequested, exoextStoppingTarget, exoextViewConfig, init, recoverExoextRun, update, view)

import CloudShield.Card
import CloudShield.Reader
import CloudShield.Wire
import DateFormat.Relative
import Dict
import Element
import Element.Font as Font
import Element.Input as Input
import Exoext.Discovery
import Exoext.Health
import Exoext.Lifecycle
import Exoext.Transport
import FeatherIcons as Icons
import Helpers.Cidr as Cidr
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers exposing (serverCreatorName)
import Helpers.Interaction as IHelpers
import Helpers.RemoteDataPlusPlus as RDPP exposing (Haveness(..), RefreshStatus(..))
import Helpers.String exposing (removeEmptiness)
import Helpers.Time
import Helpers.Validation as Validation
import ISO8601
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Maybe.Extra
import OpenStack.DnsRecordSet
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.ServerVolumes exposing (serverCanHaveVolumeAttached)
import OpenStack.Types as OSTypes
import Page.ServerResourceUsageAlerts
import Page.ServerResourceUsageCharts
import Rest.Nova
import Rest.Swift
import Route
import State.Error as Error
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Alert as Alert
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Grid exposing (scrollableCell)
import Style.Widgets.Icon as Icon
import Style.Widgets.Link as Link
import Style.Widgets.Popover.Popover exposing (popover, popoverStyleDefaults)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.StatusBadge as StatusBadge
import Style.Widgets.Tag exposing (tag)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Style.Widgets.Uuid exposing (copyableUuid)
import Style.Widgets.Validation exposing (warningMessage)
import Task
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.ExtensionApproval as ExtensionApproval exposing (ExtensionApproval)
import Types.ExtensionBatch as ExtensionBatch exposing (ExtensionBatch)
import Types.Guacamole exposing (ServerGuacamoleStatus(..))
import Types.HelperTypes exposing (FloatingIpOption(..), ProjectIdentifier, ServerResourceQtys, UserAppProxyHostname)
import Types.Interaction as ITypes
import Types.Project exposing (Project)
import Types.Server exposing (ExoFeature(..), ExoSetupStatus(..), Server, ServerOrigin(..), ServerUiStatus(..))
import Types.ServerResourceUsage
import Types.SharedMsg as SharedMsg
import View.Helpers as VH exposing (edges)
import View.Types exposing (SelectMod(..), ServerActionOption)
import Widget


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , verboseStatus : VerboseStatus
    , passphraseVisibility : PassphraseVisibility
    , serverActionNamePendingConfirmation : Maybe String
    , serverNamePendingConfirmation : Maybe String
    , retainFloatingIpsWhenDeleting : Bool
    , deleteFloatingIpsWhenShelving : Bool
    , exoextCard : CloudShield.Card.Model
    , exoextManifest :
        RDPP.RemoteDataPlusPlus
            String
            { etag : String
            , body : String
            }
    , exoextManifestRequestEtag : Maybe String
    , exoextResultRef :
        RDPP.RemoteDataPlusPlus
            String
            { etag : String
            , objectName : String
            , body : String
            }
    , exoextResultRefRequest :
        Maybe
            { etag : String
            , objectName : String
            }
    , exoextHistory : RDPP.RemoteDataPlusPlus String (List CloudShield.Wire.IndexEntry)
    , exoextHistoryRequestKey : Maybe String

    -- how many times this session has changed the archive itself (today: removals the publisher
    -- confirmed). It rides the index read's cache-buster, and it is the reason that buster is not
    -- simply the refresh key: the key is the manifest etag plus the run slot, and a removal moves
    -- NEITHER — so the refetch a removal asks for would go out on the same URL as the read before
    -- it, and could be answered out of the browser's cache with the removed row still in it.
    , exoextHistoryGeneration : Int

    -- the in-flight history-View getEmbed request, as the generic `Exoext.Lifecycle.PendingRequest`
    -- (`kind == "getEmbed"`, `subject` = the archived result id), recorded when its req slot is
    -- written so the card can show a spinner while the bridge mints a fresh embed (~10s over the
    -- next poll) and can time the request out. Single-slot: a newer getEmbed replaces it; a
    -- matching-`requestId` result clears it (see `clearResolvedPendingEmbed`). `Nothing` when
    -- nothing is in flight. This is the session-request view of `PendingRequest`; the scan-request
    -- view lives in the card model (`CloudShield.Card.Model.pending`).
    , exoextPendingEmbed : Maybe Exoext.Lifecycle.PendingRequest

    -- the in-flight `deleteResult` request, the removal twin of `exoextPendingEmbed`
    -- (`kind == "deleteResult"`, `subject` = the archived result being removed). Recorded when its
    -- req slot is written so the row can read "Removing…" from the press, and cleared by the
    -- publisher's acknowledgement (`resolveExoextRemoval`) or by the §7.1 request timeout.
    , exoextPendingDelete : Maybe Exoext.Lifecycle.PendingRequest

    -- the last removal the publisher REFUSED, with the reason it gave. Retained until the next
    -- removal is written, so a refusal survives the polls that follow it and stays on the row the
    -- researcher pressed. `Nothing` when no removal has failed.
    , exoextDeleteError :
        Maybe
            { resultId : String
            , message : String
            }

    -- the shared client clock as of the last read sync. The in-flight predicate has to be able to
    -- tell a request that is still coming from one that never will (`Exoext.Lifecycle.requestTimedOut`),
    -- and it is consulted from `update`, which has no clock of its own. A poll-old timestamp is
    -- ample for a 30-second bound and is honest about what it is: the clock as of the last time the
    -- host actually looked at the wire.
    , exoextClock : Time.Posix

    -- which archived result the last getEmbed asked for, keyed by that request's own `requestId`.
    -- The host's own correlation record — the request id matches the response, the result id names
    -- the resource — and unlike `exoextPendingEmbed` it survives the response (which clears the
    -- pending marker), so a resolved session keeps its identity. Replaced by the next getEmbed.
    -- It is the SECOND source `CloudShield.Reader.resultId` consults, behind the response's own echoed
    -- `resultId`: it exists for a publisher that does not echo one, and being session-local it is
    -- gone after a page reload, which is exactly where the echoed field takes over.
    , exoextEmbedResultId :
        Maybe
            { requestId : String
            , resultId : String
            }

    -- the undrained tail of a multi-target scan, as the generic `Exoext.Lifecycle.Batch`
    -- (`remaining` = the target ids whose requests have not been written yet, in selection order;
    -- `batchId` = the §4.1 id every sibling request carries, minted on the batch's first write).
    -- §7.1 allows one request in flight per VM, so the host — which owns request writing — also
    -- owns the pacing: `advanceExoextBatch` pops one subject per settled run. `Nothing` when no
    -- batch is draining.
    , exoextBatch : Maybe Exoext.Lifecycle.Batch

    -- the §4.1 requestId this session last asked the publisher to stop (written to
    -- `exoext.v1.req.cancel`). Kept host-side for one reason: the run it names must stop offering
    -- its Cancel control, which is the whole visible acknowledgement of the press. Cleared when the
    -- next request is written, mirroring the wire, so it can never suppress a later run's control.
    , exoextCancelRequestId : Maybe String

    -- whether the researcher CLOSED the open result session. Purely local display state: the results
    -- pane and its iframe are unmounted (unmounted, not hidden — a dead embed left mounted keeps
    -- retrying auth), and nothing on the wire or in the archive is touched.
    --
    -- A plain flag is sufficient because the pane's content cannot change without a request: it is
    -- cleared by every request write, so a dismissal can only ever suppress the session that was on
    -- screen when it was pressed. Pressing View again (even on the same row) writes a request, which
    -- is exactly the "the researcher wants it back" signal.
    , exoextSessionDismissed : Bool

    -- a batch tail read out of localStorage at page entry (`adoptStoredExoextBatch`), waiting to be
    -- adopted into `exoextBatch`.
    --
    -- Adoption is two-phase because the halves of the stale-record check become answerable at
    -- different moments. The IDENTITY half (this cloud, this project, this instance) is answerable at
    -- page entry, and is settled there. Whether the stored targets are still ELIGIBLE instances is
    -- not: the project's server list is still being fetched then, so every target would look
    -- ineligible and a perfectly live batch would be discarded. That half waits for the discovery
    -- sync, the first moment the instance list is real. `Nothing` once adopted or rejected.
    , exoextRestoredBatch : Maybe Exoext.Lifecycle.Batch
    }


type alias VerboseStatus =
    Bool


type PassphraseVisibility
    = PassphraseShown
    | PassphraseHidden


type Msg
    = GotPassphraseVisibility PassphraseVisibility
    | GotServerActionNamePendingConfirmation (Maybe String)
    | GotServerNamePendingConfirmation (Maybe String)
    | GotResetServerAction
    | GotRetainFloatingIpsWhenDeleting Bool
    | GotDeleteFloatingIpsWhenShelving Bool
    | GotSetServerName String
      -- Carries the shared client clock, because the read sync is the one place the host judges
      -- whether the §7.1 run slot is still believable (`exoextAbandonStaleRun`), and that is a
      -- question about elapsed time rather than about metadata.
    | GotExoextSync Time.Posix
    | GotExoextManifestObject Time.Posix String (Result HttpErrorWithBody String)
    | GotExoextResultObject Time.Posix String String (Result HttpErrorWithBody String)
    | GotExoextIndexObject Time.Posix String (Result HttpErrorWithBody String)
    | CloudShieldMsg CloudShield.Card.Msg
    | ExoextWriteRequest { kind : String, subject : String, batchId : Maybe String } Time.Posix
    | ExoextWriteEmbedRequest { kind : String, resultId : String, batchId : String } Time.Posix
    | ExoextWriteDeleteRequest { kind : String, resultId : String, batchId : String } Time.Posix
    | ExoextWriteApproval Time.Posix
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.ServerUuid -> Model
init serverUuid =
    { serverUuid = serverUuid
    , verboseStatus = False
    , passphraseVisibility = PassphraseHidden
    , serverActionNamePendingConfirmation = Nothing
    , serverNamePendingConfirmation = Nothing
    , retainFloatingIpsWhenDeleting = False
    , deleteFloatingIpsWhenShelving = True
    , exoextCard = CloudShield.Card.init
    , exoextManifest = RDPP.empty
    , exoextManifestRequestEtag = Nothing
    , exoextResultRef = RDPP.empty
    , exoextResultRefRequest = Nothing
    , exoextHistory = RDPP.empty
    , exoextHistoryRequestKey = Nothing
    , exoextHistoryGeneration = 0
    , exoextPendingEmbed = Nothing
    , exoextPendingDelete = Nothing
    , exoextDeleteError = Nothing
    , exoextClock = Time.millisToPosix 0
    , exoextEmbedResultId = Nothing
    , exoextBatch = Nothing
    , exoextCancelRequestId = Nothing
    , exoextSessionDismissed = False
    , exoextRestoredBatch = Nothing
    }


{-| Take up a batch tail persisted by an earlier session, called at page entry where the shared
model's stored records are in hand (`State.ViewState`).

Only the identity half of the stale check happens here — see `exoextRestoredBatch` for why the
eligibility half cannot. `awaitingWrite` is deliberately reconstructed as False rather than stored:
it guards a window between two messages inside one session, and after a reload nothing is in flight.

-}
adoptStoredExoextBatch : Project -> List ExtensionBatch -> Model -> Model
adoptStoredExoextBatch project batches model =
    { model
        | exoextRestoredBatch =
            ExtensionBatch.find
                { cloudUrl = project.endpoints.keystone
                , projectUuid = project.auth.project.uuid
                , instanceUuid = model.serverUuid
                }
                batches
                |> Maybe.map
                    (\stored ->
                        { batchId = emptyToNothing stored.batchId
                        , remaining = stored.remaining
                        , awaitingWrite = False
                        }
                    )
    }


{-| An identifier that identifies nothing reads as absent. The stored `batchId` is `""` for a lone
request, matching the null the wire carries there.
-}
emptyToNothing : String -> Maybe String
emptyToNothing value =
    if String.isEmpty value then
        Nothing

    else
        Just value


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotPassphraseVisibility visibility ->
            ( { model | passphraseVisibility = visibility }, Cmd.none, SharedMsg.NoOp )

        GotServerActionNamePendingConfirmation maybeAction ->
            ( { model | serverActionNamePendingConfirmation = maybeAction }, Cmd.none, SharedMsg.NoOp )

        GotServerNamePendingConfirmation maybeName ->
            ( { model | serverNamePendingConfirmation = maybeName }, Cmd.none, SharedMsg.NoOp )

        GotResetServerAction ->
            ( { model | serverActionNamePendingConfirmation = Nothing }
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg model.serverUuid <|
                    SharedMsg.ResetServerAction
            )

        GotRetainFloatingIpsWhenDeleting retain ->
            ( { model | retainFloatingIpsWhenDeleting = retain }, Cmd.none, SharedMsg.NoOp )

        GotDeleteFloatingIpsWhenShelving delete ->
            ( { model | deleteFloatingIpsWhenShelving = delete }, Cmd.none, SharedMsg.NoOp )

        GotSetServerName validName ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg model.serverUuid <|
                    SharedMsg.RequestSetServerName validName
            )

        GotExoextSync now ->
            syncExoextReads now project model

        GotExoextManifestObject receivedTime etag result ->
            ( receiveExoextManifest project receivedTime etag result model, Cmd.none, SharedMsg.NoOp )

        GotExoextResultObject receivedTime etag objectName result ->
            ( receiveExoextResultRef project receivedTime etag objectName result model, Cmd.none, SharedMsg.NoOp )

        GotExoextIndexObject receivedTime refreshKey result ->
            ( receiveExoextIndex project receivedTime refreshKey result model, Cmd.none, SharedMsg.NoOp )

        CloudShieldMsg cloudMsg ->
            let
                instances =
                    exoextInstances project model

                ( cloudModel, outMsg ) =
                    CloudShield.Card.update instances cloudMsg model.exoextCard

                -- A blocked scan must also undo the card's optimistic mutation: `requestScan` has
                -- already flipped its rows to `queued` and retargeted `pending`, and keeping that
                -- with no matching write would point the tracker at a seq the wire will never
                -- carry — losing the live run's correlation and wedging the drain. Only the
                -- scan-tracking fields roll back; the rest of the press stands, notably the
                -- renderer having closed its confirm dialog.
                card =
                    case outMsg of
                        Just (CloudShield.Card.WriteRequested _) ->
                            if exoextRequestBlocked project model then
                                CloudShield.Card.rollbackScanRequest model.exoextCard cloudModel

                            else
                                cloudModel

                        _ ->
                            cloudModel

                -- The card's own state is settled above; each outcome below decides what the HOST
                -- state does about it. Every branch returns the whole model rather than one field,
                -- because a stop touches three of them at once (the cancel marker, the batch, and
                -- the abandoned rows' badges).
                baseModel =
                    { model | exoextCard = card }
            in
            case outMsg of
                Just (CloudShield.Card.WriteRequested req) ->
                    -- §2.2/§4.1: N targets are N sibling requests, not one request with N
                    -- targets, and §7.1 admits one at a time — so write the head now and
                    -- park the tail for `advanceExoextBatch` to drain. The shared `batchId`
                    -- is minted on that first write (below), when the wall-clock seq exists.
                    -- Fetch the real wall-clock time so the §4.1 `createdAt` is genuine
                    -- (the agent's §4.4 expiry guard compares it to REQUEST_TTL; a
                    -- placeholder epoch would be treated as expired and never run).
                    -- A press that would disturb an in-flight request or a draining batch
                    -- is silently ignored, same convention as the getEmbed guard below.
                    case ( exoextRequestBlocked project model, req.targetIds ) of
                        ( True, _ ) ->
                            ( baseModel, Cmd.none, SharedMsg.NoOp )

                        ( False, [] ) ->
                            ( baseModel, Cmd.none, SharedMsg.NoOp )

                        ( False, firstId :: rest ) ->
                            let
                                started =
                                    { baseModel | exoextBatch = Just { batchId = Nothing, remaining = rest, awaitingWrite = True } }
                            in
                            ( started
                            , Task.perform (ExoextWriteRequest { kind = req.kind, subject = firstId, batchId = Nothing }) Time.now
                            , exoextBatchSharedMsg project started
                            )

                Just (CloudShield.Card.SessionRequested req) ->
                    -- §7.1 single-req-slot guard, from the live wire state: don't write a
                    -- getEmbed while a scan is active or its request is still unclaimed —
                    -- the seq bump would cancel that scan bridge-side. Silently ignore the
                    -- press when blocked (spec behavior). Otherwise stamp a genuine
                    -- wall-clock seq/`createdAt` (same as a scan request).
                    if exoextRequestBlocked project model then
                        ( baseModel, Cmd.none, SharedMsg.NoOp )

                    else
                        ( baseModel, Task.perform (ExoextWriteEmbedRequest req) Time.now, SharedMsg.NoOp )

                Just (CloudShield.Card.DeletionRequested req) ->
                    -- The same §7.1 slot and therefore the same guard as every other request. A
                    -- blocked press is silently ignored, exactly as a blocked View is: the manifest
                    -- disables the control while the guard holds, so a press that gets through and
                    -- does nothing is a race, not a state to explain.
                    if exoextRequestBlocked project model then
                        ( baseModel, Cmd.none, SharedMsg.NoOp )

                    else
                        ( baseModel, Task.perform (ExoextWriteDeleteRequest req) Time.now, SharedMsg.NoOp )

                Just (CloudShield.Card.NavigationRequested req) ->
                    ( baseModel, Cmd.none, exoextNavigation project req.instanceId )

                Just (CloudShield.Card.CancelRequested req) ->
                    exoextStopRequested project req baseModel

                Just CloudShield.Card.SessionDismissed ->
                    -- Host-local, and deliberately so: no Cmd, no SharedMsg, nothing written.
                    ( exoextDismissSession baseModel, Cmd.none, SharedMsg.NoOp )

                Just CloudShield.Card.ApprovalGranted ->
                    -- Stamp a genuine wall-clock `approvedAt`, same idiom as a scan request:
                    -- fetch `Time.now`, then build and persist the approval record.
                    ( baseModel, Task.perform ExoextWriteApproval Time.now, SharedMsg.NoOp )

                Just CloudShield.Card.ApprovalForgotten ->
                    -- Forgetting needs no timestamp; drop the record for this instance.
                    ( baseModel, Cmd.none, SharedMsg.ForgetExtensionApproval model.serverUuid )

                Nothing ->
                    ( baseModel, Cmd.none, SharedMsg.NoOp )

        ExoextWriteRequest req now ->
            let
                -- Use wall-clock millis as the req-slot seq: the card's own counter resets to 0
                -- on reload and would collide with a still-claimed seq persisted in the instance
                -- metadata (the agent then treats the new request as already-handled and skips it).
                -- A time-based seq is monotonic across reloads, so it never collides. Sync the
                -- card's `pending.seq` to it so the run<->request correlation still matches, and
                -- retarget `pending.subject` so a continuation's live run projects onto its own row.
                timeSeq =
                    Time.posixToMillis now

                -- §4.1 siblings share one `batchId`. A continuation carries the id it was given;
                -- a batch's first write mints it from that write's wall-clock seq (unique across
                -- reloads, same shape as `requestId`). A lone target keeps `Nothing` — the wire
                -- field is null for a single-instance scan.
                batchId =
                    case req.batchId of
                        Just id ->
                            Just id

                        Nothing ->
                            if List.isEmpty (model.exoextBatch |> Maybe.map .remaining |> Maybe.withDefault []) then
                                Nothing

                            else
                                Just ("exoext-batch-" ++ String.fromInt timeSeq)

                oldCloud =
                    model.exoextCard

                syncedCloud =
                    { oldCloud
                        | pending = Maybe.map (\p -> { p | seq = timeSeq, subject = req.subject }) oldCloud.pending
                    }

                written =
                    { model
                        | exoextCard = syncedCloud

                        -- The decided-on write is being issued right here, so the §7.1 pre-write
                        -- guard lifts; from now on the seq correlation is what keeps the slot
                        -- single-occupancy.
                        , exoextBatch = Maybe.map (\batch -> { batch | batchId = batchId, awaitingWrite = False }) model.exoextBatch

                        -- Mirror what the write itself does to the wire: `reqSlotMetadata` clears the
                        -- cancel channel, so the host's memory of that cancel has to go too, or the
                        -- new run would inherit a withdrawn Cancel control. A new scan's results also
                        -- supersede whatever the researcher had closed, so the pane is un-dismissed.
                        , exoextCancelRequestId = Nothing
                        , exoextSessionDismissed = False
                    }
            in
            -- This is where a batch's shared id is minted, so it is also where the stored record
            -- first learns it: a tail restored without it would resume as unrelated lone requests.
            ( written
            , writeExoextRequestCmd project model (exoextInstances project model) { kind = req.kind, seq = timeSeq, subject = req.subject, batchId = batchId } now
            , exoextBatchSharedMsg project written
            )

        ExoextWriteEmbedRequest req now ->
            -- getEmbed does not touch the card's scan state or `pending`; it just writes the
            -- §7.1 req slot. The embed result is correlated later by `kind == "embed"`, not by
            -- `run.state`. Record a single-slot pending marker so the card can show a spinner and
            -- time the request out; its `requestId` mirrors the one `writeExoextRequestCmd` stamps
            -- (both derived from the same `now`) so a matching result later clears it.
            let
                requestId =
                    exoextRequestId (Time.posixToMillis now)
            in
            ( { model
                | exoextPendingEmbed =
                    Just
                        { seq = Time.posixToMillis now
                        , requestId = requestId
                        , kind = req.kind
                        , subject = req.resultId
                        , since = now
                        }

                -- Outlives the pending marker: the response clears that, and the resolved session
                -- still has to know which archived result it is showing (see the field's comment).
                , exoextEmbedResultId = Just { requestId = requestId, resultId = req.resultId }

                -- Asking for a session is the researcher asking to SEE one, so any earlier dismissal
                -- is spent. This is also what makes re-opening the very scan that was just closed
                -- work, without the dismissal having to know which result it closed.
                , exoextSessionDismissed = False

                -- Same reasoning as the scan write: this write clears the cancel channel on the wire.
                , exoextCancelRequestId = Nothing
              }
            , writeExoextRequestCmd project
                model
                (exoextInstances project model)
                { kind = req.kind, seq = Time.posixToMillis now, subject = req.resultId, batchId = Just req.batchId }
                now
            , SharedMsg.NoOp
            )

        ExoextWriteDeleteRequest req now ->
            -- The removal twin of `ExoextWriteEmbedRequest`, and deliberately its mirror image: the
            -- same §7.1 slot, the same seq-derived `requestId` (so the acknowledgement correlates),
            -- and the same single-marker discipline. It touches no scan state and no run: a removal
            -- is about the archive, not about a run, so `run.state` is none of its business.
            let
                requestId =
                    exoextRequestId (Time.posixToMillis now)
            in
            ( { model
                | exoextPendingDelete =
                    Just
                        { seq = Time.posixToMillis now
                        , requestId = requestId
                        , kind = req.kind
                        , subject = req.resultId
                        , since = now
                        }

                -- A fresh attempt retires the previous refusal: the row is being asked again, so
                -- the old reason is no longer what is happening to it.
                , exoextDeleteError = Nothing

                -- Same reasoning as the other two request writers: this write clears the cancel
                -- channel on the wire, so the host's memory of that cancel has to go with it.
                , exoextCancelRequestId = Nothing
              }
            , writeExoextRequestCmd project
                model
                (exoextInstances project model)
                { kind = req.kind, seq = Time.posixToMillis now, subject = req.resultId, batchId = emptyToNothing req.batchId }
                now
            , SharedMsg.NoOp
            )

        ExoextWriteApproval now ->
            -- Build the `exoext.approval.v1` record with a genuine wall-clock `approvedAt`, then
            -- hand it to the shared model for persistence. A missing server (nothing to approve)
            -- is a no-op.
            ( model
            , Cmd.none
            , buildExoextApproval project model now
                |> Maybe.map SharedMsg.GrantExtensionApproval
                |> Maybe.withDefault SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


{-| The §2.4 `$instances` projection: the project's real servers, eligibility-filtered (ACTIVE-only,
excluding the publishing instance itself and anything the publisher marked as its own transient
machinery). Computed identically in `update` and `view` so the renderer's row indices stay stable.
The VM never supplies this list — it comes from Exosphere's own server data, so it cannot inject fake
or out-of-project rows.

Each server's own Nova metadata rides along because eligibility now reads one key off it
(`Exoext.Discovery.transientKey`); the filter stays in `Exoext.Discovery` rather than being applied
here, so all of §2.4 keeps living in one place.

-}
exoextInstances : Project -> Model -> List CloudShield.Card.Instance
exoextInstances project model =
    project.servers
        |> RDPP.withDefault []
        |> List.map
            (\s ->
                { id = s.osProps.uuid
                , name = s.osProps.name
                , status = s.osProps.details.openstackStatus
                , metadata = s.osProps.details.metadata
                }
            )
        |> Exoext.Discovery.eligibleInstances model.serverUuid


{-| The §7.1 single-slot guard: whether writing ANY request right now would disturb one that is
already in flight.

There is ONE predicate rather than one per verb, because there is one request slot. The old pair
each guarded its own verb against the other's requests and neither guarded a verb against its own
kind, so the two ways a press could destroy work were exactly the two nobody was watching: a Scan
pressed while a View was in flight bumped the slot out from under it, and a View pressed while a
Scan was still unclaimed did the same in reverse. The bridge drops the superseded request with no
terminal state, so the tracker then waits out the six-hour stale bound — a lost request that looks
like a running one.

The five clauses are the five ways a request can be outstanding:

  - a tracked scan whose correlated run has not settled (`exoextScanRequestPending`);
  - an inline-answer request still within its timeout — a session mint, or a removal;
  - a batch still draining: subjects left to write, or a decided write not yet issued. Not redundant
    with the first clause — between two siblings the tracked run reads terminal, which is precisely
    the window in which a press corrupts a live batch;
  - a request the publisher has not claimed yet, read off the wire
    ([`Exoext.Transport.reqSlotUnclaimedSeq`](Exoext-Transport#reqSlotUnclaimedSeq)). This is the
    only clause that can see a request a DIFFERENT browser tab wrote, and the only one that covers
    the window between a write and the poll that reports it.

Every clause that could otherwise never clear is bounded: the inline markers by the request timeout,
the unclaimed slot by the same bound against its own wall-clock seq. A guard that outlives the
request it protects is not a guard, it is a page nobody can press anything on.

No server to write against counts as blocked, which is the fail-closed answer.

-}
exoextRequestBlocked : Project -> Model -> Bool
exoextRequestBlocked project model =
    case GetterSetters.serverLookup project model.serverUuid of
        Just server ->
            let
                inlineRequestPending pending =
                    pending
                        |> Maybe.map (\p -> not (Exoext.Lifecycle.requestTimedOut model.exoextClock p))
                        |> Maybe.withDefault False
            in
            exoextScanRequestPending project model
                || inlineRequestPending model.exoextPendingEmbed
                || inlineRequestPending model.exoextPendingDelete
                || (case model.exoextBatch of
                        Just batch ->
                            not (List.isEmpty batch.remaining) || batch.awaitingWrite

                        Nothing ->
                            False
                   )
                || exoextSlotUnclaimed model.exoextClock server.osProps.details.metadata

        Nothing ->
            True


{-| Whether the publishing instance's request slot holds a request the publisher has not claimed
yet, and recently enough to still be worth protecting.

The freshness bound is the point. The seq IS the wall-clock moment the request was written (§7.1),
so an unclaimed slot older than the request timeout belongs to a publisher that is not answering —
and the answer to a publisher that is not answering is to let the researcher act, never to keep the
page locked on its behalf.

-}
exoextSlotUnclaimed : Time.Posix -> List OSTypes.MetadataItem -> Bool
exoextSlotUnclaimed now metadata =
    Exoext.Transport.reqSlotUnclaimedSeq metadata
        |> Maybe.map
            (\seq ->
                not
                    (Exoext.Lifecycle.requestTimedOut now
                        { seq = seq
                        , requestId = ""
                        , kind = ""
                        , subject = ""
                        , since = Time.millisToPosix seq
                        }
                    )
            )
        |> Maybe.withDefault False


{-| True when any exoext request on this ServerDetail page is still worth fast-polling for.
The CloudShield card currently exposes two browser-tracked in-flight paths: a history `getEmbed`
request (`exoextPendingEmbed`) and a scan request (`exoextCard.pending`) whose correlated
run slot has not reached a terminal state.
-}
exoextRequestsPending : Project -> Model -> Bool
exoextRequestsPending project model =
    (model.exoextPendingEmbed /= Nothing) || exoextScanRequestPending project model


{-| True when the CloudShield scan marker is present and the publishing server's correlated
exoext run state is not terminal. An absent/uncorrelated run slot defaults to queued because the
request was just written and the bridge may not have claimed it yet.
-}
exoextScanRequestPending : Project -> Model -> Bool
exoextScanRequestPending project model =
    case GetterSetters.serverLookup project model.serverUuid of
        Just server ->
            Exoext.Lifecycle.requestStillPending model.exoextCard.pending server.osProps.details.metadata

        Nothing ->
            -- No server to read a run slot from: with a tracked scan the request has just been
            -- issued and cannot yet be terminal, so treat it as pending (matches the old default).
            model.exoextCard.pending /= Nothing


{-| Build the `exoext.approval.v1` record for the instance being viewed, stamped with a genuine
wall-clock `approvedAt`. Cloud/project identity reuse the values the stored-project convention
already keys on (keystone URL + `auth.project.uuid`); `nameAtApproval` and
`manifestEtagAtApproval` are display/staleness metadata captured at approval time (matching is
by `instanceUuid` only). `Nothing` when the server is not in the project's list.
-}
buildExoextApproval : Project -> Model -> Time.Posix -> Maybe ExtensionApproval
buildExoextApproval project model now =
    GetterSetters.serverLookup project model.serverUuid
        |> Maybe.map
            (\server ->
                { cloudUrl = project.endpoints.keystone
                , projectUuid = project.auth.project.uuid
                , instanceUuid = server.osProps.uuid
                , nameAtApproval = server.osProps.name
                , approvedAt = ISO8601.toString (ISO8601.fromPosix now)
                , manifestEtagAtApproval = exoextEtag server.osProps.details.metadata
                }
            )


syncExoextReads : Time.Posix -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
syncExoextReads now project model =
    case GetterSetters.serverLookup project model.serverUuid of
        Just server ->
            let
                metadata =
                    server.osProps.details.metadata

                -- Fold each fresh poll's metadata into the pending-embed marker: a matching
                -- result resolves the in-flight getEmbed, so drop the spinner state here.
                syncedModel =
                    { model
                        | exoextPendingEmbed =
                            clearResolvedPendingEmbed metadata model.exoextPendingEmbed
                        , exoextClock = now
                    }
                        |> resolveExoextRemoval metadata

                -- Four steps that must run in this order, because each feeds the next.
                --
                -- 1. Take up a stored batch tail (the instance list is real by now, which is what
                --    the eligibility half of its stale check was waiting for), so that
                -- 2. run recovery can see whether a tail is waiting on the run it is adopting —
                --    that is what decides whether a FINISHED run still needs a tracker; so that
                -- 3. the safety valve sees the batch AND the tracker that a stale run is holding
                --    hostage, which is what lets it release both at once. It comes last of the
                --    three deliberately: releasing before adoption would let the very next poll
                --    restore the tail it just dropped. So that
                -- 4. the batch step below has both the tail and the tracker it needs to continue.
                recoveredModel =
                    syncedModel
                        |> adoptRestoredExoextBatch project
                        |> recoverExoextRun now metadata
                        |> exoextAbandonStaleRun now metadata

                ( readModel, readCmd ) =
                    case Exoext.Discovery.readSentinel metadata of
                        Just ({ store } as sentinel) ->
                            if store == Exoext.Discovery.StoreSwift then
                                let
                                    etag =
                                        exoextEtag metadata
                                in
                                syncExoextIndex project sentinel (Exoext.Transport.historyRefreshKey metadata) <|
                                    syncExoextResultRef project sentinel etag metadata <|
                                        syncExoextManifest project sentinel etag recoveredModel

                            else
                                ( recoveredModel, Cmd.none )

                        Nothing ->
                            ( recoveredModel, Cmd.none )

                -- A fresh poll is the only moment the wire can report a run settling, so it is
                -- also the batch's continuation trigger.
                ( batchModel, batchCmd ) =
                    advanceExoextBatch metadata readModel
            in
            -- A poll can adopt, pop or drain the tail, so it is also where the stored record is
            -- brought back in line with it.
            ( batchModel, Cmd.batch [ readCmd, batchCmd ], exoextBatchSharedMsg project batchModel )

        Nothing ->
            ( model, Cmd.none, SharedMsg.NoOp )


{-| Adopt a run the wire reports but this browser session never wrote — the reload-mid-scan fix.

Reloading the page throws away every session-local tracker, so before the run slot carried
`run.target` (§4.3) the host had no way to say which row a live run belonged to and the card read
`idle` while a scan was plainly running. With the target on the wire the decision is
[`Exoext.Lifecycle.recoverRun`](Exoext-Lifecycle#recoverRun); this is just its application to the
page model, so the precedence rule lives in one generic place:

  - `RecoverPending` sets `exoextCard.pending`, which is all the existing projections need — the
    row's live state (`statusOverride`), the elapsed timer, the 5 s fast poll and the batch pacing
    are every one of them keyed on the tracked request, and none of them care who wrote it.
  - `NoRecovery` leaves the model exactly as it was. Notably this is the case whenever a tracker is
    already set, so a live scan can never be rewound by a lagging run slot — and also whenever the
    wire's run is already terminal with no batch tail waiting on it, because the §7.1 run slot is a
    LAST-run record rather than a current-run one. Committing that into the card's durable per-row
    state is what made a finished scan read as `done` on every page load ever after; the completed
    scan belongs in the history panel, where it carries the timestamp that says when it happened.
    It is ALSO the case for a non-terminal run that is
    [stale](Exoext-Lifecycle#staleRunAfterMillis), which is why `now` is threaded in here.

A recovered run does NOT pre-fill the row's optimistic `scanState`, so the card's own dedup does not
see it and its Scan button stays pressable. `exoextRequestBlocked` is the guard that catches such a
press (and rolls the card back), which is where a duplicate request has always been stopped.

-}
recoverExoextRun : Time.Posix -> List OSTypes.MetadataItem -> Model -> Model
recoverExoextRun now metadata model =
    case
        Exoext.Lifecycle.recoverRun
            { now = now
            , tracked = model.exoextCard.pending
            , tailPending = exoextTailPending model
            , metadata = metadata
            }
    of
        Exoext.Lifecycle.NoRecovery ->
            model

        Exoext.Lifecycle.RecoverPending request ->
            let
                card =
                    model.exoextCard
            in
            { model | exoextCard = { card | pending = Just request } }


{-| The safety valve: let go of a §7.1 run the wire has left non-terminal for implausibly long
([`Exoext.Lifecycle.runStale`](Exoext-Lifecycle#runStale) — that is where the threshold and the
contract argument live).

It exists because two correct behaviors compose into a trap. A publisher whose daemon is restarted
mid-run never settles `exoext.v1.run.state`, so the host reads a scan that is live forever; and the
host correctly withholds every Scan affordance while a run is in flight (`exoextRequestBlocked`, and
`/scanBusy` through it). The researcher is then left with a row that says scanning, no way to start
a new scan, and no way to clear the old one. The publisher-side bug is fixed on its own side; this
is the host's defense in depth, because the host cannot assume every publisher is healthy.

Three things go at once, because releasing any subset leaves the trap half-shut:

  - **the tracked request**, which is what `exoextRequestBlocked`, `exoextStatusOverride`, the elapsed
    timer, and the 5 s fast poll are all keyed on. Dropping it is what makes the row read `idle` and
    the Scan controls live again, and it is why no consumer needed a staleness check of its own.
  - **the batch**, stored record included. A tail waiting behind the wedged run is the OTHER half of
    `exoextRequestBlocked`, so leaving it would keep the press blocked with the tracker already gone.
    The tail is dropped rather than resumed on purpose: its head died at an unknown hour, and
    silently starting scans on an unattended page would spend the researcher's snapshot quota on
    work they are no longer watching. Re-selecting the targets is one press; an unwatched batch is
    not undoable.
  - **the optimistic row states** of the abandoned subjects, via `abandonScanState` — the same
    treatment a stopped batch's targets get. Those rows have no run coming and no terminal state
    will ever arrive to overwrite them, so without this they would sit on a spinning `queued` badge.
    Removing the key returns them to the absent-means-idle default, so a stale run reads as idle /
    absent and introduces no new visible state and no new display string.

The guard is that the host is not tracking some NEWER run: a tracked request whose seq differs from
the run slot's is a request written after the stale one, and it owns the batch. `Nothing` tracked
passes the guard, which is the post-reload case — recovery has already declined to adopt the stale
run, so there is nothing left to match against and the only thing to release is a restored tail.

Nothing is written to the wire here. Going stale withdraws the host's belief in a run; it does not
cancel it, supersede it, or touch the request slot.

-}
exoextAbandonStaleRun : Time.Posix -> List OSTypes.MetadataItem -> Model -> Model
exoextAbandonStaleRun now metadata model =
    case Exoext.Transport.runStatusFromMetadata metadata of
        Nothing ->
            model

        Just status ->
            let
                -- Not tracking a DIFFERENT (newer) run than the one the slot reports.
                tracksThisRun =
                    model.exoextCard.pending
                        |> Maybe.map (\pending -> pending.seq == status.seq)
                        |> Maybe.withDefault True
            in
            if tracksThisRun && Exoext.Lifecycle.runStale now { seq = status.seq, state = status.state } then
                let
                    abandoned =
                        (model.exoextCard.pending |> Maybe.map (\pending -> [ pending.subject ]) |> Maybe.withDefault [])
                            ++ (model.exoextBatch |> Maybe.map .remaining |> Maybe.withDefault [])
                            ++ (model.exoextRestoredBatch |> Maybe.map .remaining |> Maybe.withDefault [])

                    card =
                        model.exoextCard
                in
                { model
                    | exoextCard = CloudShield.Card.abandonScanState abandoned { card | pending = Nothing }
                    , exoextBatch = Nothing
                    , exoextRestoredBatch = Nothing
                }

            else
                model


{-| Second phase of adopting a stored batch tail: keep the targets that are still eligible instances
and move the tail into `exoextBatch`.

Three guards, in this order:

  - **Never displace a live batch.** Same precedence rule as run recovery: a batch this session is
    already draining is the newer truth, and a stored record can only fill a gap.
  - **Wait for an instance list to compare against.** No eligible instances means the list has not
    arrived, NOT that every target is gone. Entering this page fetches the publishing VM first and
    its siblings later, so the earliest polls genuinely see a list of one — and reading that as "all
    the stored targets have been deleted" would discard a live batch in exactly the reload case this
    whole record exists for. The decision waits instead; `exoextBatchSharedMsg` holds the record
    while it does. A project with no scannable instance at all never resolves, which costs one idle
    stored record and cannot cost a scan, because there is nothing there to scan.
  - **Drop targets that are no longer eligible.** An instance deleted or shut down since the record
    was written would otherwise get a request the publisher can only fail. If nothing survives, this
    is not a batch and the record goes.

Once decided, `exoextRestoredBatch` clears, so this settles once per page entry rather than per poll.

-}
adoptRestoredExoextBatch : Project -> Model -> Model
adoptRestoredExoextBatch project model =
    case ( model.exoextRestoredBatch, model.exoextBatch ) of
        ( Just restored, Nothing ) ->
            let
                eligible =
                    exoextInstances project model |> List.map .id
            in
            if List.isEmpty eligible then
                model

            else
                let
                    remaining =
                        restored.remaining |> List.filter (\subject -> List.member subject eligible)
                in
                { model
                    | exoextRestoredBatch = Nothing
                    , exoextBatch =
                        if List.isEmpty remaining then
                            Nothing

                        else
                            Just { restored | remaining = remaining }
                }

        ( Just _, Just _ ) ->
            { model | exoextRestoredBatch = Nothing }

        ( Nothing, _ ) ->
            model


{-| The persistence decision for the current batch, derived from state rather than from a diff: a
tail with work left is worth storing, and anything else is worth forgetting. Emitted by every
`update` branch that can change the batch, so the record cannot drift from the model — and being
idempotent, re-emitting it is free.

A batch with an empty `remaining` is exactly the "drained, or its run slot went terminal with nothing
left" case, so it needs no separate condition.

The one thing it must not do is delete a record it has not finished reading: a restored tail still
waiting on the instance list (see [`adoptRestoredExoextBatch`](#adoptRestoredExoextBatch)) looks
exactly like "no batch" from here, and forgetting it there would destroy the tail on the first poll
after a reload — before anything had a chance to resume it.

-}
exoextBatchSharedMsg : Project -> Model -> SharedMsg.SharedMsg
exoextBatchSharedMsg project model =
    case model.exoextBatch of
        Just batch ->
            if List.isEmpty batch.remaining then
                SharedMsg.ForgetExtensionBatch model.serverUuid

            else
                SharedMsg.RecordExtensionBatch
                    { cloudUrl = project.endpoints.keystone
                    , projectUuid = project.auth.project.uuid
                    , instanceUuid = model.serverUuid
                    , batchId = batch.batchId |> Maybe.withDefault ""
                    , remaining = batch.remaining
                    }

        Nothing ->
            if model.exoextRestoredBatch /= Nothing then
                SharedMsg.NoOp

            else
                SharedMsg.ForgetExtensionBatch model.serverUuid


{-| Whether an undrained batch tail is waiting on the current run. It is what tells
[`recoverExoextRun`](#recoverExoextRun) that a FINISHED run still has to be adopted as a tracked
request: `advanceExoextBatch` pops the next subject only by settling a tracked request, so a batch
whose run had already finished by the time the page reloaded would otherwise never resume.
-}
exoextTailPending : Model -> Bool
exoextTailPending model =
    case model.exoextBatch of
        Just batch ->
            not (List.isEmpty batch.remaining)

        Nothing ->
            False


{-| Pace a §7.1 batch through the single request slot, one subject per settled run. Commit the
finished subject's terminal state into the card's durable `scanState` first: the live run
projection covers only the tracked request's subject, so a finished row would otherwise revert to
its optimistic `queued` badge the moment the tracker retargets. Then write the next sibling's
request, carrying the batch's shared `batchId`.

An exhausted batch clears `exoextBatch` but deliberately leaves `exoextCard.pending` set — the
completion timer (`CloudShield.Reader.completionTimer`) and the settled-run projection are both
keyed on it.

-}
advanceExoextBatch : List OSTypes.MetadataItem -> Model -> ( Model, Cmd Msg )
advanceExoextBatch metadata model =
    case Exoext.Lifecycle.advanceBatch model.exoextCard.pending model.exoextBatch metadata of
        Exoext.Lifecycle.BatchWaiting ->
            ( model, Cmd.none )

        Exoext.Lifecycle.BatchSettled settled ->
            let
                settledModel =
                    { model
                        | exoextCard =
                            CloudShield.Card.settleScanState settled.subject settled.state model.exoextCard
                    }
            in
            case settled.next of
                Just nextSubject ->
                    let
                        batchId =
                            model.exoextBatch |> Maybe.andThen .batchId
                    in
                    ( { settledModel | exoextBatch = Just { batchId = batchId, remaining = settled.remaining, awaitingWrite = True } }
                    , Task.perform (ExoextWriteRequest { kind = exoextTrackedKind model, subject = nextSubject, batchId = batchId }) Time.now
                    )

                Nothing ->
                    ( { settledModel | exoextBatch = Nothing }, Cmd.none )


{-| The wire request `kind` a batch's NEXT sibling should be written with: the one the leg
currently in flight was written with (`Exoext.Lifecycle.PendingRequest.kind`). §4.1 siblings are by
definition the same request repeated over different subjects, so the tracker is the right and only
place the host can read this from — a continuation is written with no second press, so there is no
out-message to carry it.

`""` when the tracker is gone or carries no kind, which is not a failure: a run adopted from the
wire after a reload has no verb to recover (§4.3 carries none, and
[`Exoext.Lifecycle.recoverRun`](Exoext-Lifecycle#recoverRun) refuses to invent one). The host does
not resolve that — it passes the empty kind on, and the adapter decides what an unnamed request
means for its own wire (`CloudShield.Wire.encodeRequestBody`). Deciding here is exactly what would
put one extension's default verb back in the host.

-}
exoextTrackedKind : Model -> String
exoextTrackedKind model =
    model.exoextCard.pending |> Maybe.map .kind |> Maybe.withDefault ""


{-| Resolve the pending session marker against fresh metadata: clear it once the res slot holds a
body answering the same `requestId` (the publisher answered this request). A body answering a
different (earlier) requestId leaves the marker in place — a newer request is still in flight.

The host pulls the §7.1 res slot and owns the comparison, because the marker is its own; whether a
body is a reply at all, and which request it replies to, is the adapter's read
([`CloudShield.Reader.answeredRequestId`](CloudShield-Reader#answeredRequestId)).

-}
clearResolvedPendingEmbed : List OSTypes.MetadataItem -> Maybe Exoext.Lifecycle.PendingRequest -> Maybe Exoext.Lifecycle.PendingRequest
clearResolvedPendingEmbed metadata pending =
    pending
        |> Maybe.andThen
            (\p ->
                if
                    (Exoext.Transport.resultBodyFromMetadata metadata
                        |> Maybe.andThen CloudShield.Reader.answeredRequestId
                    )
                        == Just p.requestId
                then
                    Nothing

                else
                    Just p
            )


{-| Fold a fresh poll's response slot into the in-flight removal.

An acknowledgement that matches the tracked request's id settles it, and the two outcomes are
genuinely different pieces of work:

  - **Removed.** Drop the row from the loaded history immediately, and clear the history refresh key
    so the next poll refetches the index rather than waiting for a run transition that a removal
    never causes. The local drop is what makes the row disappear at the acknowledgement instead of a
    poll later; the refetch is what makes the publisher's archive, not this browser, the last word.
  - **Refused.** Keep the row and record the publisher's reason against the result it named, so the
    card can put it on that row with a way to try again.

An acknowledgement for some other request is ignored: the slot is single-occupancy but a stale
answer can still be sitting in it when a new request goes out.

-}
resolveExoextRemoval : List OSTypes.MetadataItem -> Model -> Model
resolveExoextRemoval metadata model =
    case ( model.exoextPendingDelete, exoextRemovalAck metadata ) of
        ( Just pending, Just ack ) ->
            if ack.requestId /= pending.requestId then
                model

            else if ack.ok then
                { model
                    | exoextPendingDelete = Nothing
                    , exoextDeleteError = Nothing
                    , exoextHistory =
                        RDPP.setData
                            (RDPP.DoHave
                                (RDPP.withDefault [] model.exoextHistory
                                    |> List.filter (\entry -> exoextRowResultId entry /= pending.subject)
                                )
                                pending.since
                            )
                            model.exoextHistory
                    , exoextHistoryRequestKey = Nothing
                    , exoextHistoryGeneration = model.exoextHistoryGeneration + 1
                }

            else
                { model
                    | exoextPendingDelete = Nothing
                    , exoextDeleteError = Just { resultId = pending.subject, message = ack.message }
                }

        _ ->
            model


{-| The removal acknowledgement sitting in the §7.1 response slot, if there is one. The host pulls
the body out of the envelope; recognizing it as an answer to a removal is the adapter's
([`CloudShield.Reader.removalAck`](CloudShield-Reader#removalAck)).
-}
exoextRemovalAck : List OSTypes.MetadataItem -> Maybe { requestId : String, resultId : Maybe String, ok : Bool, message : String }
exoextRemovalAck metadata =
    Exoext.Transport.resultBodyFromMetadata metadata
        |> Maybe.andThen CloudShield.Reader.removalAck


{-| A history row's own result id: its `requestId`, falling back to `batchId` for a legacy row that
carries none. The same identity the card projects rows under and the same one a removal names, kept
in step so a removed row is the row that disappears.
-}
exoextRowResultId : CloudShield.Wire.IndexEntry -> String
exoextRowResultId entry =
    Maybe.withDefault entry.batchId entry.requestId


{-| The removal state the card projects: which row is being removed right now, and the last refusal.

A removal that has waited past the §7.1 request timeout is reported as a failure rather than as
still in flight ([`Exoext.Lifecycle.requestTimedOut`](Exoext-Lifecycle#requestTimedOut)). Without
that, a publisher that never answers leaves a row reading "Removing…" forever — and, worse, leaves
the single request slot looking occupied, so every later press is refused to protect a request that
is not coming back.

-}
exoextRemovalState :
    Time.Posix
    -> Model
    ->
        ( Maybe String
        , Maybe { resultId : String, message : String }
        )
exoextRemovalState now model =
    case model.exoextPendingDelete of
        Just pending ->
            if Exoext.Lifecycle.requestTimedOut now pending then
                ( Nothing, Just { resultId = pending.subject, message = Exoext.Lifecycle.requestTimedOutMessage } )

            else
                ( Just pending.subject, model.exoextDeleteError )

        Nothing ->
            ( Nothing, model.exoextDeleteError )


syncExoextManifest : Project -> Exoext.Discovery.Sentinel -> String -> Model -> ( Model, Cmd Msg )
syncExoextManifest project sentinel etag model =
    case ( project.endpoints.swift, Exoext.Discovery.manifestObjectLocation sentinel ) of
        ( Nothing, _ ) ->
            ( exoextManifestError (Time.millisToPosix 0)
                etag
                {- @nonlocalized -} "The extension's manifest is stored in object storage, but this cloud has no Swift endpoint."
                model
            , Cmd.none
            )

        ( _, Nothing ) ->
            ( exoextManifestError (Time.millisToPosix 0) etag "The extension's manifest is stored in object storage, but its container or object name is missing." model
            , Cmd.none
            )

        ( Just swiftUrl, Just location ) ->
            if exoextManifestNeedsFetch etag model then
                ( { model
                    | exoextManifest = RDPP.setLoading model.exoextManifest
                    , exoextManifestRequestEtag = Just etag
                  }
                , Rest.Swift.requestGetObjectCapped
                    project
                    swiftUrl
                    location.container
                    location.objectName
                    Nothing
                    Exoext.Transport.manifestCapBytes
                    (\result ->
                        SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                            SharedMsg.ServerMsg model.serverUuid <|
                                SharedMsg.ReceiveExoextManifestObject etag result
                    )
                    |> Cmd.map SharedMsg
                )

            else
                ( model, Cmd.none )


{-| The object to fetch for the current res-slot body, if any. The host pulls the §7.1 res slot out
of metadata and hands the body, plus its own request record and the §3.1 prefix, to the adapter that
knows how its archive is laid out ([`CloudShield.Reader.resultObjectName`](CloudShield-Reader#resultObjectName)).
`Nothing` means nothing to fetch.
-}
exoextResultObjectName : Model -> Exoext.Discovery.Sentinel -> List OSTypes.MetadataItem -> Maybe String
exoextResultObjectName model sentinel metadata =
    Exoext.Transport.resultBodyFromMetadata metadata
        |> Maybe.andThen
            (CloudShield.Reader.resultObjectName model.exoextEmbedResultId (Maybe.withDefault "" sentinel.prefix))


syncExoextResultRef : Project -> Exoext.Discovery.Sentinel -> String -> List OSTypes.MetadataItem -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
syncExoextResultRef project sentinel etag metadata ( model, manifestCmd ) =
    case exoextResultObjectName model sentinel metadata of
        Just objectName ->
            case ( project.endpoints.swift, sentinel.container ) of
                ( Nothing, _ ) ->
                    ( exoextResultRefError (Time.millisToPosix 0)
                        etag
                        objectName
                        {- @nonlocalized -} "The extension's result is stored in object storage, but this cloud has no Swift endpoint."
                        model
                    , manifestCmd
                    )

                ( _, Nothing ) ->
                    ( exoextResultRefError (Time.millisToPosix 0) etag objectName "The extension's result is stored in object storage, but its container is missing." model
                    , manifestCmd
                    )

                ( Just swiftUrl, Just container ) ->
                    if exoextResultRefNeedsFetch etag objectName model then
                        ( { model
                            | exoextResultRef = RDPP.setLoading model.exoextResultRef
                            , exoextResultRefRequest = Just { etag = etag, objectName = objectName }
                          }
                        , Cmd.batch
                            [ manifestCmd
                            , Rest.Swift.requestGetObjectCapped
                                project
                                swiftUrl
                                container
                                objectName
                                Nothing
                                Exoext.Transport.resultCapBytes
                                (\result ->
                                    SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                        SharedMsg.ServerMsg model.serverUuid <|
                                            SharedMsg.ReceiveExoextResultObject etag objectName result
                                )
                                |> Cmd.map SharedMsg
                            ]
                        )

                    else
                        ( model, manifestCmd )

        _ ->
            ( model, manifestCmd )


{-| Fetch the archived-scan history index (`<prefix>results/index.json`) in the live loop. Keyed
on `Exoext.Transport.historyRefreshKey` (etag + run.seq + run.state), NOT the etag alone:
the etag is a content hash of the static manifest and does not move when a scan completes, so it
would leave history stale. The composite key advances on every run-state transition (and on a
getEmbed claim), so each transition triggers one refetch and a steady state suppresses it. Any
failure resolves to no history (fail-closed), never an error card. `store=metadata` has no
archive, so this only runs for `store=swift`.
-}
syncExoextIndex : Project -> Exoext.Discovery.Sentinel -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
syncExoextIndex project sentinel refreshKey ( model, priorCmd ) =
    case ( project.endpoints.swift, sentinel.container ) of
        ( Just swiftUrl, Just container ) ->
            if exoextIndexNeedsFetch refreshKey model then
                ( { model
                    | exoextHistory = RDPP.setLoading model.exoextHistory
                    , exoextHistoryRequestKey = Just refreshKey
                  }
                , Cmd.batch
                    [ priorCmd
                    , Rest.Swift.requestGetObjectCapped
                        project
                        swiftUrl
                        container
                        (CloudShield.Wire.indexObjectName (Maybe.withDefault "" sentinel.prefix))
                        (Just (refreshKey ++ ":" ++ String.fromInt model.exoextHistoryGeneration))
                        CloudShield.Wire.indexCapBytes
                        (\result ->
                            SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                SharedMsg.ServerMsg model.serverUuid <|
                                    SharedMsg.ReceiveExoextIndexObject refreshKey result
                        )
                        |> Cmd.map SharedMsg
                    ]
                )

            else
                ( model, priorCmd )

        _ ->
            ( { model
                | exoextHistory =
                    model.exoextHistory
                        |> RDPP.setData (RDPP.DoHave [] (Time.millisToPosix 0))
                        |> RDPP.setNotLoading Nothing
              }
            , priorCmd
            )


exoextIndexNeedsFetch : String -> Model -> Bool
exoextIndexNeedsFetch refreshKey model =
    model.exoextHistoryRequestKey /= Just refreshKey


{-| Store the decoded history index, fail-closed. First-fetch errors and over-cap/malformed bodies
resolve to fetched empty history (`[]`) rather than an error state. Refresh errors keep any stale
loaded rows visible. Stamped by the refresh key it was fetched for, and dropped if the current key
(etag + run slot) has since moved on.

**A failed fetch does NOT stamp the key**, and that is the fix for history that froze until a
reload. The key is a "this generation has been fetched" marker, so stamping it on a failure recorded
a fetch that never happened: nothing asked again until the run slot moved, which for a settled scan
can be hours away, or never. Leaving it unstamped makes the next poll retry, which is exactly what
the reload a researcher resorts to is doing by hand.

-}
receiveExoextIndex : Project -> Time.Posix -> String -> Result HttpErrorWithBody String -> Model -> Model
receiveExoextIndex project receivedTime refreshKey result model =
    if currentExoextHistoryKey project model == Just refreshKey then
        case result of
            Ok body ->
                { model
                    | exoextHistory =
                        RDPP.RemoteDataPlusPlus
                            (RDPP.DoHave (CloudShield.Wire.decodeIndex body) receivedTime)
                            (RDPP.NotLoading Nothing)
                    , exoextHistoryRequestKey = Just refreshKey
                }

            Err _ ->
                { model
                    | exoextHistory =
                        case model.exoextHistory.data of
                            RDPP.DoHave _ _ ->
                                RDPP.setNotLoading Nothing model.exoextHistory

                            RDPP.DontHave ->
                                RDPP.RemoteDataPlusPlus
                                    (RDPP.DoHave [] receivedTime)
                                    (RDPP.NotLoading Nothing)
                    , exoextHistoryRequestKey = Nothing
                }

    else
        model


receiveExoextManifest : Project -> Time.Posix -> String -> Result HttpErrorWithBody String -> Model -> Model
receiveExoextManifest project receivedTime etag result model =
    if currentExoextEtag project model == Just etag then
        case result |> Result.mapError Helpers.httpErrorWithBodyToString |> Result.andThen (Exoext.Transport.capBody Exoext.Transport.manifestCapBytes) of
            Ok body ->
                { model
                    | exoextManifest =
                        model.exoextManifest
                            |> RDPP.setData (RDPP.DoHave { etag = etag, body = body } receivedTime)
                            |> RDPP.setNotLoading Nothing
                    , exoextManifestRequestEtag = Just etag
                }

            Err error ->
                exoextManifestError receivedTime etag error model

    else
        model


receiveExoextResultRef : Project -> Time.Posix -> String -> String -> Result HttpErrorWithBody String -> Model -> Model
receiveExoextResultRef project receivedTime etag objectName result model =
    if currentExoextEtag project model == Just etag then
        case result |> Result.mapError Helpers.httpErrorWithBodyToString |> Result.andThen (Exoext.Transport.capBody Exoext.Transport.resultCapBytes) of
            Ok body ->
                { model
                    | exoextResultRef =
                        model.exoextResultRef
                            |> RDPP.setData (RDPP.DoHave { etag = etag, objectName = objectName, body = body } receivedTime)
                            |> RDPP.setNotLoading Nothing
                    , exoextResultRefRequest = Just { etag = etag, objectName = objectName }
                }

            Err error ->
                exoextResultRefError receivedTime etag objectName error model

    else
        model


exoextManifestBodyForEtag : String -> Model -> Maybe String
exoextManifestBodyForEtag etag model =
    case model.exoextManifest.data of
        RDPP.DoHave fetched _ ->
            if fetched.etag == etag then
                Just fetched.body

            else
                Nothing

        RDPP.DontHave ->
            Nothing


exoextManifestNeedsFetch : String -> Model -> Bool
exoextManifestNeedsFetch etag model =
    exoextManifestBodyForEtag etag model
        == Nothing
        && model.exoextManifestRequestEtag
        /= Just etag


{-| Whether the last object-store manifest fetch settled on an error (so an unresolved Swift body
reads as unavailable rather than still-loading). A `NotLoading (Just …)` refresh status is the
error state `exoextManifestError` records.
-}
exoextManifestErrored : Model -> Bool
exoextManifestErrored model =
    case model.exoextManifest.refreshStatus of
        RDPP.NotLoading (Just _) ->
            True

        _ ->
            False


effectiveExoextResultBody : String -> String -> Model -> Maybe String
effectiveExoextResultBody etag slotBody model =
    let
        resolved =
            Exoext.Transport.resolveResultBody slotBody

        fetchedRefBody =
            case ( Exoext.Transport.resultRefObjectName resolved, model.exoextResultRef.data ) of
                ( Just objectName, RDPP.DoHave fetched _ ) ->
                    if fetched.etag == etag && fetched.objectName == objectName then
                        Just fetched.body

                    else
                        Nothing

                _ ->
                    Nothing
    in
    Exoext.Transport.resultBody resolved fetchedRefBody


exoextResultRefNeedsFetch : String -> String -> Model -> Bool
exoextResultRefNeedsFetch etag objectName model =
    let
        fetched =
            case model.exoextResultRef.data of
                RDPP.DoHave body _ ->
                    body.etag == etag && body.objectName == objectName

                RDPP.DontHave ->
                    False

        requested =
            model.exoextResultRefRequest == Just { etag = etag, objectName = objectName }
    in
    not fetched && not requested


exoextManifestError : Time.Posix -> String -> String -> Model -> Model
exoextManifestError receivedTime etag error model =
    { model
        | exoextManifest =
            RDPP.RemoteDataPlusPlus RDPP.DontHave (RDPP.NotLoading (Just ( error, receivedTime )))
        , exoextManifestRequestEtag = Just etag
    }


exoextResultRefError : Time.Posix -> String -> String -> String -> Model -> Model
exoextResultRefError receivedTime etag objectName error model =
    { model
        | exoextResultRef =
            RDPP.RemoteDataPlusPlus RDPP.DontHave (RDPP.NotLoading (Just ( error, receivedTime )))
        , exoextResultRefRequest = Just { etag = etag, objectName = objectName }
    }


currentExoextEtag : Project -> Model -> Maybe String
currentExoextEtag project model =
    GetterSetters.serverLookup project model.serverUuid
        |> Maybe.map (.osProps >> .details >> .metadata >> exoextEtag)


{-| The current history refresh key (etag + run slot) from this instance's live metadata, used to
drop a stale index response whose key no longer matches. See `historyRefreshKey`.
-}
currentExoextHistoryKey : Project -> Model -> Maybe String
currentExoextHistoryKey project model =
    GetterSetters.serverLookup project model.serverUuid
        |> Maybe.map (.osProps >> .details >> .metadata >> Exoext.Transport.historyRefreshKey)


exoextEtag : List OSTypes.MetadataItem -> String
exoextEtag metadata =
    metadataValue "exoext.v1.etag" metadata |> Maybe.withDefault ""


metadataValue : String -> List OSTypes.MetadataItem -> Maybe String
metadataValue key metadata =
    metadata
        |> List.Extra.find (\item -> item.key == key)
        |> Maybe.map .value


{-| Resolve the manifest + provenance + live status for the card view. The body comes from
the POC metadata transport when the discovery sentinel is present and `store=metadata`;
otherwise it falls back to the embedded frozen `card.json` (a dev convenience so the card is
demoable locally without a publishing VM). The fail-closed renderer validates whichever body
is used. The live `statusOverride` is read from the §7.1 status slot on this instance's
metadata and correlated to the in-flight request by `seq`.
-}
exoextViewConfig : Bool -> Project -> Model -> Time.Posix -> Server -> CloudShield.Card.ViewConfig
exoextViewConfig approved project model currentTime server =
    let
        metadata =
            server.osProps.details.metadata

        maybeSentinel =
            Exoext.Discovery.readSentinel metadata

        maybeTransportBody =
            maybeSentinel
                |> Maybe.andThen
                    (\{ store } ->
                        case store of
                            Exoext.Discovery.StoreSwift ->
                                exoextManifestBodyForEtag (exoextEtag metadata) model

                            _ ->
                                Exoext.Discovery.manifestBodyFromMetadata metadata
                    )

        -- The wire-resolved manifest source + the header transport chip. The manifest arrives
        -- ONLY over the wire (no embedded fallback): a resolved body renders; a not-yet-resolved
        -- Swift body reads as loading (host shows quiet loading chrome) until it errors; a missing
        -- metadata body, or an errored fetch, reads as unavailable (host shows a muted line). The
        -- transport chip appears only when a real body resolved, naming how it arrived.
        ( manifestSource, transportLabel ) =
            case ( maybeSentinel, maybeTransportBody ) of
                ( Just { store }, Just body ) ->
                    ( CloudShield.Card.ManifestReady (unwrapManifestUi model.serverUuid body)
                    , Just
                        (case store of
                            Exoext.Discovery.StoreSwift ->
                                "object storage"

                            _ ->
                                "server metadata"
                        )
                    )

                ( Just { store }, Nothing ) ->
                    ( case store of
                        Exoext.Discovery.StoreSwift ->
                            -- The object body has not resolved yet: still loading (fetch in flight
                            -- or not yet requested) until the fetch errors, then unavailable.
                            if exoextManifestErrored model then
                                CloudShield.Card.ManifestUnavailable

                            else
                                CloudShield.Card.ManifestLoading

                        _ ->
                            -- Metadata mode: the chunks ride the same server poll as the sentinel,
                            -- so an absent / undecodable body is genuinely unavailable, not loading.
                            CloudShield.Card.ManifestUnavailable
                    , Nothing
                    )

                ( Nothing, _ ) ->
                    -- Unreachable in practice: the card is gated on the sentinel being present.
                    ( CloudShield.Card.ManifestUnavailable, Nothing )

        statusOverride =
            exoextEffectiveStatusOverride currentTime metadata model

        resultBody =
            Exoext.Transport.resultBodyFromMetadata metadata
                |> Maybe.andThen (\body -> effectiveExoextResultBody (exoextEtag metadata) body model)

        -- The origin-pin for the catalog `Iframe`: the instance's own floating IPs, each mapped
        -- to its `https://<ip>.sslip.io` origin (dotted quad, matching the Let's Encrypt cert and
        -- the bridge's EMBED_PUBLIC_BASE). The renderer emits an `<iframe>` only for a `src` whose
        -- origin is an exact member of this list; anything else self-hides.
        allowedIframeOrigins =
            GetterSetters.getServerFloatingIps project server.osProps.uuid
                |> List.map (\ip -> "https://" ++ ip.address ++ ".sslip.io")

        -- The object the current embed result points at, built by the SAME function that decides
        -- what to fetch (`syncExoextResultRef`), so the read side and the fetch side can never
        -- disagree about which body belongs to the scan on screen.
        archivedResultObjectName =
            maybeSentinel
                |> Maybe.andThen (\sentinel -> exoextResultObjectName model sentinel metadata)

        -- The adapter's read side, all three parts of it. The host has done its half — polled the
        -- instance, pulled the §7.1 slots, fetched the capped object, held the etag and the clock —
        -- and hands that over rather than deciding what any of it means.
        readerProjection =
            exoextReaderProjection metadata archivedResultObjectName currentTime statusOverride resultBody model

        results =
            readerProjection.results

        embedUrl =
            readerProjection.embedUrl

        -- The elapsed descriptor for the card's frozen confirmation line, keyed on the tracked
        -- request's wall-clock seq and the correlated run state.
        elapsedTimer =
            CloudShield.Reader.completionTimer
                (Maybe.map .seq model.exoextCard.pending)
                (statusOverride |> Maybe.map .state |> Maybe.withDefault "queued")
                resultBody

        -- The row display state (left column). The RAW status slot drives the reader logic above;
        -- what a row says while a run is live is the adapter's word to compose.
        displayStatusOverride =
            CloudShield.Reader.rowStatus statusOverride
                (Maybe.map .seq model.exoextCard.pending)
                (Time.posixToMillis currentTime)

        ( removingResultId, removeError ) =
            exoextRemovalState currentTime model
    in
    { approved = approved
    , sourceName = server.osProps.name
    , manifest = manifestSource
    , transportLabel = transportLabel
    , transportWarning = exoextTransportWarning project model metadata maybeSentinel resultBody
    , runOutcome = exoextRunOutcome metadata statusOverride
    , scanTimer = elapsedTimer
    , statusOverride = displayStatusOverride

    -- The targets still waiting for their turn at the single §7.1 request slot. Projecting them
    -- keeps a batch looking the way it did before a reload: the tail is durable (`exoext.batch.v1`)
    -- and resumes on its own, but the optimistic `queued` badges the card set when the batch started
    -- are session-local, so without this a restored batch's untouched targets read as idle while
    -- they are demonstrably still coming.
    , queuedTargets = model.exoextBatch |> Maybe.map .remaining |> Maybe.withDefault []
    , results = results
    , history =
        case maybeSentinel of
            Just { store } ->
                if store == Exoext.Discovery.StoreSwift then
                    { rows = RDPP.withDefault [] model.exoextHistory
                    , loading = RDPP.isLoading model.exoextHistory
                    , loaded = RDPP.gotData model.exoextHistory
                    }

                else
                    { rows = [], loading = False, loaded = True }

            Nothing ->
                { rows = [], loading = False, loaded = True }
    , activeResultId = readerProjection.activeResultId
    , pendingResultId = readerProjection.pendingResultId
    , erroredResultId = readerProjection.erroredResultId
    , expiredResultId = readerProjection.expiredResultId
    , removingResultId = removingResultId
    , removeError = removeError

    -- The §7.1 guard, projected so the manifest can disable the affordances it would swallow.
    -- Exactly the predicate `update` applies to every request press, read from the same live wire
    -- state, so a control is greyed when (and only when) pressing it would do nothing.
    --
    -- The two keys carry the same value and stay two keys anyway: they are the manifest's names for
    -- two groups of controls, and one guard answering both is a fact about this host's §7.1
    -- transport rather than a promise to the manifest. A transport with more than one slot would
    -- separate them again without a manifest change.
    , requestBusy = exoextRequestBlocked project model
    , scanBusy = exoextRequestBlocked project model

    -- Which row can be stopped, and the request id a stop must name. Derived from the RAW
    -- `statusOverride`, never the display-composed one: the decision is about the run's real state,
    -- not about the string currently on its badge.
    , cancellableRun = exoextCancellableRun currentTime metadata statusOverride model

    -- The row whose stop has been asked for but not yet answered, projected as a state token (the
    -- manifest owns the word). Same raw-`statusOverride` discipline and the same reason.
    , stoppingTargetId = exoextStoppingTarget currentTime metadata statusOverride model
    , sessionOpen = readerProjection.sessionOpen
    , allowedIframeOrigins = allowedIframeOrigins
    , embedUrl = embedUrl
    , embedState = readerProjection.embedState

    -- The results iframe is now the catalog's origin-pinned `Iframe` element (bound to the scan
    -- result's embedUrl). The old raw/unpinned demo panel is disabled to avoid a second, confusing
    -- iframe; `Nothing` hides the panel and its toggle entirely.
    , demoIframeUrl = Nothing

    -- The publishing instance's own `exoext.v1.health.*` report, read off the same metadata poll
    -- everything else here comes from. It is what the card falls back to when there is no manifest
    -- to render, and what the health strip under a rendered card is drawn from.
    , health = Exoext.Health.read metadata
    , now = currentTime
    }


{-| How the tracked run ended, paired with the publisher's own reason when it has one.

The state comes from the correlated status override — so this reports the run the page is actually
tracking and never some other run's leftover slot — and the reason from `exoext.v1.run.error` on the
same slot. The host neither reads the sentence nor writes one: it carries the publisher's words to
the adapter, which decides which states are worth a line and what to call them.

`Nothing` when no run is correlated, which is also what keeps a fresh page quiet.

-}
exoextRunOutcome : List OSTypes.MetadataItem -> Maybe { targetId : String, state : String } -> Maybe { state : String, error : Maybe String }
exoextRunOutcome metadata statusOverride =
    statusOverride
        |> Maybe.map
            (\override ->
                { state = override.state
                , error =
                    Exoext.Transport.runStatusFromMetadata metadata
                        |> Maybe.andThen .error
                }
            )


{-| The run state a row should show, from whichever of the two sources can name one.

The wire is preferred and unchanged ([`exoextStatusOverride`](#exoextStatusOverride)). What is new is
the fallback, and it closes a contradiction rather than adding a feature: between writing a request
and the publisher echoing its seq, the wire says nothing about that run, so the row fell through to
its durable state and read **idle** — while the very same host record was simultaneously offering
that row a **Stop** control and greying every Scan button on the card. Three controls, three
different accounts of what was happening, and the one the eye lands on said nothing was.

So when the wire cannot name the run, the host's own pre-echo record does
([`exoextPreEchoRun`](#exoextPreEchoRun)) — the same record the Stop control is already derived from,
which is what makes the three agree by construction. It carries all of that function's guards, a
stale request included, so a run the host has stopped believing cannot reappear as a badge.

-}
exoextEffectiveStatusOverride : Time.Posix -> List OSTypes.MetadataItem -> Model -> Maybe { targetId : String, state : String }
exoextEffectiveStatusOverride now metadata model =
    case exoextStatusOverride metadata model of
        Just override ->
            Just override

        Nothing ->
            exoextPreEchoRun now metadata model
                |> Maybe.map (\run -> { targetId = run.targetId, state = run.state })


{-| The live run projected onto its own row: the tracked request says WHICH row, the §7.1 status
slot says what state to draw, and the `seq` correlation is what ties the two together. `Nothing`
when nothing is tracked or the slot reports a different run, in which case the row falls back to the
card's durable `scanState`.

The tracked request may be one this session wrote or one adopted from the wire
([`recoverExoextRun`](#recoverExoextRun)) — this function cannot tell, and that is the point: a
recovered run projects through exactly the same path as a locally-issued one.

-}
exoextStatusOverride : List OSTypes.MetadataItem -> Model -> Maybe { targetId : String, state : String }
exoextStatusOverride metadata model =
    case ( Exoext.Transport.runStatusFromMetadata metadata, model.exoextCard.pending ) of
        ( Just status, Just pending ) ->
            if status.seq == pending.seq then
                Just { targetId = pending.subject, state = status.state }

            else
                Nothing

        _ ->
            Nothing


{-| The run the stop affordances are about, resolved to everything they need: WHICH row it is on,
what the publisher calls it, what state it is in, and whether a stop is already pending for it. One
resolution, three consumers (`exoextCancellableRun`, `exoextStoppingTarget`, and the
`stopping`/`cancellable` row projection through them), so they can never disagree about which run is
on screen.

There are TWO sources, and which one applies is decided by one question: has the §7.1 status slot
echoed the seq of the request this host is tracking?

1.  **Not yet echoed ⇒ the host's own record** ([`exoextPreEchoRun`](#exoextPreEchoRun)). The window
    between writing a request and the publisher claiming it and reporting `run.*` is a whole poll
    interval wide, and for that window the wire carries no run for this request at all — either
    nothing, or the settled record of the PREVIOUS leg of a draining batch. Resolving purely from
    the wire therefore named nothing and offered no control, so the head of a batch (the one target
    actually about to be scanned) was the only one that could not be called off.
2.  **Echoed ⇒ the wire** ([`exoextWireRun`](#exoextWireRun)), unchanged and still authoritative for
    everything durable, including after a reload where the record is gone.

This is the same shape as the WP10 `stopping` fix, and for the same reason: the wire is the truth
but it LAGS, the host already knows what it just wrote, so the record covers the immediate window
and the wire covers the durable one.

-}
exoextRunControl :
    Time.Posix
    -> List OSTypes.MetadataItem
    -> Maybe { targetId : String, state : String }
    -> Model
    -> Maybe { targetId : String, requestId : String, state : String, stopping : Bool }
exoextRunControl now metadata statusOverride model =
    case exoextPreEchoRun now metadata model of
        Just run ->
            Just run

        Nothing ->
            exoextWireRun now metadata statusOverride model


{-| The tracked request the §7.1 status slot has not echoed yet, resolved as a stoppable run from
the host's OWN record of what it wrote.

Both halves of the identity come out of the record and neither is a guess: the target is the
request's `subject`, and the request id is minted from the seq by `exoextRequestId`, which is
deterministic — the same id the publisher will be handed when it does claim the slot. So a stop
pressed in this window names exactly the request that is about to run.

**Which §7.1 cancel this uses, and why.** §7.1 describes a pre-claim cancel as the host bumping or
clearing the request slot, and the explicit `exoext.v1.req.cancel` channel as the claimed case. This
takes the cancel channel for both, because the host cannot tell the two apart here: no run status
only means the publisher has not REPORTED, not that it has not claimed, and a slot bump aimed at an
unclaimed request would land on a run already in flight and leave it nameable by nobody. The cancel
channel is additive, names a request id the publisher can match whenever it gets to the slot, and is
already what `exoextStopRequested` writes — so the pre-echo window needs no second stop mechanism.

The guards are the wire path's guards, deliberately: an implausibly
[stale](Exoext-Lifecycle#runStale) tracked request resolves to `Nothing` (the same safety valve, on
the same clock, since `seq` IS the wall-clock moment of the write), and `stopping` is armed from the
same two sources. Nothing here can weaken the terminal-run guard: the branch only runs while the
wire says nothing about this request, so there is no terminal state to override.

-}
exoextPreEchoRun :
    Time.Posix
    -> List OSTypes.MetadataItem
    -> Model
    -> Maybe { targetId : String, requestId : String, state : String, stopping : Bool }
exoextPreEchoRun now metadata model =
    model.exoextCard.pending
        |> Maybe.Extra.filter (\pending -> not (exoextRunEchoes pending.seq metadata))
        |> Maybe.Extra.filter
            (\pending ->
                not
                    (Exoext.Lifecycle.runStale now
                        { seq = pending.seq
                        , state = Exoext.Lifecycle.correlatedRunState pending.seq metadata
                        }
                    )
            )
        |> Maybe.map
            (\pending ->
                let
                    requestId =
                        exoextRequestId pending.seq

                    -- By construction of the filter above this is `"queued"` — the state
                    -- `correlatedRunState` documents for a tracked request the slot does not
                    -- carry. Taken from there rather than written as a literal so the host keeps
                    -- one definition of what an unreported request reads as.
                    state =
                        Exoext.Lifecycle.correlatedRunState pending.seq metadata
                in
                { targetId = pending.subject
                , requestId = requestId
                , state = state
                , stopping = exoextRunStopping metadata model { requestId = requestId, state = state }
                }
            )


{-| Whether the §7.1 status slot is reporting the run of `seq`. False when the slot is absent
entirely, and false when it carries some other seq — the settled previous leg of a draining batch is
the common case, and it is exactly as silent about this request as an empty slot is.
-}
exoextRunEchoes : Int -> List OSTypes.MetadataItem -> Bool
exoextRunEchoes seq metadata =
    Exoext.Transport.runStatusFromMetadata metadata
        |> Maybe.map (\status -> status.seq == seq)
        |> Maybe.withDefault False


{-| The run the §7.1 status slot is reporting, resolved to what the stop affordances need.

Identity has to resolve on both halves or there is nothing to offer — a stop control that cannot name
its request would write a value the publisher can never match. Each half is taken from the wire first
and from the host's own record second:

  - the **target** comes from `run.target` (§4.3), else from the tracked request's subject when the
    run slot correlates to it. The wire is preferred because it is also right after a reload.
  - the **request id** comes from `run.requestId`, else from the id this host minted for that seq.
    The fallback keeps a stop working against a publisher that reports no request id, since the
    minting is deterministic in the seq (see `exoextRequestId`).

A [stale](Exoext-Lifecycle#runStale) run resolves to `Nothing`, and this is the OTHER half of the
safety valve rather than a detail. `exoextAbandonStaleRun` releases the tracker, but the run slot's
own §4.3 descriptors need no tracker to resolve a target and a request id — so without this gate a
run the host has stopped believing would keep a live Stop control on a row that otherwise reads
idle, and a press on it would arm `stopping` against a publisher that answers nothing, wedging the
row in a second, worse state. A control nobody is listening to is not an escape hatch; starting a
fresh scan is, and that is what the valve restores.

-}
exoextWireRun :
    Time.Posix
    -> List OSTypes.MetadataItem
    -> Maybe { targetId : String, state : String }
    -> Model
    -> Maybe { targetId : String, requestId : String, state : String, stopping : Bool }
exoextWireRun now metadata statusOverride model =
    Exoext.Transport.runStatusFromMetadata metadata
        |> Maybe.Extra.filter
            (\status -> not (Exoext.Lifecycle.runStale now { seq = status.seq, state = status.state }))
        |> Maybe.andThen
            (\status ->
                let
                    -- The tracked request, only when the run slot is reporting THAT run.
                    correlated =
                        model.exoextCard.pending
                            |> Maybe.andThen
                                (\pending ->
                                    if pending.seq == status.seq then
                                        Just pending

                                    else
                                        Nothing
                                )

                    targetId =
                        Maybe.Extra.or status.target (statusOverride |> Maybe.map .targetId)

                    requestId =
                        Maybe.Extra.or status.requestId
                            (correlated |> Maybe.map (\_ -> exoextRequestId status.seq))
                in
                Maybe.map2
                    (\target request ->
                        { targetId = target
                        , requestId = request
                        , state = status.state
                        , stopping = exoextRunStopping metadata model { requestId = request, state = status.state }
                        }
                    )
                    targetId
                    requestId
            )


{-| Whether a stop is pending for a run. The WP10 fix for "the stop did nothing visible", shared by
both resolution paths so neither can acknowledge a press the other would ignore. Armed by either of
two sources, and both are needed:

1.  **The wire** (`Exoext.Lifecycle.runStopping`) — the cancel channel on the publishing VM's own
    metadata names this run. This is the durable half: the host wrote that channel, so it is still
    there after a reload, which is precisely the case the session-local record cannot cover.
2.  **This session's own record** (`exoextCancelRequestId`) — set the instant the press is dispatched.
    This is the immediate half: the wire write is an HTTP round-trip and only becomes visible on the
    NEXT server poll, so between the press and that poll the wire still says nothing at all. Without
    this the row would keep reading `scanning` for seconds after a press that plainly landed, which
    is the bug in miniature.

Both are gated on the run being non-terminal, which is what retires the state: a `done`/`cancelled`
run is not stopping, it has stopped, and its own terminal state is the truthful badge.

-}
exoextRunStopping : List OSTypes.MetadataItem -> Model -> { requestId : String, state : String } -> Bool
exoextRunStopping metadata model run =
    Exoext.Lifecycle.runStopping run metadata
        || ((model.exoextCancelRequestId == Just run.requestId)
                && not (Exoext.Lifecycle.isTerminalRunState run.state)
           )


{-| The one target row whose run can be stopped, and the §4.1 request id the stop must name
(`exoextRunControl` resolves both).

**A stop is offered for any run that is not terminal.** The rule is stated that way round on
purpose. §4.4 fixes exactly four states in which a publisher is done — `done`, `error`, `cancelled`,
`expired` ([`Exoext.Lifecycle.terminalRunStates`](Exoext-Lifecycle#terminalRunStates)) — and says
nothing about the rest, because the rest are the publisher's own vocabulary: `queued` and `running`
are the two the contract names, but a publisher is free to report `snapshotting`, `booting-clone`,
or any phase word of its own. Every one of those is, by the contract's own definition, a run the
publisher has not finished, and therefore a run it still has work to abandon. An allowlist of the
states this host happens to recognize would silently withhold the stop from all of them, which is
the same host-decides-for-the-publisher mistake the `stopping` projection below already refuses to
make — and it would be the host holding an opinion about verbs it has never seen.

`Nothing` in two cases. On a terminal run, because a stop would be a lie and there is nothing on the
wire for the publisher to act on. And on a run that is already `stopping`:
a second press names the same request the channel already carries, so it can only be a no-op, and the
row says so with its `stopping` state rather than with a control that does nothing. That the control
also withdraws is now the SECOND acknowledgement of a press, no longer the only one — which is what
let the state token replace the host-owned "cancelling" display string this comment used to rule out.

-}
exoextCancellableRun :
    Time.Posix
    -> List OSTypes.MetadataItem
    -> Maybe { targetId : String, state : String }
    -> Model
    -> Maybe { targetId : String, requestId : String }
exoextCancellableRun now metadata statusOverride model =
    exoextRunControl now metadata statusOverride model
        |> Maybe.andThen
            (\run ->
                if not (Exoext.Lifecycle.isTerminalRunState run.state) && not run.stopping then
                    Just { targetId = run.targetId, requestId = run.requestId }

                else
                    Nothing
            )


{-| The row whose run is stopping, projected as `ViewConfig.stoppingTargetId` and from there as that
row's `"stopping"` state token. The host projects the TOKEN and nothing else: the manifest owns the
word the researcher reads, exactly as it owns `queued` / `scanning` / `done`.

Deliberately NOT gated on whether a stop could still be OFFERED for the run. Those are different
questions: offering a stop is about work the publisher has left to abandon, whereas whether a stop is
PENDING is a fact about the wire, and a publisher that has already settled the run is still a
publisher that was asked to stop.

-}
exoextStoppingTarget :
    Time.Posix
    -> List OSTypes.MetadataItem
    -> Maybe { targetId : String, state : String }
    -> Model
    -> Maybe String
exoextStoppingTarget now metadata statusOverride model =
    exoextRunControl now metadata statusOverride model
        |> Maybe.andThen
            (\run ->
                if run.stopping then
                    Just run.targetId

                else
                    Nothing
            )


{-| Where an `exoext.navigate` press actually goes.

§5.4 applied to navigation: the id is re-resolved against Exosphere's OWN server list before
anything moves, so an extension can send the researcher to a page of their own project and nowhere
else. An id this project does not have is ignored, which is both the fail-closed answer and the
honest one — there is no page to go to.

Naming the destination is deliberately all the extension may do. It supplies an instance id, not a
URL and not a route, so the reachable set is exactly the set of pages the researcher could have
reached by clicking around their own project.

-}
exoextNavigation : Project -> String -> SharedMsg.SharedMsg
exoextNavigation project instanceId =
    case GetterSetters.serverLookup project instanceId of
        Just _ ->
            SharedMsg.NavigateToRoute
                (Route.ProjectRoute (GetterSetters.projectIdentifier project)
                    (Route.ServerDetail instanceId)
                )

        Nothing ->
            SharedMsg.NoOp


{-| The §4.1 `requestId` this host writes for a request-slot seq. Deterministic in the seq (which is
wall-clock millis), which is what lets a cancel name a run whose id the publisher never echoed back.
One definition, used by the request writers and by `exoextCancellableRun`, so the two can never
disagree about what a run is called.
-}
exoextRequestId : Int -> String
exoextRequestId seq =
    "exoext-req-" ++ String.fromInt seq


{-| Stamp the host's half of the adapter's read side and ask for the projection back.

This is the read-side twin of the request seam: everything on this side of the call is framing the
host owns — the §7.1 res slot pulled out of metadata, the §3.1 etag, the object it last fetched, the
in-flight session request, the shared client clock, the researcher's dismissal — and nothing on this
side decides what any of it means. Which body wins, when a session is stale, which row is being
viewed and what it should say are all the adapter's (`CloudShield.Reader.projection`).

`resultBody` is the res-slot body the host has already matched to the current manifest etag;
`archivedResultObjectName` is what `exoextResultObjectName` resolved for it, passed rather than
recomputed so the fetch side and the read side can never name two different objects.

-}
exoextReaderProjection : List OSTypes.MetadataItem -> Maybe String -> Time.Posix -> Maybe { targetId : String, state : String } -> Maybe String -> Model -> CloudShield.Reader.Projection
exoextReaderProjection metadata archivedResultObjectName currentTime statusOverride resultBody model =
    CloudShield.Reader.projection
        { resSlotBody = Exoext.Transport.resultBodyFromMetadata metadata
        , resultBody = resultBody
        , manifestEtag = exoextEtag metadata
        , archivedObjectName = archivedResultObjectName
        , fetchedArchive =
            case model.exoextResultRef.data of
                RDPP.DoHave fetched _ ->
                    Just fetched

                RDPP.DontHave ->
                    Nothing
        , currentTime = currentTime
        , runStatus = statusOverride
        , pendingSession = model.exoextPendingEmbed
        , sessionRequest = model.exoextEmbedResultId
        , sessionDismissed = model.exoextSessionDismissed
        }


{-| The one warning line under the card, whichever of two kinds it is. A publisher that had to cut
its own payload down says so through the adapter
([`CloudShield.Reader.truncationWarning`](CloudShield-Reader#truncationWarning)) and wins the slot;
otherwise the host reports its own transport failures (no Swift endpoint, a failed object read).
-}
exoextTransportWarning : Project -> Model -> List OSTypes.MetadataItem -> Maybe Exoext.Discovery.Sentinel -> Maybe String -> Maybe String
exoextTransportWarning project model metadata maybeSentinel resultBody =
    case resultBody |> Maybe.andThen CloudShield.Reader.truncationWarning of
        Just warning ->
            Just warning

        Nothing ->
            case maybeSentinel of
                Just { store } ->
                    if store == Exoext.Discovery.StoreSwift then
                        case project.endpoints.swift of
                            Nothing ->
                                Just
                                    {- @nonlocalized -} "The extension's manifest is stored in object storage, but this cloud has no Swift endpoint."

                            Just _ ->
                                exoextReadErrorForEtag (exoextEtag metadata) model

                    else
                        Nothing

                Nothing ->
                    Nothing


exoextReadErrorForEtag : String -> Model -> Maybe String
exoextReadErrorForEtag etag model =
    let
        manifestError =
            case ( model.exoextManifestRequestEtag, model.exoextManifest.refreshStatus ) of
                ( Just requestedEtag, RDPP.NotLoading (Just ( error, _ )) ) ->
                    if requestedEtag == etag then
                        Just ("Extension object-storage manifest read failed: " ++ error)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    case manifestError of
        Just _ ->
            manifestError

        Nothing ->
            case ( model.exoextResultRefRequest, model.exoextResultRef.refreshStatus ) of
                ( Just request, RDPP.NotLoading (Just ( error, _ )) ) ->
                    if request.etag == etag then
                        Just ("Extension object-storage result read failed: " ++ error)

                    else
                        Nothing

                _ ->
                    Nothing


{-| Extract the json-render `ui` body from a §1 manifest envelope for the renderer.

The CloudShield agent publishes the full manifest envelope (`{schemaVersion, catalog,
publisher, ui: {root, elements, state}}`, §1); the renderer wants the bare json-render spec.
We unwrap `.ui`. Two host trust checks happen here, fail-closed:

  - **§5.1 self-instance placement.** If the envelope's `publisher.instanceId` is present and
    does not equal the instance whose page this is, the manifest is dropped (a VM may only
    place UI on its own page) — we return an empty body, which the fail-closed renderer decodes
    to its error notice rather than rendering anyone else's UI.
  - A body that is already a bare spec (no `ui` field — e.g. the dev seed) is used as-is.

-}
unwrapManifestUi : OSTypes.ServerUuid -> String -> String
unwrapManifestUi pageInstanceId body =
    let
        publisherId =
            Decode.decodeString (Decode.at [ "publisher", "instanceId" ] Decode.string) body
                |> Result.toMaybe
    in
    case publisherId of
        Just pid ->
            if pid == pageInstanceId then
                case Decode.decodeString (Decode.field "ui" Decode.value) body of
                    Ok ui ->
                        Encode.encode 0 ui

                    Err _ ->
                        body

            else
                -- §5.1 violation: published-by claims another instance → fail closed to an
                -- empty (undecodable) body rather than rendering another instance's UI.
                ""

        Nothing ->
            -- No envelope (bare spec, e.g. dev seed) → render directly.
            body


{-| Write the §7.1 metadata request-slot for one confirmed request. §7.1 admits a single request at
a time, so a multi-target press calls this once per target: the head from the confirmed press, every
sibling from `advanceExoextBatch` as the previous run settles, all carrying the batch's shared
`batchId`. The slot is written on the **publishing instance's own** metadata (the viewed instance).

**This is the whole of what the host knows about a request.** It stamps the seq, the ids, the
timestamp and the §5.4-resolved subject into an `Exoext.Transport.RequestContext`, asks the adapter
for a body of `kind`, and chunks whatever comes back into the slot. It never composes a body, never
reads one, and holds no opinion about what any kind means — a kind the adapter declines to encode
writes nothing at all, which is the fail-closed answer for a verb this build cannot speak.

The `kind` reaches here from the press that started the request (`CloudShield.Card.OutMsg`) or, for
a batch continuation, from the leg already in flight (`exoextTrackedKind`).

POC limitations (dropped at `store=swift`, Phase 1b): `requestId` is seq-derived rather than a
UUID, and a continuation whose request is never claimed has no per-request expiry of its own —
the batch simply stops advancing. `createdAt` is a real wall-clock timestamp (`Time.now` via
`ExoextWriteRequest`), so the publisher's §4.4 expiry guard works. The §4.1 JSON bytes are pinned
in `Tests.CloudShield.Wire`; the §7.1 framing in `Tests.CloudShield.Card`.

-}
writeExoextRequestCmd : Project -> Model -> List CloudShield.Card.Instance -> { kind : String, seq : Int, subject : String, batchId : Maybe String } -> Time.Posix -> Cmd Msg
writeExoextRequestCmd project model instances req now =
    let
        context =
            { requestId = exoextRequestId req.seq
            , batchId = req.batchId
            , createdAt = ISO8601.toString (ISO8601.fromPosix now)
            , projectId = project.auth.project.uuid
            , subject =
                { id = req.subject

                -- §5.4: re-resolve the subject id against Exosphere's own instance list, so a
                -- publisher cannot make a request claim a target it invented. A subject that is
                -- not an instance at all (an archived result, say) keeps its raw id.
                , name =
                    instances
                        |> List.Extra.find (\i -> i.id == req.subject)
                        |> Maybe.map .name
                        |> Maybe.withDefault req.subject
                }
            }
    in
    case CloudShield.Wire.encodeRequestBody req.kind context of
        Just body ->
            -- One atomic POST for all request-slot keys. Firing one request per key
            -- concurrently races on Nova's single metadata row and silently drops writes,
            -- leaving a half-written (unparseable) request slot the agent never picks up.
            Rest.Nova.requestSetServerMetadataItems project model.serverUuid (Exoext.Transport.reqSlotMetadata req.seq body)
                |> Cmd.map SharedMsg

        Nothing ->
            Cmd.none


{-| What closing the results pane does to the host's own state. Nothing is written to the wire, no
request is cancelled and no archived result is touched — a session is a view, and the same one can be
re-opened.

Two fields, and the second is the WP10 fix. Closing the pane also has to retire the request that was
going to FILL it: `exoextPendingEmbed` is what the history row reads as "Opening…", and only a
matching wire result clears it. A researcher who closes the pane while one is in flight has said they
no longer want what it would show, so leaving the marker set left the row advertising an arrival that
nothing would then display — indefinitely, since the pane it would have opened is closed. Dropping it
also lets the fast poll settle (`exoextRequestsPending`).

`exoextEmbedResultId` (the field) deliberately does NOT go with it. It is not the in-flight marker but the record
of WHICH archived result a res-slot embed result names, and it outlives the request by design (see
the field's own comment). A dismissal leaves that result standing on the wire, along with the per-row
expired / errored states derived from it; clearing the record would degrade those to the §2.2
`batchId`, which siblings share, and flag every sibling row of the batch at once.

-}
exoextDismissSession : Model -> Model
exoextDismissSession model =
    { model
        | exoextSessionDismissed = True
        , exoextPendingEmbed = Nothing
    }


{-| Route a confirmed stop. There are two ways to stop a target and the host is the only party that
can tell them apart, because only the host owns the batch:

  - **A run on the wire** (`requestId` non-empty) — write the cancel channel naming it, exactly as
    before. Only the publisher can abandon work it has already started.
  - **A target still in the undrained tail** (empty `requestId`, and a `targetId` the batch is still
    holding) — nothing has been written for it and nothing ever will be, so the stop is simply to
    take it out of the queue. Host-local by construction: no wire write, no cancel channel, and
    above all nothing that touches the run currently in flight. Writing a cancel here would be
    actively wrong — the channel names ONE request and the only request on the wire is the live
    run's, so a queued row's stop would kill a different target's scan.

Anything else is a no-op: an empty `requestId` with a `targetId` the batch is not holding names
nothing this host can act on (a stale row from before the tail drained, say), and the correct answer
to a press the host cannot attribute is to do nothing at all.

-}
exoextStopRequested : Project -> { requestId : String, targetId : String } -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
exoextStopRequested project req model =
    if not (String.isEmpty req.requestId) then
        let
            cancelled =
                exoextCancelRequested req.requestId model
        in
        ( cancelled
        , writeCancelRequestCmd project model req.requestId
        , exoextBatchSharedMsg project cancelled
        )

    else if exoextTailHolds req.targetId model then
        let
            dropped =
                exoextDropFromTail req.targetId model
        in
        ( dropped, Cmd.none, exoextBatchSharedMsg project dropped )

    else
        ( model, Cmd.none, SharedMsg.NoOp )


{-| Whether the undrained batch tail is still holding this target, i.e. whether removing it is a
thing this host can actually do.
-}
exoextTailHolds : String -> Model -> Bool
exoextTailHolds targetId model =
    model.exoextBatch
        |> Maybe.map (\batch -> List.member targetId batch.remaining)
        |> Maybe.withDefault False


{-| Take one target out of the undrained tail, the whole of a queued row's stop.

Two things, and the second is what makes the row look stopped: the target leaves `remaining` so no
request is ever written for it, and its optimistic `queued` badge is dropped (`abandonScanState`)
so it reverts to the absent-means-idle default that also makes its Scan action pressable again. The
same pair `exoextCancelRequested` applies to a whole abandoned tail, applied to one member.

Removing the LAST member leaves the batch exactly as a drained one: `exoextBatch` cleared, which is
what `advanceExoextBatch` does when it runs out of subjects, and what `exoextBatchSharedMsg` then
turns into a forgotten stored record. The one exception is a decided-but-unwritten continuation
(`awaitingWrite`): that flag is the §7.1 pre-write guard, and dropping the record while a write is
on its way would open exactly the window it exists to close, so the emptied record is kept until the
write lands and the normal drain retires it.

Nothing here touches the live run, its tracker, or the cancel channel. A researcher removing a queued
target has said nothing about the scan that is actually running.

-}
exoextDropFromTail : String -> Model -> Model
exoextDropFromTail targetId model =
    case model.exoextBatch of
        Just batch ->
            let
                remaining =
                    List.filter (\subject -> subject /= targetId) batch.remaining
            in
            { model
                | exoextBatch =
                    if List.isEmpty remaining && not batch.awaitingWrite then
                        Nothing

                    else
                        Just { batch | remaining = remaining }
                , exoextCard = CloudShield.Card.abandonScanState [ targetId ] model.exoextCard
            }

        Nothing ->
            model


{-| What a confirmed stop does to the host's own state, alongside writing the cancel channel.

Three things, and the second is the judgment call:

1.  Remember the request that was stopped, so the run withdraws its Cancel control.
2.  **End the batch.** A researcher stopping a scan does not expect the next target to start on its
    own a moment later, so a stop is taken as a stop of the whole batch, not just its current leg.
3.  Drop the abandoned targets' optimistic `queued` badges. `requestScan` set those up front to make
    the press read as accepted; with no request coming, no terminal state would ever replace them.

-}
exoextCancelRequested : String -> Model -> Model
exoextCancelRequested requestId model =
    { model
        | exoextCancelRequestId = Just requestId
        , exoextBatch = Nothing
        , exoextCard =
            CloudShield.Card.abandonScanState
                (model.exoextBatch |> Maybe.map .remaining |> Maybe.withDefault [])
                model.exoextCard
    }


{-| Ask the publisher to stop the run belonging to `requestId`, by writing the cancel channel
(`exoext.v1.req.cancel`) on the publishing VM's own metadata.

One atomic `requestSetServerMetadataItems`, the same idiom as a request write and for the same
reason — but deliberately NOT through the request slot: a stop must never look like a new request.
Nothing else is touched, so a cancel cannot disturb the run's own status keys or the result slot.

-}
writeCancelRequestCmd : Project -> Model -> String -> Cmd Msg
writeCancelRequestCmd project model requestId =
    Rest.Nova.requestSetServerMetadataItems project model.serverUuid (Exoext.Transport.reqCancelMetadata requestId)
        |> Cmd.map SharedMsg


popoverMsgMapper : PopoverId -> Msg
popoverMsgMapper popoverId =
    SharedMsg <| SharedMsg.TogglePopover popoverId


view : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Element.Element Msg
view context project currentTimeAndZone model =
    let
        renderHasServers servers =
            let
                maybeServer =
                    servers |> List.Extra.find (\s -> s.osProps.uuid == model.serverUuid)
            in
            case maybeServer of
                Just server ->
                    serverDetail_ context project currentTimeAndZone model server

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.virtualComputer
                            , "found"
                            ]
    in
    VH.renderRDPP context
        project.servers
        context.localization.virtualComputer
        renderHasServers


serverDetail_ : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Server -> Element.Element Msg
serverDetail_ context project ( currentTime, timeZone ) model server =
    {- Render details of a server type and associated resources (e.g. volumes) -}
    let
        details =
            server.osProps.details

        whenCreated =
            let
                { timeDistanceStr, createdTimeText } =
                    VH.whenCreatedText { currentTime = currentTime, createdAt = details.created }

                setupTimeText =
                    case server.exoProps.serverOrigin of
                        ServerFromExo exoOriginProps ->
                            case exoOriginProps.exoSetupStatus.data of
                                RDPP.DoHave ( ExoSetupComplete, maybeSetupCompleteTime ) _ ->
                                    let
                                        setupTimeStr =
                                            case maybeSetupCompleteTime of
                                                Nothing ->
                                                    "Unknown"

                                                Just setupCompleteTime ->
                                                    Helpers.Time.relativeTimeNoAffixes details.created setupCompleteTime
                                    in
                                    Element.text ("Setup time: " ++ setupTimeStr)

                                _ ->
                                    Element.none

                        _ ->
                            Element.none

                toggleTipContents =
                    Element.column [ Element.spacing spacer.px4 ] [ createdTimeText, setupTimeText ]
            in
            VH.whenCreatedToggleTip
                context
                project
                popoverMsgMapper
                timeDistanceStr
                server.osProps
                toggleTipContents

        creatorName =
            serverCreatorName project server

        maybeFlavor =
            GetterSetters.flavorLookup project details.flavorId

        flavorContents =
            case maybeFlavor of
                Just flavor ->
                    let
                        serverResourceQtys =
                            Helpers.serverResourceQtys project flavor server

                        toggleTipContents =
                            Element.column
                                []
                                [ Element.text (String.fromInt serverResourceQtys.cores ++ " CPU cores")
                                , case serverResourceQtys.vgpus of
                                    Just vgpuQty ->
                                        let
                                            desc =
                                                "virtual GPU" |> Helpers.String.pluralizeCount vgpuQty
                                        in
                                        Element.text
                                            (String.fromInt vgpuQty ++ " " ++ desc)

                                    Nothing ->
                                        Element.none
                                , Element.text
                                    (String.fromInt serverResourceQtys.ramGb ++ " GB RAM")
                                , case serverResourceQtys.rootDiskGb of
                                    Just rootDiskGb ->
                                        Element.text
                                            (String.fromInt rootDiskGb
                                                ++ " GB root disk"
                                            )

                                    Nothing ->
                                        Element.text "unknown root disk size"
                                ]

                        toggleTip =
                            Style.Widgets.ToggleTip.toggleTip
                                context
                                popoverMsgMapper
                                (Helpers.String.hyphenate [ "flavorToggleTip", project.auth.project.uuid, server.osProps.uuid ])
                                toggleTipContents
                                ST.PositionBottomRight
                    in
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text flavor.name
                        , toggleTip
                        ]

                Nothing ->
                    Element.text ("Unknown " ++ context.localization.virtualComputerHardwareConfig)

        imageEl =
            let
                maybeImage =
                    GetterSetters.imageLookup project details.imageUuid

                maybeBootVolumeImageData =
                    GetterSetters.getBootVolume project server.osProps.uuid
                        |> Maybe.andThen .imageMetadata

                staticUuid uuid =
                    Text.text Text.Small
                        [ Element.paddingXY spacer.px4 0
                        , Text.fontFamily Text.Mono
                        , Element.alignBottom
                        ]
                        uuid

                nameOrUuid imageData =
                    case Just imageData.name |> removeEmptiness of
                        Just name ->
                            Text.body name

                        Nothing ->
                            staticUuid imageData.uuid
            in
            case ( maybeBootVolumeImageData, maybeImage ) of
                ( Just bootVolumeImageData, _ ) ->
                    nameOrUuid bootVolumeImageData

                ( _, Just image ) ->
                    nameOrUuid image

                _ ->
                    staticUuid details.imageUuid

        serverDetailTiles =
            let
                usernameView =
                    case server.exoProps.serverOrigin of
                        ServerFromExo _ ->
                            Element.text "exouser"

                        _ ->
                            Element.el
                                [ context.palette.neutral.text.subdued
                                    |> SH.toElementColor
                                    |> Font.color
                                ]
                                (Element.text "unknown")

                buttonLabel onPress =
                    Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Attach " ++ context.localization.blockDevice
                        , onPress = onPress
                        }

                attachButton =
                    if serverCanHaveVolumeAttached server then
                        Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.VolumeAttach (Just server.osProps.uuid) Nothing
                                    )
                            , label = buttonLabel <| Just NoOp
                            }

                    else
                        Element.none

                guiTag =
                    IHelpers.getLaunchedWithGaucamoleProps server
                        |> Maybe.map .vncSupported
                        |> Maybe.withDefault False
                        |> (\vncSupported ->
                                if vncSupported then
                                    tag context.palette context.localization.graphicalDesktopEnvironment

                                else
                                    Element.none
                           )
            in
            [ VH.tile
                context
                [ Icon.featherIcon [] Icons.monitor
                , Element.text "Interactions"
                , guiTag
                ]
                [ interactions
                    context
                    project
                    server
                    currentTime
                    (GetterSetters.getUserAppProxyFromContext project context)
                ]
            , VH.tile
                context
                [ Icon.featherIcon [] Icons.key
                , Element.text (context.localization.credential |> Helpers.String.pluralize |> Helpers.String.toTitleCase)
                ]
                [ renderIpAddresses
                    context
                    project
                    server
                , VH.compactKVSubRow "Username" usernameView
                , VH.compactKVSubRow "Passphrase"
                    (serverPassphrase context model server)
                , VH.compactKVSubRow
                    (String.join " "
                        [ context.localization.pkiPublicKeyForSsh
                            |> Helpers.String.toTitleCase
                        , "Name"
                        ]
                    )
                    (Element.text (Maybe.withDefault "(none)" details.keypairName))
                ]
            , VH.tile
                context
                [ Icon.featherIcon [] Icons.hardDrive
                , context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                    |> Element.text
                ]
                [ renderServerVolumes context project server
                , Element.el [ Element.centerX ] attachButton
                ]
            , if context.experimentalFeaturesEnabled then
                VH.tile
                    context
                    [ Icon.featherIcon [] Icons.shield
                    , context.localization.securityGroup
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                        |> Element.text
                    , Element.link [ Element.alignRight ]
                        { url =
                            Route.toUrl context.urlPathPrefix <|
                                Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                    Route.ServerSecurityGroups server.osProps.uuid
                        , label =
                            Widget.button
                                (SH.materialStyle context.palette).button
                                { text = "Edit"
                                , icon = Icon.sizedFeatherIcon 16 Icons.edit3
                                , onPress =
                                    Just NoOp
                                }
                        }
                    ]
                    [ renderSecurityGroups context project server ]

              else
                Element.none
            , VH.tile
                context
                [ Icon.history (SH.toElementColor context.palette.neutral.text.default) 20
                , Element.text "Action History"
                ]
                [ renderServerEventHistory
                    context
                    project
                    server
                    currentTime
                ]
            ]

        serverFaultView =
            case details.fault of
                Just serverFault ->
                    Alert.alert [ Element.width Element.fill ]
                        context.palette
                        { state = Alert.Danger
                        , showIcon = True
                        , showContainer = True
                        , content =
                            Element.paragraph []
                                [ Text.strong serverFault.message ]
                        }

                Nothing ->
                    Element.none

        statusTransitionInProgress =
            let
                { targetOpenstackStatus, request } =
                    GetterSetters.getServerExoActions project server.osProps.uuid
            in
            case ( targetOpenstackStatus, request ) of
                ( Just _, { data, refreshStatus } ) ->
                    case ( data, refreshStatus ) of
                        ( _, Loading ) ->
                            -- The server status is a transition so we don't indicate anything further.
                            Element.none

                        ( _, NotLoading (Just ( error, _ )) ) ->
                            Alert.alert [ Element.width Element.fill ]
                                context.palette
                                { state = Alert.Danger
                                , showIcon = False
                                , showContainer = True
                                , content =
                                    Element.row [ Element.spacing spacer.px8, Element.width Element.fill ]
                                        [ Text.p [ Element.width Element.fill ] [ Text.body <| Error.formattedError error ]
                                        , Button.button Button.Danger
                                            context.palette
                                            { text = "Clear"
                                            , onPress = Just GotResetServerAction
                                            }
                                        ]
                                }

                        ( DoHave _ receivedAt, _ ) ->
                            -- If we've been waiting > 5 minutes, show a reset button.
                            if Time.posixToMillis currentTime < (Time.posixToMillis receivedAt + 5 * 60 * 1000) then
                                Element.none

                            else
                                Alert.alert [ Element.width Element.fill ]
                                    context.palette
                                    { state = Alert.Warning
                                    , showIcon = False
                                    , showContainer = True
                                    , content =
                                        let
                                            action =
                                                VH.getServerUiStatus project server |> VH.getServerUiStatusStr

                                            toggleTipId =
                                                Helpers.String.hyphenate
                                                    [ "resetToggleTip"
                                                    , project.auth.project.uuid
                                                    , server.osProps.uuid
                                                    ]

                                            relativeTime =
                                                DateFormat.Relative.relativeTime currentTime receivedAt

                                            resetToggleTip =
                                                Style.Widgets.ToggleTip.toggleTip
                                                    context
                                                    popoverMsgMapper
                                                    toggleTipId
                                                    (Text.body <| "This operation was submitted " ++ relativeTime ++ " & may take some time to complete.\nIf it seems stuck, you can reset & try the action again.")
                                                    ST.PositionLeft
                                        in
                                        Element.row [ Element.spacing spacer.px8, Element.width Element.fill ]
                                            -- TODO: Some server actions are slower than others, we can probably estimate this better than using a fixed time.
                                            [ Text.p [ Element.width Element.fill ] [ Text.body <| action ++ " is taking longer than expected." ]
                                            , resetToggleTip
                                            , Button.button Button.Warning
                                                context.palette
                                                { text = "Reset"
                                                , onPress = Just GotResetServerAction
                                                }
                                            ]
                                    }

                        _ ->
                            Element.none

                ( Nothing, _ ) ->
                    Element.none

        connectivityWarningView =
            let
                maybeServerSecurityGroupUuids =
                    GetterSetters.getServerSecurityGroups project server.osProps.uuid
                        |> RDPP.toMaybe
                        |> Maybe.map (List.map .uuid)

                maybeProjectSecurityGroups =
                    project.securityGroups
                        |> RDPP.toMaybe

                maybeSecurityGroups =
                    case ( maybeServerSecurityGroupUuids, maybeProjectSecurityGroups ) of
                        ( Just sgUuids, Just projectSecurityGroups ) ->
                            Just
                                (projectSecurityGroups
                                    |> List.filter (\sg -> List.member sg.uuid sgUuids)
                                )

                        _ ->
                            Nothing

                maybeSecurityGroupRules =
                    maybeSecurityGroups
                        -- We make an effort to preserve maybe-ness instead of defaulting to []
                        -- because an empty rule list will imply no incoming/outgoing connections are allowed.
                        |> Maybe.map (List.concatMap .rules)

                ( guacamoleRequired, vncRequired ) =
                    case server.exoProps.serverOrigin of
                        ServerFromExo serverFromExo ->
                            case serverFromExo.guacamoleStatus of
                                LaunchedWithGuacamole props ->
                                    ( True, props.vncSupported )

                                _ ->
                                    ( False, False )

                        _ ->
                            ( False, False )

                { isConnectivityBroken, connectivityChecks } =
                    VH.isConnectivityBroken context
                        (maybeSecurityGroupRules |> Maybe.withDefault [])
                        { guacamoleRequired = guacamoleRequired, vncRequired = vncRequired }

                -- Don't show connectivity warnings when security group data may be unreliable or changing (esp. while building).
                serverUiStatus =
                    VH.getServerUiStatus project server

                isStatusStableForSecurityGroupCheck =
                    not <|
                        List.member serverUiStatus
                            [ ServerUiStatusUnknown
                            , ServerUiStatusBuilding
                            , ServerUiStatusDeleting
                            , ServerUiStatusSoftDeleted
                            , ServerUiStatusDeleted
                            , ServerUiStatusResizing
                            , ServerUiStatusVerifyResize
                            , ServerUiStatusRevertingResize
                            , ServerUiStatusMigrating
                            , ServerUiStatusError
                            ]
            in
            if
                context.experimentalFeaturesEnabled
                    && isConnectivityBroken
                    && isStatusStableForSecurityGroupCheck
                    && maybeSecurityGroupRules
                    /= Nothing
            then
                Alert.alert [ Element.width Element.fill ]
                    context.palette
                    { state = Alert.Warning
                    , showIcon = True
                    , showContainer = True
                    , content =
                        Element.column [ Element.spacing spacer.px8, Element.width Element.fill ] <|
                            Text.p []
                                [ Text.body <| "This " ++ context.localization.virtualComputer ++ "'s "
                                , Link.link context.palette
                                    (Route.toUrl context.urlPathPrefix <|
                                        Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                            Route.ServerSecurityGroups server.osProps.uuid
                                    )
                                    (context.localization.securityGroup
                                        ++ " configuration"
                                    )
                                , Text.body " may result in connectivity problems:"
                                ]
                                :: (connectivityChecks
                                        |> List.filter (\( _, permitted ) -> not permitted)
                                        |> List.map
                                            (\( c, _ ) ->
                                                Text.body <|
                                                    " • "
                                                        ++ Maybe.withDefault "Other connections" c.description
                                                        ++ " may be blocked."
                                            )
                                   )
                    }

            else
                Element.none
    in
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.row (Text.headingStyleAttrs context.palette)
            [ Icon.featherIcon [] Icons.server
            , Text.text Text.ExtraLarge
                []
                (context.localization.virtualComputer
                    |> Helpers.String.toTitleCase
                )
            , serverNameView context project currentTime model server
            , Element.row [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                [ serverStatus context project server
                , serverActionsDropdown context project model server
                ]
            ]
        , statusTransitionInProgress
        , VH.tile
            context
            [ Icon.featherIcon [] Icons.cpu
            , Element.text "Info"
            , Element.el
                [ Text.fontSize Text.Small
                , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                , Element.paddingXY spacer.px12 0
                , Text.fontFamily Text.Mono
                ]
                (copyableUuid context.palette server.osProps.uuid)
            ]
            [ VH.createdAgoByFromSize
                context
                ( "created", whenCreated )
                (Just ( "user", creatorName ))
                (Just ( context.localization.staticRepresentationOfBlockDeviceContents, imageEl ))
                (Just ( context.localization.virtualComputerHardwareConfig, flavorContents ))
                server.osProps
                project
            , passphraseVulnWarning context server
            ]
        , serverFaultView
        , connectivityWarningView
        , if List.member details.openstackStatus [ OSTypes.ServerActive, OSTypes.ServerVerifyResize ] then
            VH.tile
                context
                [ Icon.featherIcon [] Icons.activity
                , Element.text "Resource Usage"
                ]
                [ resourceUsageCharts context
                    ( currentTime, timeZone )
                    server
                    (maybeFlavor |> Maybe.map (\flavor -> Helpers.serverResourceQtys project flavor server))
                ]

          else
            Element.none
        , exoextCardView context project ( currentTime, timeZone ) server model
        , Element.wrappedRow [ Element.spacing spacer.px24 ] serverDetailTiles
        ]


exoextCardView : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Server -> Model -> Element.Element Msg
exoextCardView context project ( currentTime, timeZone ) server model =
    -- Gated behind the experimental-features flag AND the §5.1 self-instance-placement
    -- discovery gate: the card appears only on an instance that actually published an
    -- extension, i.e. the `exoext.v1.kind` sentinel is present in *this* instance's own Nova
    -- metadata. No sentinel => no card. (Removes the prior dev fallback that rendered the
    -- embedded card on every instance.)
    let
        sentinel =
            Exoext.Discovery.readSentinel server.osProps.details.metadata

        -- The health keys are a discovery signal in their own right: an extension can fail (or
        -- still be booting) before it ever writes a sentinel, and those are exactly the states the
        -- page used to render as nothing at all. Either signal opens the card.
        health =
            Exoext.Health.read server.osProps.details.metadata
    in
    if context.experimentalFeaturesEnabled && (sentinel /= Nothing || health /= Nothing) then
        let
            -- Approval is matched by the publishing instance's UUID only (a persisted
            -- `exoext.approval.v1` record). No record => the card shows its opt-in affordance.
            approved =
                ExtensionApproval.isApproved server.osProps.uuid context.extensionApprovals

            config =
                exoextViewConfig approved project model currentTime server

            -- The transport label rides in the header (top-right), not the card body.
            headerChip =
                case config.transportLabel of
                    Just label ->
                        [ Element.el [ Element.alignRight ] (CloudShield.Card.transportChip context.palette label) ]

                    Nothing ->
                        []
        in
        VH.tile
            context
            ([ Icon.featherIcon [] Icons.grid
             , Element.text (exoextCardTitle sentinel)
             , extensionExperimentalTag context
             ]
                ++ headerChip
            )
            [ CloudShield.Card.view
                context.palette
                context.localization
                timeZone
                config
                (exoextInstances project model)
                model.exoextCard
                |> Element.map CloudShieldMsg
            ]

    else
        Element.none


{-| The card header's title, resolved from the §3.1 discovery sentinel's `kind` rather than
hardcoded. `kind` is the one thing the sentinel already says about WHAT was published, and until now
nothing read it — the header asserted one extension's name on behalf of every publisher.

The resolution is a lookup, not a transformation of the publisher's string, and that is the point in
two directions. A kind is a wire token (`cloudshield`), not a display name (`CloudShield`), and no
general rule turns one into the other without guessing at capitalization. More importantly `kind`
is publisher-controlled data read off a VM's own metadata: painting it straight into Exosphere's
own chrome would let any instance title a panel of the researcher's UI with whatever it liked.

So an adapter Exosphere ships supplies its own name, and everything else is the generic word. That
is not a placeholder — it is the honest label for a card whose contents Exosphere cannot vouch for
and whose publisher it does not recognize.

-}
exoextCardTitle : Maybe Exoext.Discovery.Sentinel -> String
exoextCardTitle sentinel =
    if (sentinel |> Maybe.map .kind) == Just CloudShield.Card.sentinelKind then
        CloudShield.Card.headerTitle

    else
        "Extension"


{-| The "Experimental" tag for the extension card header (plan §C3). Same idiom as the
Settings "Experimental features" label: an emphasized word with a toggle-tip explaining that
extensions are experimental and that the card's UI is published by a VM, not by Exosphere. It
sits in the card header so it shows in both the opt-in and the approved state.
-}
extensionExperimentalTag : View.Types.Context -> Element.Element Msg
extensionExperimentalTag context =
    Text.text Text.Emphasized
        [ Element.onRight
            (Element.el [ Element.paddingXY spacer.px8 0 ] <|
                Style.Widgets.ToggleTip.toggleTip
                    context
                    (\tipId -> SharedMsg <| SharedMsg.TogglePopover tipId)
                    "exoextExtensionExperimentalToggleTip"
                    (Element.paragraph
                        [ Element.width (Element.fill |> Element.minimum 300)
                        , Element.spacing spacer.px8
                        , Font.regular
                        ]
                        [ Element.text ("Extensions are an experimental feature. This interface is published by " ++ Helpers.String.indefiniteArticle context.localization.virtualComputer ++ " " ++ context.localization.virtualComputer ++ " in your " ++ context.localization.unitOfTenancy ++ ", not by Exosphere.") ]
                    )
                    ST.PositionRight
            )
        ]
        "Experimental"


serverNameEditView : View.Types.Context -> Project -> Time.Posix -> Model -> Server -> Element.Element Msg
serverNameEditView context project currentTime model server =
    let
        serverNamePendingConfirmation =
            model.serverNamePendingConfirmation
                |> Maybe.withDefault ""

        invalidNameReasons =
            serverNameValidator
                (Just context.localization.virtualComputer)
                serverNamePendingConfirmation

        renderInvalidNameReasons =
            case invalidNameReasons of
                Just reasons ->
                    List.map Element.text reasons
                        |> List.map List.singleton
                        |> List.map (Element.paragraph [])
                        |> Element.column
                            (popoverStyleDefaults context.palette
                                ++ [ Font.color (SH.toElementColor context.palette.danger.textOnNeutralBG)
                                   , Text.fontSize Text.Small
                                   , Element.alignRight
                                   , Element.moveDown 6
                                   , Element.spacing spacer.px12
                                   , Element.padding spacer.px16
                                   ]
                            )

                Nothing ->
                    Element.none

        renderServerNameExists =
            if
                Validation.serverNameExists project serverNamePendingConfirmation
                    -- the server's own name currently exists, of course:
                    && server.osProps.name
                    /= Maybe.withDefault "" model.serverNamePendingConfirmation
            then
                let
                    message =
                        Element.row []
                            [ Element.paragraph
                                [ Element.width (Element.fill |> Element.minimum 300)
                                , Element.spacing spacer.px8
                                , Font.regular
                                , Font.color <| SH.toElementColor <| context.palette.warning.textOnNeutralBG
                                ]
                                [ Element.text <|
                                    Validation.resourceNameExistsMessage context.localization.virtualComputer context.localization.unitOfTenancy
                                ]
                            ]

                    suggestedNames =
                        Validation.resourceNameSuggestions currentTime project serverNamePendingConfirmation
                            |> List.filter (\n -> not (Validation.serverNameExists project n))

                    content =
                        Element.column []
                            (message
                                :: List.map
                                    (\name ->
                                        Element.row [ Element.paddingEach { edges | top = spacer.px12 } ]
                                            [ Button.default
                                                context.palette
                                                { text = name
                                                , onPress = Just <| GotServerNamePendingConfirmation (Just name)
                                                }
                                            ]
                                    )
                                    suggestedNames
                            )
                in
                Style.Widgets.ToggleTip.warningToggleTip
                    context
                    (\serverRenameAlreadyExistsToggleTipId -> SharedMsg <| SharedMsg.TogglePopover serverRenameAlreadyExistsToggleTipId)
                    "serverRenameAlreadyExistsToggleTip"
                    content
                    ST.PositionRightTop

            else
                Element.none

        rowStyle =
            [ Element.spacing spacer.px8
            , Element.width Element.fill
            ]

        cancelOnPress =
            Just <| GotServerNamePendingConfirmation Nothing

        saveOnPress =
            case ( invalidNameReasons, model.serverNamePendingConfirmation ) of
                ( Nothing, Just validName ) ->
                    if validName == server.osProps.name then
                        cancelOnPress

                    else
                        Just <| GotSetServerName validName

                ( _, _ ) ->
                    Nothing
    in
    Element.row rowStyle
        [ Element.el
            [ Element.below renderInvalidNameReasons
            ]
            (Input.text
                (VH.inputItemAttributes context.palette
                    ++ [ Element.width <| Element.minimum 300 Element.fill ]
                )
                { text = model.serverNamePendingConfirmation |> Maybe.withDefault ""
                , placeholder =
                    Just
                        (Input.placeholder
                            []
                            (Element.text <|
                                String.join " "
                                    [ "My"
                                    , context.localization.virtualComputer
                                        |> Helpers.String.toTitleCase
                                    ]
                            )
                        )
                , onChange = \name -> GotServerNamePendingConfirmation <| Just name
                , label = Input.labelHidden "Name"
                }
            )
        , Widget.iconButton
            (SH.materialStyle context.palette).button
            { text = "Save"
            , icon = Icon.sizedFeatherIcon 16 Icons.save
            , onPress =
                saveOnPress
            }
        , Widget.iconButton
            (SH.materialStyle context.palette).button
            { text = "Cancel"
            , icon = Icon.sizedFeatherIcon 16 Icons.xCircle
            , onPress =
                cancelOnPress
            }
        , renderServerNameExists
        ]


serverNameView : View.Types.Context -> Project -> Time.Posix -> Model -> Server -> Element.Element Msg
serverNameView context project currentTime model server =
    case model.serverNamePendingConfirmation of
        Just _ ->
            serverNameEditView context project currentTime model server

        Nothing ->
            let
                name_ =
                    VH.resourceName (Just server.osProps.name) server.osProps.uuid
            in
            Element.row
                [ Element.spacing spacer.px8 ]
                [ Text.text Text.ExtraLarge [] name_
                , Widget.iconButton
                    (SH.materialStyle context.palette).button
                    { text = "Edit"
                    , icon = Icon.sizedFeatherIcon 16 Icons.edit3
                    , onPress =
                        Just <| GotServerNamePendingConfirmation (Just name_)
                    }
                ]


passphraseVulnWarning : View.Types.Context -> Server -> Element.Element Msg
passphraseVulnWarning context server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Element.none

        ServerFromExo serverFromExoProps ->
            if serverFromExoProps.exoServerVersion < 1 then
                Element.el [ Element.paddingXY 0 spacer.px16 ] <|
                    Alert.alert []
                        context.palette
                        { state = Alert.Danger
                        , showIcon = False
                        , showContainer = True
                        , content =
                            Element.paragraph []
                                [ Element.text <|
                                    String.join " "
                                        [ "This"
                                        , context.localization.virtualComputer
                                        , "was created with an older version of Exosphere which left the opportunity for unprivileged processes running on the"
                                        , context.localization.virtualComputer
                                        , "to query the "
                                        , context.localization.virtualComputer
                                        , " metadata service and determine the passphrase for exouser (who is a sudoer). This represents a "
                                        ]
                                , Link.externalLink
                                    context.palette
                                    "https://en.wikipedia.org/wiki/Privilege_escalation"
                                    "privilege escalation vulnerability"
                                , Element.text <|
                                    String.join " "
                                        [ ". If you have used this"
                                        , context.localization.virtualComputer
                                        , "for anything important or sensitive, consider rotating the passphrase for exouser, or building a new"
                                        , context.localization.virtualComputer
                                        , "and moving to that one instead of this one. For more information, see "
                                        ]
                                , Link.externalLink
                                    context.palette
                                    "https://gitlab.com/exosphere/exosphere/issues/284"
                                    "issue #284"
                                , Element.text {- @nonlocalized -} " on the Exosphere GitLab project."
                                ]
                        }

            else
                Element.none


serverStatus : View.Types.Context -> Project -> Server -> Element.Element Msg
serverStatus context project server =
    let
        details =
            server.osProps.details

        statusBadge =
            VH.serverStatusBadge context.palette StatusBadge.Normal project server

        lockStatus : OSTypes.ServerLockStatus -> Element.Element Msg
        lockStatus lockStatus_ =
            case lockStatus_ of
                OSTypes.ServerLocked ->
                    Icon.lock (SH.toElementColor context.palette.neutral.icon) 28

                OSTypes.ServerUnlocked ->
                    Icon.lockOpen (SH.toElementColor context.palette.neutral.icon) 28

        verboseStatusToggleTip =
            let
                friendlyOpenstackStatus : OSTypes.ServerStatus -> String
                friendlyOpenstackStatus osStatus =
                    OSTypes.serverStatusToString osStatus

                friendlyPowerState =
                    OSTypes.serverPowerStateToString details.powerState
                        |> String.dropLeft 5

                contents =
                    -- TODO nicer layout here?
                    Element.column [ Element.spacing spacer.px8, Element.padding spacer.px4 ]
                        [ Element.text ("OpenStack Status: " ++ friendlyOpenstackStatus details.openstackStatus)
                        , case (GetterSetters.getServerExoActions project server.osProps.uuid).targetOpenstackStatus of
                            Just expectedStatusList ->
                                let
                                    listStr =
                                        expectedStatusList
                                            |> List.map friendlyOpenstackStatus
                                            |> String.join ", "
                                in
                                Element.text ("Transitioning to: " ++ listStr)

                            Nothing ->
                                Element.none
                        , Element.text ("Power State: " ++ friendlyPowerState)
                        , Element.text
                            ("Lock Status: "
                                ++ (case details.lockStatus of
                                        OSTypes.ServerLocked ->
                                            "Locked"

                                        OSTypes.ServerUnlocked ->
                                            "Unlocked"
                                   )
                            )
                        , case VH.getExoSetupStatusStr server of
                            Just setupStatusStr ->
                                Element.text ("Exosphere Setup Status: " ++ setupStatusStr)

                            Nothing ->
                                Element.none
                        ]

                toggleTipId =
                    Helpers.String.hyphenate
                        [ "verboseStatusTip"
                        , project.auth.project.uuid
                        , server.osProps.uuid
                        , friendlyOpenstackStatus details.openstackStatus
                        ]
            in
            Style.Widgets.ToggleTip.toggleTip
                context
                popoverMsgMapper
                toggleTipId
                contents
                ST.PositionLeft
    in
    Element.row [ Element.spacing spacer.px16 ]
        [ verboseStatusToggleTip
        , statusBadge
        , lockStatus details.lockStatus
        ]


interactions : View.Types.Context -> Project -> Server -> Time.Posix -> Maybe UserAppProxyHostname -> Element.Element Msg
interactions context project server currentTime tlsReverseProxyHostname =
    let
        renderInteraction interaction =
            let
                interactionStatus =
                    IHelpers.interactionStatus
                        project
                        server
                        interaction
                        context
                        currentTime
                        tlsReverseProxyHostname
            in
            case interactionStatus of
                ITypes.Hidden ->
                    Element.none

                _ ->
                    let
                        interactionDetails =
                            IHelpers.interactionDetails interaction context

                        ( statusWord, statusColor ) =
                            IHelpers.interactionStatusWordColor context.palette interactionStatus

                        status =
                            Element.row []
                                [ Text.strong "Status: "
                                , Element.text statusWord
                                ]

                        statusReason =
                            let
                                renderReason reason =
                                    Element.text <| "(" ++ reason ++ ")"
                            in
                            case interactionStatus of
                                ITypes.Unavailable reason ->
                                    renderReason reason

                                ITypes.Error reason ->
                                    renderReason reason

                                ITypes.Warn _ reason ->
                                    renderReason reason

                                _ ->
                                    Element.none

                        description =
                            Element.paragraph []
                                [ Text.strong "Description: "
                                , Element.text interactionDetails.description
                                ]

                        contents =
                            Element.column
                                [ Element.width (Element.shrink |> Element.minimum 200)
                                , Element.spacing spacer.px12
                                , Element.padding spacer.px4
                                ]
                                [ status
                                , statusReason
                                , description
                                ]

                        toggleTipId =
                            Helpers.String.hyphenate
                                [ "interactionToggleTip"
                                , project.auth.project.uuid
                                , server.osProps.uuid
                                , interactionDetails.name
                                ]
                    in
                    Element.row
                        [ Element.spacing spacer.px12 ]
                        [ Icon.roundRect statusColor 14
                        , case interactionDetails.type_ of
                            ITypes.UrlInteraction ->
                                Widget.button
                                    (SH.materialStyle context.palette).button
                                    { text = interactionDetails.name
                                    , icon =
                                        Element.el
                                            [ Element.paddingEach
                                                { top = 0
                                                , right = spacer.px4
                                                , left = 0
                                                , bottom = 0
                                                }
                                            ]
                                            (interactionDetails.icon 18)
                                    , onPress =
                                        case interactionStatus of
                                            ITypes.Ready url ->
                                                Just <| SharedMsg <| SharedMsg.OpenNewWindow url

                                            ITypes.Warn url _ ->
                                                Just <| SharedMsg <| SharedMsg.OpenNewWindow url

                                            _ ->
                                                Nothing
                                    }

                            ITypes.NavigateInteraction ->
                                let
                                    buttonLabel =
                                        Widget.button
                                            (SH.materialStyle context.palette).button
                                            { text = interactionDetails.name
                                            , icon =
                                                Element.el
                                                    [ Element.paddingEach
                                                        { top = 0
                                                        , right = spacer.px4
                                                        , left = 0
                                                        , bottom = 0
                                                        }
                                                    ]
                                                    (interactionDetails.icon 18)
                                            , onPress = Just NoOp
                                            }
                                in
                                case interactionStatus of
                                    ITypes.Ready url ->
                                        Element.link [] { url = url, label = buttonLabel }

                                    ITypes.Warn url _ ->
                                        Element.link [] { url = url, label = buttonLabel }

                                    _ ->
                                        Widget.button
                                            (SH.materialStyle context.palette).button
                                            { text = interactionDetails.name
                                            , icon =
                                                Element.el
                                                    [ Element.paddingEach
                                                        { top = 0
                                                        , right = spacer.px4
                                                        , left = 0
                                                        , bottom = 0
                                                        }
                                                    ]
                                                    (interactionDetails.icon 18)
                                            , onPress = Nothing
                                            }

                            ITypes.TextInteraction ->
                                let
                                    ( iconColor, fontColor ) =
                                        case interactionStatus of
                                            ITypes.Ready _ ->
                                                ( SH.toElementColor context.palette.primary
                                                , SH.toElementColor context.palette.neutral.text.default
                                                )

                                            ITypes.Warn _ _ ->
                                                ( SH.toElementColor context.palette.neutral.icon
                                                , SH.toElementColor context.palette.neutral.text.default
                                                )

                                            _ ->
                                                ( SH.toElementColor context.palette.neutral.icon
                                                , SH.toElementColor context.palette.neutral.text.subdued
                                                )
                                in
                                Element.row
                                    [ Font.color fontColor
                                    , Element.spacing spacer.px8
                                    ]
                                    [ Element.el
                                        [ Font.color iconColor ]
                                        (interactionDetails.icon 22)
                                    , Element.text interactionDetails.name
                                    , case interactionStatus of
                                        ITypes.Ready text ->
                                            Element.row
                                                []
                                                [ Element.text ": "
                                                , copyableText context.palette [] text
                                                ]

                                        ITypes.Warn text _ ->
                                            Element.row
                                                []
                                                [ Element.text ": "
                                                , copyableText context.palette [] text
                                                ]

                                        _ ->
                                            Element.none
                                    ]
                        , Style.Widgets.ToggleTip.toggleTip
                            context
                            popoverMsgMapper
                            toggleTipId
                            contents
                            ST.PositionRightBottom
                        ]
    in
    [ ITypes.GuacTerminal
    , ITypes.GuacDesktop
    , ITypes.NativeSSH
    , ITypes.Console
    , ITypes.ConsoleLog
    , ITypes.CustomWorkflow
    ]
        |> List.map renderInteraction
        |> Element.column [ Element.spacing spacer.px16 ]


serverPassphrase : View.Types.Context -> Model -> Server -> Element.Element Msg
serverPassphrase context model server =
    let
        passphraseShower passphrase =
            Element.column
                [ Element.spacing spacer.px12 ]
                [ case model.passphraseVisibility of
                    PassphraseShown ->
                        copyableText context.palette [] passphrase

                    PassphraseHidden ->
                        Element.none
                , let
                    changeMsg newValue =
                        GotPassphraseVisibility newValue

                    ( buttonText, onPressMsg ) =
                        case model.passphraseVisibility of
                            PassphraseShown ->
                                ( "Hide passphrase"
                                , changeMsg PassphraseHidden
                                )

                            PassphraseHidden ->
                                ( "Show"
                                , changeMsg PassphraseShown
                                )
                  in
                  Widget.textButton
                    (SH.materialStyle context.palette).button
                    { text = buttonText
                    , onPress = Just onPressMsg
                    }
                ]
    in
    case GetterSetters.getServerExouserPassphrase server.osProps.details of
        Just passphrase ->
            passphraseShower passphrase

        Nothing ->
            -- TODO factor out this logic used to determine whether to display the charts as well
            case server.exoProps.serverOrigin of
                ServerFromExo originProps ->
                    case originProps.exoSetupStatus.data of
                        RDPP.DoHave ( ExoSetupWaiting, _ ) _ ->
                            Element.text "Not available yet, check in a few minutes."

                        RDPP.DoHave ( ExoSetupRunning, _ ) _ ->
                            Element.text "Not available yet, check in a few minutes."

                        _ ->
                            Element.text "Not available"

                _ ->
                    Element.el
                        [ context.palette.neutral.text.subdued
                            |> SH.toElementColor
                            |> Font.color
                        ]
                        (Element.text <|
                            String.concat
                                [ "Not available because "
                                , context.localization.virtualComputer
                                , " was not created by Exosphere"
                                ]
                        )


serverActionsDropdown : View.Types.Context -> Project -> Model -> Server -> Element.Element Msg
serverActionsDropdown context project model server =
    let
        dropdownContent closeDropdown =
            let
                disallowedActions =
                    GetterSetters.getServerFlavorGroup project context server
                        |> Maybe.map .disallowedActions
                        |> Maybe.withDefault []
            in
            Element.column [ Element.spacing spacer.px8 ] <|
                List.map
                    (renderServerAction context project model server closeDropdown)
                    (VH.getAllowedServerActionOptions context
                        server.osProps.details.openstackStatus
                        server.osProps.details.lockStatus
                        disallowedActions
                    )

        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { text = "Actions"
                , icon =
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text "Actions"
                        , Icon.sizedFeatherIcon 18 <|
                            if dropdownIsShown then
                                Icons.chevronUp

                            else
                                Icons.chevronDown
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
    case (GetterSetters.getServerExoActions project server.osProps.uuid).targetOpenstackStatus of
        Nothing ->
            let
                dropdownId =
                    [ "serverActionsDropdown", project.auth.project.uuid, server.osProps.uuid ]
                        |> List.intersperse "-"
                        |> String.concat
            in
            popover context
                popoverMsgMapper
                { id = dropdownId
                , content = dropdownContent
                , contentStyleAttrs = [ Element.padding spacer.px24 ]
                , position = ST.PositionBottomRight
                , distanceToTarget = Nothing
                , target = dropdownTarget
                , targetStyleAttrs = []
                }

        Just _ ->
            Element.none


renderServerEventHistory :
    View.Types.Context
    -> Project
    -> Server
    -> Time.Posix
    -> Element.Element Msg
renderServerEventHistory context project server currentTime =
    VH.renderRDPP context
        (GetterSetters.getServerEvents project server.osProps.uuid)
        "Action History"
        (serverEventHistoryTable context project server currentTime)


serverEventHistoryTable :
    View.Types.Context
    -> Project
    -> Server
    -> Time.Posix
    -> List OSTypes.ServerEvent
    -> Element.Element Msg
serverEventHistoryTable context project server currentTime serverEvents =
    let
        serverSetupStatus : Maybe ( String, Maybe Time.Posix )
        serverSetupStatus =
            case server.exoProps.serverOrigin of
                ServerNotFromExo ->
                    Nothing

                ServerFromExo exoOriginProps ->
                    case exoOriginProps.exoSetupStatus.data of
                        RDPP.DoHave ( exoSetupStatus, timestamp ) _ ->
                            Just
                                ( Types.Server.exoSetupStatusToString exoSetupStatus
                                , timestamp
                                )

                        RDPP.DontHave ->
                            Nothing

        columns : List (Element.Column { action : String, startTime : Time.Posix } Msg)
        columns =
            [ { header = Text.strong "Action"
              , width = Element.px 180
              , view =
                    \event ->
                        let
                            actionStr =
                                event.action
                                    |> String.replace "_" " "
                        in
                        Element.paragraph [] [ Element.text actionStr ]
              }
            , { header = Text.strong "Time"
              , width = Element.px 180
              , view =
                    \event ->
                        let
                            relativeTime =
                                DateFormat.Relative.relativeTime currentTime event.startTime

                            absoluteTime =
                                let
                                    toggleTipId =
                                        Helpers.String.hyphenate
                                            [ "serverEventTimeTip"
                                            , project.auth.project.uuid
                                            , server.osProps.uuid
                                            , event.startTime |> Time.posixToMillis |> String.fromInt
                                            ]
                                in
                                Style.Widgets.ToggleTip.toggleTip
                                    context
                                    popoverMsgMapper
                                    toggleTipId
                                    (Element.text (Helpers.Time.humanReadableDateAndTime event.startTime))
                                    ST.PositionBottomRight
                        in
                        Element.row []
                            [ Element.text relativeTime
                            , absoluteTime
                            ]
              }
            ]

        serverEventsWithActionAndStartTime =
            serverEvents
                |> List.map (\{ action, startTime } -> { action = action, startTime = startTime })

        serverSetupStatusInfo =
            case serverSetupStatus of
                Just ( status, Just timestamp ) ->
                    [ { action = "Setup " ++ status
                      , startTime = timestamp
                      }
                    ]

                Just ( _, Nothing ) ->
                    []

                Nothing ->
                    []
    in
    Element.table
        [ Element.spacingXY 0 spacer.px8
        , Element.width Element.fill
        ]
        { data =
            (serverEventsWithActionAndStartTime ++ serverSetupStatusInfo)
                |> List.sortBy (\{ startTime } -> startTime |> Time.posixToMillis)
                |> List.reverse
        , columns = columns
        }


securityGroupsTable :
    View.Types.Context
    -> ProjectIdentifier
    -> List OSTypes.SecurityGroup
    -> Element.Element Msg
securityGroupsTable context projectId securityGroups =
    case List.length securityGroups of
        0 ->
            Element.text "(none)"

        _ ->
            let
                columns : List (Element.Column { name : String, description : Maybe String, uuid : String } Msg)
                columns =
                    [ { header = Text.strong "Name"
                      , width = Element.shrink
                      , view =
                            \securityGroup ->
                                Element.link []
                                    { url =
                                        Route.toUrl context.urlPathPrefix
                                            (Route.ProjectRoute projectId <|
                                                Route.SecurityGroupDetail securityGroup.uuid
                                            )
                                    , label =
                                        Element.el
                                            [ Font.color (SH.toElementColor context.palette.primary), Element.width (Element.px 180) ]
                                            (VH.ellipsizedText <|
                                                VH.extendedResourceName
                                                    (Just securityGroup.name)
                                                    securityGroup.uuid
                                                    context.localization.securityGroup
                                            )
                                    }
                      }
                    , { header = Text.strong "Description"
                      , width = Element.fill
                      , view =
                            \securityGroup ->
                                let
                                    description =
                                        Maybe.withDefault "-" securityGroup.description
                                in
                                Element.el [ Element.clipY ]
                                    (Text.text Text.Body [ Element.width (Element.px 0) ] <|
                                        if String.isEmpty description then
                                            "-"

                                        else
                                            description
                                    )
                      }
                    ]
            in
            Element.table
                [ Element.spacing spacer.px16
                ]
                { data = List.map (\s -> { name = s.name, description = s.description, uuid = s.uuid }) securityGroups
                , columns = columns
                }


renderSecurityGroups : View.Types.Context -> Project -> Server -> Element.Element Msg
renderSecurityGroups context project server =
    let
        renderTable serverSecurityGroups =
            securityGroupsTable
                context
                (GetterSetters.projectIdentifier project)
                (GetterSetters.securityGroupsFromServerSecurityGroups project serverSecurityGroups)

        serverSecurityGroupsRdpp =
            GetterSetters.getServerSecurityGroups project server.osProps.uuid
    in
    VH.renderRDPPWithDependencies context
        serverSecurityGroupsRdpp
        (context.localization.securityGroup |> Helpers.String.pluralize)
        [ project.securityGroups ]
        renderTable


renderServerAction :
    View.Types.Context
    -> Project
    -> Model
    -> Server
    -> Element.Attribute Msg
    -> ServerActionOption
    -> Element.Element Msg
renderServerAction context project model server closeActionsDropdown serverAction =
    let
        displayConfirmation =
            case model.serverActionNamePendingConfirmation of
                Nothing ->
                    False

                Just actionName ->
                    actionName == serverAction.name
    in
    case ( serverAction.confirmable, displayConfirmation ) of
        ( True, False ) ->
            let
                updateAction =
                    GotServerActionNamePendingConfirmation <| Just serverAction.name
            in
            renderActionButton context serverAction (Just updateAction) serverAction.name Nothing

        ( True, True ) ->
            let
                cancelMsg =
                    Just <| GotServerActionNamePendingConfirmation Nothing

                title =
                    confirmationMessage serverAction

                ( actionOption, actionOptionMsg ) =
                    renderServerActionOption context project model server serverAction
            in
            Element.column
                [ Element.spacing spacer.px8 ]
            <|
                List.concat
                    [ [ renderConfirmationButton context serverAction (Just actionOptionMsg) cancelMsg title closeActionsDropdown ]
                    , actionOption
                    ]

        ( _, _ ) ->
            -- This is ugly, we should have an explicit custom type for server actions and match on that
            if String.toLower serverAction.name == String.toLower context.localization.staticRepresentationOfBlockDeviceContents then
                -- Overriding button for image, because we just want to navigate to another page
                Element.link [ Element.width Element.fill ]
                    { url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ServerCreateImage server.osProps.uuid <|
                                    Just <|
                                        server.osProps.name
                                            ++ {- @nonlocalized -} "-image"
                            )
                    , label =
                        renderActionButton
                            context
                            serverAction
                            (Just NoOp)
                            (Helpers.String.toTitleCase context.localization.staticRepresentationOfBlockDeviceContents)
                            (Just closeActionsDropdown)
                    }
                -- This is similarly ugly

            else if serverAction.name == "Resize" then
                -- Overriding button for resize, because we just want to navigate to another page
                Element.link [ Element.width Element.fill ]
                    { url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ServerResize server.osProps.uuid
                            )
                    , label =
                        renderActionButton
                            context
                            serverAction
                            (Just NoOp)
                            (Helpers.String.toTitleCase "Resize")
                            (Just closeActionsDropdown)
                    }

            else
                let
                    actionMsg =
                        Just <| SharedMsg <| serverAction.action (GetterSetters.projectIdentifier project) server.osProps.uuid model.retainFloatingIpsWhenDeleting

                    title =
                        serverAction.name
                in
                renderActionButton context serverAction actionMsg title (Just closeActionsDropdown)


confirmationMessage : ServerActionOption -> String
confirmationMessage serverAction =
    "Are you sure you want to " ++ (serverAction.name |> String.toLower) ++ "?"


renderServerActionOption :
    View.Types.Context
    -> Project
    -> Model
    -> Server
    -> ServerActionOption
    -> ( List (Element.Element Msg), SharedMsg.SharedMsg )
renderServerActionOption context project model server serverAction =
    let
        floatingIps =
            GetterSetters.getServerFloatingIps project server.osProps.uuid

        hasFloatingIps =
            not <| List.isEmpty <| floatingIps

        noOption =
            ( [], serverAction.action (GetterSetters.projectIdentifier project) server.osProps.uuid False )
    in
    if hasFloatingIps then
        case serverAction.name of
            "Delete" ->
                deleteActionOption context project model server serverAction

            "Shelve" ->
                shelveActionOption context project model server serverAction floatingIps

            _ ->
                noOption

    else
        noOption


deleteActionOption :
    View.Types.Context
    -> Project
    -> Model
    -> Server
    -> ServerActionOption
    -> ( List (Element.Element Msg), SharedMsg.SharedMsg )
deleteActionOption context project model server serverAction =
    ( [ Input.checkbox
            []
            { onChange = GotRetainFloatingIpsWhenDeleting
            , icon = Input.defaultCheckbox
            , checked = model.retainFloatingIpsWhenDeleting
            , label =
                Input.labelRight []
                    (Element.text <|
                        String.join " "
                            [ "Keep the"
                            , context.localization.floatingIpAddress
                            , "of this"
                            , context.localization.virtualComputer
                            , "for future use"
                            ]
                    )
            }
      ]
    , serverAction.action (GetterSetters.projectIdentifier project) server.osProps.uuid model.retainFloatingIpsWhenDeleting
    )


shelveActionOption :
    View.Types.Context
    -> Project
    -> Model
    -> Server
    -> ServerActionOption
    -> List OSTypes.FloatingIp
    -> ( List (Element.Element Msg), SharedMsg.SharedMsg )
shelveActionOption context project model server serverAction floatingIps =
    ( [ Input.checkbox
            [ Element.padding spacer.px8 ]
            { onChange = GotDeleteFloatingIpsWhenShelving
            , icon = Input.defaultCheckbox
            , checked = model.deleteFloatingIpsWhenShelving
            , label =
                Input.labelRight []
                    (Element.text <|
                        String.join " "
                            [ "Release"
                            , context.localization.floatingIpAddress
                            , "from this"
                            , context.localization.virtualComputer
                            , "upon shelving."
                            ]
                    )
            }
      , let
            ipAddresses =
                floatingIps |> List.map .address |> List.filter Cidr.isValidIPv4

            nudge =
                "The "
                    ++ (context.localization.floatingIpAddress |> Helpers.String.pluralizeCount (List.length ipAddresses))
                    ++ " "
                    ++ String.join ", " ipAddresses
        in
        warningMessage context.palette <|
            if model.deleteFloatingIpsWhenShelving then
                String.join " "
                    [ nudge
                    , "will no longer be associated with this"
                    , context.localization.virtualComputer ++ "."
                    ]

            else
                String.join " "
                    [ nudge
                    , "will be retained by this"
                    , context.localization.virtualComputer
                    , "while shelved."
                    , "Please remember this is a scarce resource."
                    ]
      ]
    , serverAction.action (GetterSetters.projectIdentifier project) server.osProps.uuid model.deleteFloatingIpsWhenShelving
    )


serverActionSelectModButton : View.Types.Context -> SelectMod -> (Widget.TextButton Msg -> Element.Element Msg)
serverActionSelectModButton context selectMod =
    let
        buttonPalette =
            case selectMod of
                NoMod ->
                    (SH.materialStyle context.palette).button

                Primary ->
                    (SH.materialStyle context.palette).primaryButton

                Warning ->
                    (SH.materialStyle context.palette).warningButton

                Danger ->
                    (SH.materialStyle context.palette).dangerButton
    in
    Widget.textButton
        { buttonPalette
            | container =
                buttonPalette.container
                    ++ [ Element.width Element.fill ]
            , labelRow =
                buttonPalette.labelRow
                    ++ [ Element.centerX ]
            , text =
                buttonPalette.text
                    ++ [ Element.centerX ]
        }


renderActionButton : View.Types.Context -> ServerActionOption -> Maybe Msg -> String -> Maybe (Element.Attribute Msg) -> Element.Element Msg
renderActionButton context serverAction actionMsg title closeActionsDropdown =
    let
        additionalBtnAttribs =
            case closeActionsDropdown of
                Just closeActionsDropdown_ ->
                    [ closeActionsDropdown_ ]

                Nothing ->
                    []
    in
    Element.row
        [ Element.spacing spacer.px12, Element.width Element.fill ]
        [ Element.text serverAction.description
        , Element.el
            ([ Element.width <| Element.px 100, Element.alignRight ] ++ additionalBtnAttribs)
          <|
            serverActionSelectModButton context
                serverAction.selectMod
                { text = title
                , onPress = actionMsg
                }
        ]


renderConfirmationButton : View.Types.Context -> ServerActionOption -> Maybe SharedMsg.SharedMsg -> Maybe Msg -> String -> Element.Attribute Msg -> Element.Element Msg
renderConfirmationButton context serverAction actionMsg cancelMsg title closeActionsDropdown =
    Element.row
        [ Element.spacing spacer.px12 ]
        [ Element.text title
        , Element.el
            [ closeActionsDropdown ]
          <|
            serverActionSelectModButton context
                serverAction.selectMod
                { text = "Yes"
                , onPress = Maybe.map SharedMsg actionMsg
                }
        , Element.el
            []
          <|
            Widget.textButton (SH.materialStyle context.palette).button
                { text = "No"
                , onPress = cancelMsg
                }

        -- TODO hover text with description
        ]


resourceUsageCharts : View.Types.Context -> ( Time.Posix, Time.Zone ) -> Server -> Maybe ServerResourceQtys -> Element.Element Msg
resourceUsageCharts context currentTimeAndZone server maybeServerResourceQtys =
    let
        containerWidth =
            context.windowSize.width - 120

        chartsWidth =
            max 1075 containerWidth

        charts_ : Types.ServerResourceUsage.TimeSeries -> Element.Element Msg
        charts_ timeSeries =
            Element.column
                [ Element.width Element.fill
                , Element.spacing spacer.px8
                ]
                [ Page.ServerResourceUsageAlerts.view context (Tuple.first currentTimeAndZone) timeSeries
                , Page.ServerResourceUsageCharts.view
                    context
                    chartsWidth
                    currentTimeAndZone
                    maybeServerResourceQtys
                    timeSeries
                ]
    in
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Element.text <|
                String.join " "
                    [ "Charts not available because"
                    , context.localization.virtualComputer
                    , "was not created by Exosphere."
                    ]

        ServerFromExo exoOriginProps ->
            case exoOriginProps.resourceUsage.data of
                RDPP.DoHave history _ ->
                    if Dict.isEmpty history.timeSeries then
                        case exoOriginProps.exoSetupStatus.data of
                            RDPP.DoHave ( ExoSetupError, _ ) _ ->
                                Element.none

                            RDPP.DoHave ( ExoSetupTimeout, _ ) _ ->
                                Element.none

                            RDPP.DoHave ( ExoSetupWaiting, _ ) _ ->
                                Element.none

                            _ ->
                                let
                                    thirtyMinMillis =
                                        1000 * 60 * 30
                                in
                                if Helpers.serverLessThanThisOld server (Tuple.first currentTimeAndZone) thirtyMinMillis then
                                    Element.text <|
                                        String.join " "
                                            [ "No chart data yet. This"
                                            , context.localization.virtualComputer
                                            , "is new and may take a few minutes to start reporting data."
                                            ]

                                else
                                    Element.text "No chart data to show."

                    else
                        charts_ history.timeSeries

                _ ->
                    if exoOriginProps.exoServerVersion < 2 then
                        Element.text <|
                            String.join " "
                                [ "Charts not available because"
                                , context.localization.virtualComputer
                                , "was not created using a new enough build of Exosphere."
                                ]

                    else
                        Element.text <|
                            String.join " "
                                [ "Could not access the"
                                , context.localization.virtualComputer
                                , "console log, charts not available."
                                ]


renderIpAddresses : View.Types.Context -> Project -> Server -> Element.Element Msg
renderIpAddresses context project server =
    let
        disableWhenTransitioning value =
            if
                (GetterSetters.getServerExoActions project server.osProps.uuid).targetOpenstackStatus
                    /= Nothing
                    || server.exoProps.deletionAttempted
            then
                Nothing

            else
                value

        floatingIpAddressRows =
            if List.isEmpty (GetterSetters.getServerFloatingIps project server.osProps.uuid) then
                let
                    noFloatingIpAssignButton =
                        [ Element.text <|
                            String.join " "
                                [ "No"
                                , context.localization.floatingIpAddress
                                , "assigned."
                                ]
                        , Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix <|
                                    Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.FloatingIpAssign Nothing (Just server.osProps.uuid)
                            , label =
                                Widget.textButton
                                    (SH.materialStyle context.palette).button
                                    { text =
                                        String.join " "
                                            [ "Assign", Helpers.String.indefiniteArticle context.localization.floatingIpAddress, context.localization.floatingIpAddress ]
                                    , onPress =
                                        disableWhenTransitioning <| Just NoOp
                                    }
                            }
                        ]

                    isActive =
                        List.member server.osProps.details.openstackStatus [ OSTypes.ServerActive, OSTypes.ServerVerifyResize ]

                    isBecomingActive =
                        (GetterSetters.getServerExoActions project server.osProps.uuid).targetOpenstackStatus
                            |> Maybe.andThen List.head
                            |> Maybe.map (\status -> status == OSTypes.ServerActive)
                            |> Maybe.withDefault False
                in
                -- Is this server active or becoming active?
                case ( isActive || isBecomingActive, server.exoProps.floatingIpCreationOption ) of
                    ( True, DoNotUseFloatingIp ) ->
                        -- The server doesn't have a floating IP and we aren't waiting to create one, so give the user an option to assign one.
                        noFloatingIpAssignButton

                    ( True, _ ) ->
                        -- Floating IP is not yet created as part of server launch, but it might be soon.
                        [ Element.text <|
                            String.join " "
                                [ "No"
                                , context.localization.floatingIpAddress
                                , "yet, please wait."
                                ]
                        ]

                    ( False, _ ) ->
                        -- We're not currently waiting for automatic floating IP assignment.
                        -- Give the user the option to assign one to e.g. a shelved server.
                        noFloatingIpAssignButton

            else
                GetterSetters.getServerFloatingIps project server.osProps.uuid
                    |> List.map
                        (\ipAddress ->
                            let
                                records =
                                    OpenStack.DnsRecordSet.lookupRecordsByAddress (RDPP.withDefault [] project.dnsRecordSets) ipAddress.address
                            in
                            Element.column [ Element.spacing spacer.px12 ]
                                ((case records of
                                    [] ->
                                        [ VH.compactKVSubRow
                                            (context.localization.hostname |> Helpers.String.toTitleCase)
                                            (Element.row [ Element.spacing spacer.px16 ]
                                                [ Button.default
                                                    context.palette
                                                    { text =
                                                        "Create"
                                                    , onPress =
                                                        GetterSetters.getDefaultZone project context
                                                            |> Maybe.map
                                                                (\zone ->
                                                                    SharedMsg <|
                                                                        SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                                                            SharedMsg.ServerMsg server.osProps.uuid <|
                                                                                SharedMsg.RequestCreateServerHostname ( zone, ipAddress.address )
                                                                )
                                                            |> disableWhenTransitioning
                                                    }
                                                ]
                                            )
                                        ]

                                    _ ->
                                        records
                                            |> List.indexedMap
                                                (\i r ->
                                                    VH.compactKVSubRow
                                                        (if i == 0 then
                                                            Helpers.String.pluralizeCount (List.length records) (context.localization.hostname |> Helpers.String.toTitleCase)

                                                         else
                                                            ""
                                                        )
                                                        (Element.row [ Element.spacing spacer.px16 ]
                                                            [ copyableText context.palette
                                                                []
                                                                (if String.endsWith "." r.name then
                                                                    String.dropRight 1 r.name

                                                                 else
                                                                    r.name
                                                                )
                                                            ]
                                                        )
                                                )
                                 )
                                    ++ [ VH.compactKVSubRow
                                            (Helpers.String.toTitleCase context.localization.floatingIpAddress)
                                            (Element.row [ Element.spacing spacer.px16 ]
                                                [ copyableText context.palette [] ipAddress.address
                                                , Button.default
                                                    context.palette
                                                    { text =
                                                        "Unassign"
                                                    , onPress =
                                                        disableWhenTransitioning <|
                                                            Just <|
                                                                SharedMsg <|
                                                                    SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                                                        SharedMsg.RequestUnassignFloatingIp ipAddress.uuid
                                                    }
                                                ]
                                            )
                                       ]
                                )
                        )

        fixedIpAddressRows =
            GetterSetters.getServerFixedIps project server.osProps.uuid
                |> List.map
                    (\ipAddress ->
                        let
                            toggleTipId =
                                Helpers.String.hyphenate
                                    [ "fixedIpAddressTip"
                                    , project.auth.project.uuid
                                    , server.osProps.uuid
                                    , ipAddress
                                    ]

                            toggleTip text =
                                Style.Widgets.ToggleTip.toggleTip
                                    context
                                    popoverMsgMapper
                                    toggleTipId
                                    (Element.text text)
                                    ST.PositionBottomRight

                            ( label, elements ) =
                                if Cidr.isValidIPv6 ipAddress then
                                    ( context.localization.publiclyRoutableIpAddress
                                    , [ copyableText context.palette [] ipAddress, toggleTip "An IPv6 address that is (generally) publicly routable." ]
                                    )

                                else
                                    ( context.localization.nonFloatingIpAddress
                                    , [ Text.body ipAddress, toggleTip "An IPv4 address on the internal network." ]
                                    )
                        in
                        VH.compactKVSubRow
                            (Helpers.String.toTitleCase label)
                            (Element.row [ Element.spacing spacer.px8 ] elements)
                    )
    in
    Element.column
        [ Element.spacing spacer.px8 ]
        (floatingIpAddressRows
            ++ fixedIpAddressRows
        )


serverVolumes : View.Types.Context -> Project -> Server -> List OSTypes.Volume -> Element.Element Msg
serverVolumes context project server volumes =
    case List.length volumes of
        0 ->
            Element.text "(none)"

        _ ->
            let
                volumeRow v =
                    let
                        ( device, mountpoint ) =
                            if GetterSetters.isVolumeCurrentlyBackingServer project (Just server.osProps.uuid) v then
                                ( String.join " "
                                    [ "Boot"
                                    , context.localization.blockDevice
                                    ]
                                , ""
                                )

                            else
                                case GetterSetters.volumeDeviceRawName project server v.uuid of
                                    Just device_ ->
                                        ( device_
                                        , Maybe.withDefault "Could not determine" <|
                                            if GetterSetters.serverSupportsFeature NamedMountpoints server then
                                                v.name |> Maybe.andThen GetterSetters.volNameToMountpoint

                                            else
                                                GetterSetters.volDeviceToMountpoint (Just device_)
                                        )

                                    Nothing ->
                                        ( "Could not determine", "" )
                    in
                    { name = VH.resourceName v.name v.uuid
                    , uuid = v.uuid
                    , device = device
                    , mountpoint = mountpoint
                    }

                columns =
                    List.concat
                        [ [ { header = Text.strong "Name"
                            , width = Element.shrink
                            , view =
                                \v ->
                                    Element.link []
                                        { url =
                                            Route.toUrl context.urlPathPrefix
                                                (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                                    Route.VolumeDetail v.uuid
                                                )
                                        , label =
                                            Element.el
                                                [ Font.color (SH.toElementColor context.palette.primary), Element.width (Element.px 180) ]
                                                (VH.ellipsizedText <| v.name)
                                        }
                            }
                          ]
                        , if GetterSetters.serverSupportsFeature NamedMountpoints server then
                            []

                          else
                            [ { header = Text.strong "Device"
                              , width = Element.fill
                              , view = \v -> Element.text v.device
                              }
                            ]
                        , [ { header = Text.strong "Mount point"
                            , width = Element.fill
                            , view = \v -> scrollableCell [ Element.width Element.fill ] <| Text.mono <| v.mountpoint
                            }
                          ]
                        ]
            in
            Element.table
                [ Element.spacing spacer.px16
                ]
                { data =
                    volumes
                        |> List.map volumeRow
                        |> List.sortBy .device
                , columns =
                    columns
                }


renderServerVolumes : View.Types.Context -> Project -> Server -> Element.Element Msg
renderServerVolumes context project server =
    let
        renderTable volumes =
            serverVolumes
                context
                project
                server
                (volumes |> List.filter (\v -> List.member v.uuid server.osProps.details.volumesAttached))
    in
    VH.renderRDPP context
        project.volumes
        (context.localization.blockDevice |> Helpers.String.pluralize)
        renderTable
