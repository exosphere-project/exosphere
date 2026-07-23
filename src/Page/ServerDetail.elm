module Page.ServerDetail exposing (Model, Msg(..), PassphraseVisibility, VerboseStatus, clearResolvedPendingEmbed, effectiveExoextResultBody, exoextEmbedProjection, exoextManifestBodyForEtag, exoextManifestNeedsFetch, exoextRequestsPending, exoextScanRequestPending, exoextScanTimer, init, update, view)

import CloudShield.Card
import DateFormat.Relative
import Dict
import Element
import Element.Font as Font
import Element.Input as Input
import Exoext.Discovery
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
    , exoextHistory : RDPP.RemoteDataPlusPlus String (List Exoext.Transport.IndexEntry)
    , exoextHistoryRequestKey : Maybe String

    -- the in-flight history-View getEmbed request, as the generic `Exoext.Lifecycle.PendingRequest`
    -- (`kind == "getEmbed"`, `subject` = the archived batch id), recorded when its req slot is
    -- written so the card can show a spinner while the bridge mints a fresh embed (~10s over the
    -- next poll) and can time the request out. Single-slot: a newer getEmbed replaces it; a
    -- matching-`requestId` result clears it (see `clearResolvedPendingEmbed`). `Nothing` when
    -- nothing is in flight. This is the session-request view of `PendingRequest`; the scan-request
    -- view lives in the card model (`CloudShield.Card.Model.pending`).
    , exoextPendingEmbed : Maybe Exoext.Lifecycle.PendingRequest
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
    | GotExoextSync
    | GotExoextManifestObject Time.Posix String (Result HttpErrorWithBody String)
    | GotExoextResultObject Time.Posix String String (Result HttpErrorWithBody String)
    | GotExoextIndexObject Time.Posix String (Result HttpErrorWithBody String)
    | CloudShieldMsg CloudShield.Card.Msg
    | ExoextWriteRequest { seq : Int, targetIds : List String } Time.Posix
    | ExoextWriteEmbedRequest { batchId : String } Time.Posix
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
    , exoextPendingEmbed = Nothing
    }


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

        GotExoextSync ->
            let
                ( newModel, cmd ) =
                    syncExoextReads project model
            in
            ( newModel, cmd, SharedMsg.NoOp )

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

                ( cmd, sharedMsg ) =
                    case outMsg of
                        Just (CloudShield.Card.ScanRequested req) ->
                            -- Fetch the real wall-clock time so the §4.1 `createdAt` is genuine
                            -- (the agent's §4.4 expiry guard compares it to REQUEST_TTL; a
                            -- placeholder epoch would be treated as expired and never run).
                            ( Task.perform (ExoextWriteRequest req) Time.now, SharedMsg.NoOp )

                        Just (CloudShield.Card.EmbedRequested req) ->
                            -- §7.1 single-req-slot guard, from the live wire state: don't write a
                            -- getEmbed while a scan is active or its request is still unclaimed —
                            -- the seq bump would cancel that scan bridge-side. Silently ignore the
                            -- press when blocked (spec behavior). Otherwise stamp a genuine
                            -- wall-clock seq/`createdAt` (same as a scan request).
                            if exoextGetEmbedBlocked project model then
                                ( Cmd.none, SharedMsg.NoOp )

                            else
                                ( Task.perform (ExoextWriteEmbedRequest req) Time.now, SharedMsg.NoOp )

                        Just CloudShield.Card.ApprovalGranted ->
                            -- Stamp a genuine wall-clock `approvedAt`, same idiom as a scan request:
                            -- fetch `Time.now`, then build and persist the approval record.
                            ( Task.perform ExoextWriteApproval Time.now, SharedMsg.NoOp )

                        Just CloudShield.Card.ApprovalForgotten ->
                            -- Forgetting needs no timestamp; drop the record for this instance.
                            ( Cmd.none, SharedMsg.ForgetExtensionApproval model.serverUuid )

                        Nothing ->
                            ( Cmd.none, SharedMsg.NoOp )
            in
            ( { model | exoextCard = cloudModel }
            , cmd
            , sharedMsg
            )

        ExoextWriteRequest req now ->
            let
                -- Use wall-clock millis as the req-slot seq: the card's own counter resets to 0
                -- on reload and would collide with a still-claimed seq persisted in the instance
                -- metadata (the agent then treats the new request as already-handled and skips it).
                -- A time-based seq is monotonic across reloads, so it never collides. Sync the
                -- card's `pending.seq` to it so the run<->request correlation still matches.
                timeSeq =
                    Time.posixToMillis now

                oldCloud =
                    model.exoextCard

                syncedCloud =
                    { oldCloud
                        | pending = Maybe.map (\p -> { p | seq = timeSeq }) oldCloud.pending
                    }
            in
            ( { model | exoextCard = syncedCloud }
            , writeScanRequestCmd project model (exoextInstances project model) { req | seq = timeSeq } now
            , SharedMsg.NoOp
            )

        ExoextWriteEmbedRequest req now ->
            -- getEmbed does not touch the card's scan state or `pending`; it just writes the
            -- §7.1 req slot. The embed result is correlated later by `kind == "embed"`, not by
            -- `run.state`. Record a single-slot pending marker so the card can show a spinner and
            -- time the request out; its `requestId` mirrors the one `writeEmbedRequestCmd` stamps
            -- (both derived from the same `now`) so a matching result later clears it.
            ( { model
                | exoextPendingEmbed =
                    Just
                        { seq = Time.posixToMillis now
                        , requestId = "exo-cs-req-" ++ String.fromInt (Time.posixToMillis now)
                        , kind = "getEmbed"
                        , subject = req.batchId
                        , since = now
                        }
              }
            , writeEmbedRequestCmd project model req now
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


{-| The §2.4 `$instances` projection: the project's real servers, eligibility-filtered
(ACTIVE-only, excluding the publishing instance itself). Computed identically in `update` and
`view` so the renderer's row indices stay stable. The VM never supplies this list — it comes
from Exosphere's own server data, so it cannot inject fake or out-of-project rows.
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
                }
            )
        |> Exoext.Discovery.eligibleInstances model.serverUuid


{-| The §7.1 single-req-slot guard for a `getEmbed`: block only when writing it would cancel a
genuinely in-flight SCAN — a `queued`/`running` run, or a scan this reader just wrote whose req is
still unclaimed on the wire (correlated by the scan's `pending.seq`). Crucially it does NOT block
just because a prior _getEmbed_ left the req slot unclaimed (e.g. after a timeout), so one View can
always supersede another and a timed-out getEmbed never wedges future clicks. No server (nothing to
write against) counts as blocked. See `Exoext.Transport.getEmbedBlocked`.
-}
exoextGetEmbedBlocked : Project -> Model -> Bool
exoextGetEmbedBlocked project model =
    case GetterSetters.serverLookup project model.serverUuid of
        Just server ->
            Exoext.Transport.getEmbedBlocked
                (Maybe.map .seq model.exoextCard.pending)
                server.osProps.details.metadata

        Nothing ->
            True


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


syncExoextReads : Project -> Model -> ( Model, Cmd Msg )
syncExoextReads project model =
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
                    }
            in
            case Exoext.Discovery.readSentinel metadata of
                Just ({ store } as sentinel) ->
                    if store == Exoext.Discovery.StoreSwift then
                        let
                            etag =
                                exoextEtag metadata
                        in
                        syncExoextIndex project sentinel (Exoext.Transport.historyRefreshKey metadata) <|
                            syncExoextResultRef project sentinel etag metadata <|
                                syncExoextManifest project sentinel etag syncedModel

                    else
                        ( syncedModel, Cmd.none )

                Nothing ->
                    ( syncedModel, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


{-| Resolve the pending getEmbed marker against fresh metadata: clear it once a res-slot embed
result carrying the same `requestId` appears (the bridge answered this request). A result for a
different (earlier) requestId leaves the marker in place — a newer request is still in flight.
-}
clearResolvedPendingEmbed : List OSTypes.MetadataItem -> Maybe Exoext.Lifecycle.PendingRequest -> Maybe Exoext.Lifecycle.PendingRequest
clearResolvedPendingEmbed metadata pending =
    pending
        |> Maybe.andThen
            (\p ->
                case Exoext.Transport.resultBodyFromMetadata metadata |> Maybe.andThen Exoext.Transport.embedResultFromBody of
                    Just embed ->
                        if embed.requestId == p.requestId then
                            Nothing

                        else
                            Just p

                    Nothing ->
                        Just p
            )


syncExoextManifest : Project -> Exoext.Discovery.Sentinel -> String -> Model -> ( Model, Cmd Msg )
syncExoextManifest project sentinel etag model =
    case ( project.endpoints.swift, Exoext.Discovery.manifestObjectLocation sentinel ) of
        ( Nothing, _ ) ->
            ( exoextManifestError (Time.millisToPosix 0)
                etag
                {- @nonlocalized -} "CloudShield manifest is stored in object storage, but this cloud has no Swift endpoint."
                model
            , Cmd.none
            )

        ( _, Nothing ) ->
            ( exoextManifestError (Time.millisToPosix 0) etag "CloudShield manifest is stored in object storage, but its container or object name is missing." model
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


{-| The object to fetch for the current res-slot body, if any: a `{"ref": ...}` scan-result
pointer names its object directly; an embed result (`kind == "embed"`, `status == "ok"`) names
the archived scan body `<prefix>results/<batchId>.json` — the same capped ref-fetch path serves
both. An inline scan result or a non-ok embed result names nothing.
-}
exoextResultObjectName : Exoext.Discovery.Sentinel -> List OSTypes.MetadataItem -> Maybe String
exoextResultObjectName sentinel metadata =
    Exoext.Transport.resultBodyFromMetadata metadata
        |> Maybe.andThen
            (\body ->
                case Exoext.Transport.embedResultFromBody body of
                    Just embed ->
                        if embed.status == "ok" then
                            Just (Maybe.withDefault "" sentinel.prefix ++ "results/" ++ embed.batchId ++ ".json")

                        else
                            Nothing

                    Nothing ->
                        Exoext.Transport.resultRefObjectName (Exoext.Transport.resolveResultBody body)
            )


syncExoextResultRef : Project -> Exoext.Discovery.Sentinel -> String -> List OSTypes.MetadataItem -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
syncExoextResultRef project sentinel etag metadata ( model, manifestCmd ) =
    case exoextResultObjectName sentinel metadata of
        Just objectName ->
            case ( project.endpoints.swift, sentinel.container ) of
                ( Nothing, _ ) ->
                    ( exoextResultRefError (Time.millisToPosix 0)
                        etag
                        objectName
                        {- @nonlocalized -} "CloudShield result is stored in object storage, but this cloud has no Swift endpoint."
                        model
                    , manifestCmd
                    )

                ( _, Nothing ) ->
                    ( exoextResultRefError (Time.millisToPosix 0) etag objectName "CloudShield result is stored in object storage, but its container is missing." model
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
                        (Exoext.Transport.indexObjectName (Maybe.withDefault "" sentinel.prefix))
                        Exoext.Transport.indexCapBytes
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
-}
receiveExoextIndex : Project -> Time.Posix -> String -> Result HttpErrorWithBody String -> Model -> Model
receiveExoextIndex project receivedTime refreshKey result model =
    if currentExoextHistoryKey project model == Just refreshKey then
        let
            nextHistory =
                case result of
                    Ok body ->
                        RDPP.RemoteDataPlusPlus
                            (RDPP.DoHave (Exoext.Transport.decodeIndex body) receivedTime)
                            (RDPP.NotLoading Nothing)

                    Err _ ->
                        case model.exoextHistory.data of
                            RDPP.DoHave _ _ ->
                                RDPP.setNotLoading Nothing model.exoextHistory

                            RDPP.DontHave ->
                                RDPP.RemoteDataPlusPlus
                                    (RDPP.DoHave [] receivedTime)
                                    (RDPP.NotLoading Nothing)
        in
        { model
            | exoextHistory = nextHistory
            , exoextHistoryRequestKey = Just refreshKey
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
            case ( Exoext.Transport.runStatusFromMetadata metadata, model.exoextCard.pending ) of
                ( Just status, Just pending ) ->
                    if status.seq == pending.seq then
                        Just { targetId = pending.subject, state = status.state }

                    else
                        Nothing

                _ ->
                    Nothing

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

        -- The reader projection for the history-View embed flow: the findings/`embedUrl` binding
        -- (a valid history pick wins over the live scan result), the host-side `embedState` line,
        -- and whether a history pick is active (used to hide the live-scan timer). `embedUrl` is
        -- gated on `embedState`: only `EmbedReady` carries the live URL, so an expired/errored/
        -- loading embed unmounts the iframe (the resource-drain fix).
        embedProjection =
            exoextEmbedProjection metadata currentTime statusOverride resultBody model

        results =
            embedProjection.results

        embedUrl =
            embedProjection.embedUrl

        -- The scan-completion descriptor for the card's frozen confirmation line. It exists only
        -- while a run is tracked this session (`pending` set) and drops the moment the run is
        -- terminal-but-not-done. It is independent of any concurrent history view: the *running*
        -- progress now lives on the scanning target row (`displayStatusOverride`), so the timer is
        -- no longer suppressed while a past scan is "Now viewing". See `exoextScanTimer`.
        scanTimer =
            exoextScanTimer
                (Maybe.map .seq model.exoextCard.pending)
                (statusOverride |> Maybe.map .state |> Maybe.withDefault "queued")
                resultBody

        -- The row display state (left column). Raw `statusOverride` drives the reader logic above
        -- (embed projection, completion timer); here, while the run is `running`, we compose the
        -- counting-up elapsed into its state (`"scanning · m:ss"`) so the scanning row is the live
        -- progress signal. `queued`/`done`/terminal states pass through raw.
        displayStatusOverride =
            case ( statusOverride, model.exoextCard.pending ) of
                ( Just override, Just pending ) ->
                    if override.state == "running" then
                        Just
                            { targetId = override.targetId
                            , state = CloudShield.Card.scanningRowLabel pending.seq (Time.posixToMillis currentTime)
                            }

                    else
                        Just override

                ( other, _ ) ->
                    other
    in
    { approved = approved
    , sourceName = server.osProps.name
    , manifest = manifestSource
    , transportLabel = transportLabel
    , transportWarning = exoextTransportWarning project model metadata maybeSentinel resultBody
    , scanTimer = scanTimer
    , statusOverride = displayStatusOverride
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
    , activeBatchId = embedProjection.activeBatchId
    , pendingBatchId = embedProjection.pendingBatchId
    , erroredBatchId = embedProjection.erroredBatchId
    , expiredBatchId = embedProjection.expiredBatchId
    , allowedIframeOrigins = allowedIframeOrigins
    , embedUrl = embedUrl
    , embedState = embedProjection.embedState

    -- The results iframe is now the catalog's origin-pinned `Iframe` element (bound to the scan
    -- result's embedUrl). The old raw/unpinned demo panel is disabled to avoid a second, confusing
    -- iframe; `Nothing` hides the panel and its toggle entirely.
    , demoIframeUrl = Nothing
    }


{-| The scan-completion timer descriptor, derived purely from the tracked run's wall-clock start
(`Just` the request seq, else `Nothing` when no scan is tracked), the correlated live `state`, and
the fetched result body. Independent of any history view — a scan that is queued/running/done keeps
its descriptor while a past scan is viewed on the right (its running progress renders on the row).

Shapes:

  - no tracked run (`Nothing` start) ⇒ `Nothing` (no descriptor).
  - `done` ⇒ freeze to the full wall-clock flow, `completedAt - startMillis` (minutes; snapshot →
    boot clone → scan). It never falls back to `summary.durationSec` (the scanner-only ~seconds,
    which reads as "faster than reality"); if `completedAt` is missing/unparseable the frozen
    duration is `Nothing` (no line) rather than a misleadingly small number.
  - a terminal non-`done` state (`error`/`cancelled`/`expired`) ⇒ `Nothing` (drop the line).
  - otherwise (queued/running) ⇒ a live descriptor with `doneDurationSec = Nothing`; the running
    elapsed itself is drawn on the scanning row, not here.

-}
exoextScanTimer : Maybe Int -> String -> Maybe String -> Maybe { startMillis : Int, doneDurationSec : Maybe Int }
exoextScanTimer maybeStartMillis state resultBody =
    maybeStartMillis
        |> Maybe.andThen
            (\startMillis ->
                case state of
                    "done" ->
                        Just
                            { startMillis = startMillis
                            , doneDurationSec =
                                resultBody
                                    |> Maybe.andThen
                                        (\body ->
                                            Decode.decodeString (Decode.field "completedAt" Decode.string) body
                                                |> Result.toMaybe
                                        )
                                    |> Maybe.andThen (Helpers.Time.iso8601StringToPosix >> Result.toMaybe)
                                    |> Maybe.map (\completedAt -> max 0 ((Time.posixToMillis completedAt - startMillis) // 1000))
                            }

                    "error" ->
                        Nothing

                    "cancelled" ->
                        Nothing

                    "expired" ->
                        Nothing

                    _ ->
                        Just { startMillis = startMillis, doneDurationSec = Nothing }
            )


{-| How long a written getEmbed waits for its result before the pending marker is treated as a
timeout error. getEmbed normally resolves in ~10s (one poll after the bridge claims the slot); 30s
leaves headroom for a slow mint + CloudShield without leaving a dead "Opening…" row up so long it
reads as broken.
-}
embedRequestTimeoutMillis : Int
embedRequestTimeoutMillis =
    30 * 1000


{-| The reader projection for the history-View embed flow, decided purely from this instance's
metadata, the fetched archived body (`model.exoextResultRef`), the pending getEmbed marker,
and the shared client clock. Returns the findings/`embedUrl` binding for the renderer, the
host-side `embedState` line, and the active/pending/errored batch ids for per-row state.

An ok embed result in the res slot is a history pick and wins over the live scan result for both
the findings table and the iframe; otherwise the scan-result path is used exactly as before.
`embedUrl` is gated on `embedState`: only `EmbedReady` carries the live URL, so an expired
(`embedExpiresAt <= currentTime`), errored, or in-flight embed emits `""` and the origin-pinned
Iframe self-hides. That unmount is the resource-drain fix (an expired CloudShield+Clerk app kept
mounted spins forever on auth retries).

-}
exoextEmbedProjection :
    List OSTypes.MetadataItem
    -> Time.Posix
    -> Maybe { targetId : String, state : String }
    -> Maybe String
    -> Model
    -> { results : Maybe Encode.Value, embedUrl : String, embedState : CloudShield.Card.EmbedState, activeBatchId : Maybe String, pendingBatchId : Maybe String, erroredBatchId : Maybe String, expiredBatchId : Maybe String }
exoextEmbedProjection metadata currentTime statusOverride resultBody model =
    let
        rawEmbedResult =
            Exoext.Transport.resultBodyFromMetadata metadata
                |> Maybe.andThen Exoext.Transport.embedResultFromBody

        -- A history pick to show: an ok embed result in the res slot (fresh OR expired). A non-ok
        -- embed result never binds findings/iframe (it falls to the scan path).
        maybeEmbedResult =
            rawEmbedResult
                |> Maybe.andThen
                    (\embed ->
                        if embed.status == "ok" then
                            Just embed

                        else
                            Nothing
                    )

        -- Normalize the wire embed result into the generic `Exoext.Lifecycle.ResultInput` (unwrap
        -- the error message; parse the ISO expiry once — unparseable/absent = no expiry = Fresh).
        -- The generic `sessionState` machine then classifies opening / open / stale / failed / idle.
        resultInput =
            rawEmbedResult
                |> Maybe.map
                    (\embed ->
                        { requestId = embed.requestId
                        , subject = embed.batchId
                        , status = embed.status
                        , url = embed.embedUrl
                        , expiresAt =
                            ISO8601.fromString embed.embedExpiresAt
                                |> Result.toMaybe
                                |> Maybe.map ISO8601.toPosix
                        , message = embedErrorMessage embed
                        }
                    )

        session =
            Exoext.Lifecycle.sessionState embedRequestTimeoutMillis currentTime model.exoextPendingEmbed resultInput

        -- The host-side embed line + iframe gate, mapped 1:1 from the generic session token.
        embedState =
            case session of
                Exoext.Lifecycle.NoSession ->
                    CloudShield.Card.EmbedIdle

                Exoext.Lifecycle.Opening _ ->
                    CloudShield.Card.EmbedLoading

                Exoext.Lifecycle.Open _ ->
                    CloudShield.Card.EmbedReady

                Exoext.Lifecycle.OpenStale _ ->
                    CloudShield.Card.EmbedExpired

                Exoext.Lifecycle.Failed { message } ->
                    CloudShield.Card.EmbedError message

        -- The expiry fix: an ok-but-expired session is `OpenStale`, so the previously-viewed row
        -- becomes `expiredBatchId` (a muted "Expired" / plain-View row) INSTEAD of `activeBatchId`
        -- ("Now viewing" / Refresh). A fresh ok session stays active as before.
        expiredBatchId =
            case session of
                Exoext.Lifecycle.OpenStale expiredSession ->
                    Just expiredSession.resultId

                _ ->
                    Nothing

        ( results, embedUrl ) =
            case maybeEmbedResult of
                Just _ ->
                    -- Findings for the selected history scan, parsed from the archived body fetched
                    -- into `exoextResultRef` (the fetch is deduped by objectName via
                    -- `exoextResultRefRequest`). The last-fetched body is kept so `/results` is
                    -- not clobbered while the newly selected batch's body is still in flight. The
                    -- etag check does not give scan-freshness (the etag is a constant manifest hash);
                    -- it only rejects a body left over from a different instance/manifest.
                    let
                        archivedFindings =
                            case model.exoextResultRef.data of
                                RDPP.DoHave fetched _ ->
                                    if fetched.etag == exoextEtag metadata then
                                        Decode.decodeString (Decode.field "findings" Decode.value) fetched.body
                                            |> Result.toMaybe

                                    else
                                        Nothing

                                RDPP.DontHave ->
                                    Nothing
                    in
                    ( archivedFindings
                    , case session of
                        Exoext.Lifecycle.Open openSession ->
                            -- Only a fresh, open session mounts the iframe (its live URL from the
                            -- generic session). Expired/loading keep the findings but unmount it.
                            openSession.url

                        _ ->
                            ""
                    )

                Nothing ->
                    -- The scan-result path: when the in-flight run is `done`, bind the §4.2 result
                    -- body's `findings[]` into `/results` and its `embedUrl` into `/embedUrl`. Gated
                    -- on the correlated `done` state so a stale prior-run result can't show.
                    let
                        atDone decoder =
                            case statusOverride of
                                Just override ->
                                    if override.state == "done" then
                                        resultBody |> Maybe.andThen decoder

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                    in
                    ( atDone (Decode.decodeString (Decode.field "findings" Decode.value) >> Result.toMaybe)
                    , atDone (Decode.decodeString (Decode.field "embedUrl" Decode.string) >> Result.toMaybe)
                        |> Maybe.withDefault ""
                    )

        -- The batchId whose findings/embed are on screen, so the card can flag exactly one
        -- history row as "Now viewing": the picked history row wins (only while its session is not
        -- expired — an expired session becomes `expiredBatchId` above), else the just-completed
        -- live scan (read from the result body only when the correlated run is `done`).
        activeBatchId =
            case maybeEmbedResult of
                Just embed ->
                    if expiredBatchId == Nothing then
                        Just embed.batchId

                    else
                        Nothing

                Nothing ->
                    case statusOverride of
                        Just override ->
                            if override.state == "done" then
                                -- The just-completed live scan. Its result body's `batchId` is
                                -- `null` for a scan request (we send `batchId: null`), so mirror the
                                -- history index key (`batchId or requestId`) and fall back to the
                                -- `requestId`; otherwise the new history row would key on the
                                -- requestId while `activeBatchId` was `Nothing`, leaving it stuck on
                                -- "View" instead of "Now viewing".
                                resultBody
                                    |> Maybe.andThen
                                        (\body ->
                                            case Decode.decodeString (Decode.field "batchId" Decode.string) body |> Result.toMaybe of
                                                Just batchId ->
                                                    Just batchId

                                                Nothing ->
                                                    Decode.decodeString (Decode.field "requestId" Decode.string) body |> Result.toMaybe
                                        )

                            else
                                Nothing

                        Nothing ->
                            Nothing

        -- The in-flight getEmbed's batch (`Opening`) for the per-row "Opening…" loading state, and
        -- the last failed getEmbed's batch (`Failed`) for the per-row "Couldn't open" + Retry state.
        -- Both come straight from the generic session token, so each is `Just` only in its state and
        -- clears the moment the request resolves — no row is ever wedged loading or errored.
        pendingBatchId =
            case session of
                Exoext.Lifecycle.Opening { subject } ->
                    Just subject

                _ ->
                    Nothing

        erroredBatchId =
            case session of
                Exoext.Lifecycle.Failed { subject } ->
                    Just subject

                _ ->
                    Nothing
    in
    { results = results
    , embedUrl = embedUrl
    , embedState = embedState
    , activeBatchId = activeBatchId
    , pendingBatchId = pendingBatchId
    , erroredBatchId = erroredBatchId
    , expiredBatchId = expiredBatchId
    }


{-| The user-facing text of an embed result's `error`. `EmbedResult.error` holds the error field
re-encoded as a JSON string (the bridge may send a string or an object), so unwrap a plain-string
error back to its text for a clean line, falling back to the raw JSON for a structured error.
-}
embedErrorMessage : Exoext.Transport.EmbedResult -> String
embedErrorMessage embed =
    case embed.error of
        Just encoded ->
            Decode.decodeString Decode.string encoded |> Result.withDefault encoded

        Nothing ->
            "the request failed"


exoextTransportWarning : Project -> Model -> List OSTypes.MetadataItem -> Maybe Exoext.Discovery.Sentinel -> Maybe String -> Maybe String
exoextTransportWarning project model metadata maybeSentinel resultBody =
    case resultBody |> Maybe.andThen resultTruncationWarning of
        Just warning ->
            Just warning

        Nothing ->
            case maybeSentinel of
                Just { store } ->
                    if store == Exoext.Discovery.StoreSwift then
                        case project.endpoints.swift of
                            Nothing ->
                                Just
                                    {- @nonlocalized -} "CloudShield manifest is stored in object storage, but this cloud has no Swift endpoint."

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
                        Just ("CloudShield object-storage manifest read failed: " ++ error)

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
                        Just ("CloudShield object-storage result read failed: " ++ error)

                    else
                        Nothing

                _ ->
                    Nothing


resultTruncationWarning : String -> Maybe String
resultTruncationWarning body =
    case Decode.decodeString (Decode.field "truncated" Decode.bool) body of
        Ok True ->
            let
                suffix =
                    Decode.decodeString (Decode.field "truncatedReason" Decode.string) body
                        |> Result.toMaybe
                        |> Maybe.map (\reason -> ": " ++ reason)
                        |> Maybe.withDefault ""
            in
            Just
                {- @nonlocalized -} ("Full results are too large for this cloud's metadata transport" ++ suffix)

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


{-| Write the §7.1 metadata request-slot for a confirmed scan. Per §7.1 single-in-flight, the
POC writes the request for the **first** target only (a batch/select-all drains sequentially
behind it); the optimistic `queued` badges for the rest are already set in the card model. The
request slot is written on the **CloudShield VM's own** metadata (the viewed instance).

POC limitations (dropped at `store=swift`, Phase 1b): `requestId` is seq-derived rather than a
UUID. `createdAt` is now a real wall-clock timestamp (`Time.now` via `ExoextWriteRequest`),
so the agent's §4.4 expiry guard works. The §4.1 JSON shape and §7.1 framing are exact and
unit-tested (`Tests.CloudShield.Card`).

-}
writeScanRequestCmd : Project -> Model -> List CloudShield.Card.Instance -> { seq : Int, targetIds : List String } -> Time.Posix -> Cmd Msg
writeScanRequestCmd project model instances req now =
    case req.targetIds of
        [] ->
            Cmd.none

        firstId :: _ ->
            let
                -- §5.4: re-resolve the target id against Exosphere's own instance list.
                targetName =
                    instances
                        |> List.Extra.find (\i -> i.id == firstId)
                        |> Maybe.map .name
                        |> Maybe.withDefault firstId

                scanRequest =
                    { requestId = "exo-cs-req-" ++ String.fromInt req.seq
                    , batchId =
                        if List.length req.targetIds > 1 then
                            Just ("exo-cs-batch-" ++ String.fromInt req.seq)

                        else
                            Nothing
                    , createdAt = ISO8601.toString (ISO8601.fromPosix now)
                    , projectId = project.auth.project.uuid
                    , target = { instanceId = firstId, instanceName = targetName }
                    , profile = "quick"
                    }

                items =
                    Exoext.Transport.reqSlotMetadata req.seq
                        (Exoext.Transport.scanRequestJson scanRequest)
            in
            -- One atomic POST for all request-slot keys. Firing one request per key
            -- concurrently races on Nova's single metadata row and silently drops writes,
            -- leaving a half-written (unparseable) request slot the agent never picks up.
            Rest.Nova.requestSetServerMetadataItems project model.serverUuid items
                |> Cmd.map SharedMsg


{-| Write the §7.1 metadata request-slot for a `getEmbed`. Same atomic single-POST req-slot
write as a scan request (via `reqSlotMetadata`), with a wall-clock-millis seq so it never
collides with a reload-reset counter. The bridge claims the slot and writes a small embed
result inline into the res slot; it does not update `run.state`.
-}
writeEmbedRequestCmd : Project -> Model -> { batchId : String } -> Time.Posix -> Cmd Msg
writeEmbedRequestCmd project model req now =
    let
        timeSeq =
            Time.posixToMillis now

        embedRequest =
            { requestId = "exo-cs-req-" ++ String.fromInt timeSeq
            , batchId = req.batchId
            , createdAt = ISO8601.toString (ISO8601.fromPosix now)
            }

        items =
            Exoext.Transport.reqSlotMetadata timeSeq
                (Exoext.Transport.embedRequestJson embedRequest)
    in
    Rest.Nova.requestSetServerMetadataItems project model.serverUuid items
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
        sentinelPresent =
            Exoext.Discovery.readSentinel server.osProps.details.metadata /= Nothing
    in
    if context.experimentalFeaturesEnabled && sentinelPresent then
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
            ([ Icon.featherIcon [] Icons.shield
             , Element.text "CloudShield (extension)"
             , extensionExperimentalTag context
             ]
                ++ headerChip
            )
            [ CloudShield.Card.view
                context.palette
                timeZone
                config
                (exoextInstances project model)
                model.exoextCard
                |> Element.map CloudShieldMsg
            ]

    else
        Element.none


{-| The "Experimental" tag for the CloudShield card header (plan §C3). Same idiom as the
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
                        [ Element.text ("Extensions are an experimental feature. This interface is published by a VM in your " ++ context.localization.unitOfTenancy ++ ", not by Exosphere.") ]
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
