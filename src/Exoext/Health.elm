module Exoext.Health exposing
    ( Health
    , Check
    , CheckId(..)
    , Status(..)
    , read
    , Overall(..)
    , overall
    , staleAfterMillis
    , isStale
    , strip
    , placeholder
    )

{-| The extension-system health surface: the `exoext.v1.health.*` half of the wire, and the
host chrome that renders it.

Everything here is **system chrome**. It is drawn by Exosphere from keys Exosphere reads off the
publishing server's own Nova metadata, it never routes through the json-render catalog, and it is
deliberately styled apart from the rendered manifest (which carries the provenance marker instead).
The one piece of publisher-authored text it shows — a check's `.detail` string — is rendered
verbatim in the monospace code idiom, where it reads as a quoted machine message rather than as
Exosphere speaking.

Why it exists: before it, an extension that had not yet published a manifest was indistinguishable
from one that never would. A VM whose object-store check failed at boot wrote `objectstore=fail`
within three minutes and the instance page still showed nothing for hours, because the only thing
the page looked at was the manifest. These keys are written from the first boot stage onward, so
they can carry the whole "detected → starting → healthy / degraded / failed" story on their own.


# The wire

@docs Health
@docs Check
@docs CheckId
@docs Status
@docs read


# Derived state

@docs Overall
@docs overall
@docs staleAfterMillis
@docs isStale


# Chrome

@docs strip
@docs placeholder

-}

import Color
import DateFormat.Relative
import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Helpers.Time
import Html.Attributes
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Style.Types exposing (ExoPalette, UIStateColors)
import Style.Widgets.Code as Code
import Style.Widgets.Icon as Icon
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Spinner as Spinner
import Style.Widgets.Text as Text
import Time



-- THE WIRE


{-| The boot / liveness checks a publisher reports, in boot order. The host fixes the vocabulary: a
publisher reports on these eight or on none, so the chrome can always say which of a known set is
missing rather than rendering whatever names a VM happens to invent.

The set is HOST-owned on purpose — it is a trust boundary. An extension sets values and detail text;
it never names a check, because a name the host has never seen is a name the host cannot render an
opinion about. Growing the set is forward-compatible in both directions: an unknown name is ignored
by an older host, and a check no publisher writes simply reads as pending.

-}
type CheckId
    = ObjectStore
    | WebPorts
    | Fetch
    | Network
    | Tls
    | Stack
    | Bridge
    | Store


{-| A single check's reported state. `CheckPending` is the absent / unrecognized value: the
publisher has not reported this stage yet, which during boot is the normal case and is what makes
the boot checklist a progress display rather than a failure display.
-}
type Status
    = CheckOk
    | CheckWarn
    | CheckFail
    | CheckPending


{-| One check: its identity, its reported [`Status`](#Status), and the publisher's free-text
`.detail` (≤255 chars per the wire; capped here regardless, since the cap is the publisher's
obligation and not the host's assumption).
-}
type alias Check =
    { id : CheckId
    , status : Status
    , detail : Maybe String
    }


{-| A publishing server's health as of its last write: when it last spoke (`seq`: the freshest of
the `exoext.v1.health.seq` unix epoch and the `exoext.v1.published` envelope stamp), every check
in boot order, and an optional publisher version string.
-}
type alias Health =
    { seq : Maybe Time.Posix
    , checks : List Check
    , version : Maybe String
    }


{-| Read the health keys from a server's Nova metadata. `Nothing` when the server has said nothing
this module recognizes, in which case no chrome is owed and — because this read is also a discovery
signal — no extension card is opened either.

**What counts as a health signal.** A parseable `seq`, or at least one of the known checks
carrying a value this module recognizes (`ok` / `warn` / `fail`). Deliberately NOT "any key under
the prefix": an orphan `<check>.detail`, a misspelled check name, or a key from some future
revision of the contract would otherwise be enough to open extension chrome on an instance that
publishes no extension. The gate has to be the part of the wire this build actually speaks, because
that is the only part it can render — and a card that appears on the strength of a typo is worse
than one that waits for a publisher to say something intelligible.

Note this is deliberately independent of the `exoext.v1.kind` discovery sentinel: an extension can
fail before it ever writes a sentinel, and reporting that failure is the whole point.

-}
read : List OSTypes.MetadataItem -> Maybe Health
read metadata =
    let
        dict =
            toDict metadata

        checks =
            List.map (readCheck dict) checkIds

        healthSeq =
            Dict.get (healthPrefix ++ "seq") dict
                |> Maybe.andThen String.toInt
                |> Maybe.map (\epochSeconds -> Time.millisToPosix (epochSeconds * 1000))

        -- The publisher rewrites its manifest envelope on a timer (`exoext.v1.published`, an
        -- ISO 8601 stamp) but stamps `health.seq` only when a check changes, so between events a
        -- healthy, quiet publisher goes silent on the health keys for hours. "When it last spoke"
        -- is therefore the freshest of the two writes the host already reads — no extra
        -- heartbeat, no extra polling.
        published =
            Dict.get "exoext.v1.published" dict
                |> Maybe.andThen (Helpers.Time.iso8601StringToPosix >> Result.toMaybe)

        seq =
            case ( healthSeq, published ) of
                ( Just a, Just b ) ->
                    Just (Time.millisToPosix (max (Time.posixToMillis a) (Time.posixToMillis b)))

                ( a, b ) ->
                    if a == Nothing then b else a

        reported =
            List.any (\check -> check.status /= CheckPending) checks
    in
    if seq /= Nothing || reported then
        Just
            { seq = seq
            , checks = checks
            , version = Dict.get versionKey dict |> Maybe.andThen nonEmpty
            }

    else
        Nothing


healthPrefix : String
healthPrefix =
    "exoext.v1.health."


{-| The publisher's own version, rendered if present and silently absent otherwise. It is not part
of the health contract (no publisher writes it yet), so the chrome must not depend on it.
-}
versionKey : String
versionKey =
    "exoext.v1.version"


readCheck : Dict String String -> CheckId -> Check
readCheck dict id =
    { id = id
    , status =
        Dict.get (healthPrefix ++ wireName id) dict
            |> Maybe.map statusFromString
            |> Maybe.withDefault CheckPending
    , detail =
        Dict.get (healthPrefix ++ wireName id ++ ".detail") dict
            |> Maybe.map normalizeDetail
            |> Maybe.andThen nonEmpty
    }


{-| Sanitize a publisher's `.detail` before it is allowed anywhere near the screen.

This is the one piece of publisher-authored text that Exosphere renders inside its OWN trust
chrome — the error box a researcher reads when deciding whether their machine is in trouble — so it
gets treated as hostile input rather than as a message.

  - **C0 / C1 control characters and DEL** become spaces. They are invisible, they let a string
    misreport its own length, and `\\r` in particular can make a line overwrite what precedes it in
    contexts that honor it.
  - **Unicode bidi controls and zero-width formatting characters** are removed outright
    (U+061C, U+200B–U+200F, U+202A–U+202E, U+2066–U+2069, U+FEFF). An unterminated RTL override is
    the classic trick for making a string render as something other than what it says; the detail
    element additionally pins its own direction (see [`detailLines`](#detailLines)), so this is the
    inner half of a two-part defense.
  - **Whitespace runs collapse to one space** and the result is trimmed, so a newline-padded string
    cannot claim vertical space the layout did not budget for.
  - **The 255-char cap is applied AFTER all of that**, which is the only order that bounds what is
    actually drawn: capping first lets 255 characters of stripped padding become a much shorter
    message, and worse, lets a long run of control characters push the real content past the cap.

-}
normalizeDetail : String -> String
normalizeDetail raw =
    raw
        |> String.toList
        |> List.filterMap
            (\char ->
                let
                    code =
                        Char.toCode char
                in
                if isFormattingChar code then
                    Nothing

                else if code < 0x20 || (code >= 0x7F && code <= 0x9F) then
                    Just ' '

                else
                    Just char
            )
        |> String.fromList
        |> String.words
        |> String.join " "
        |> String.left 255


isFormattingChar : Int -> Bool
isFormattingChar code =
    (code == 0x061C)
        || (code >= 0x200B && code <= 0x200F)
        || (code >= 0x202A && code <= 0x202E)
        || (code >= 0x2066 && code <= 0x2069)
        || (code == 0xFEFF)


statusFromString : String -> Status
statusFromString raw =
    case raw of
        "ok" ->
            CheckOk

        "warn" ->
            CheckWarn

        "fail" ->
            CheckFail

        _ ->
            CheckPending


{-| The checks in boot order. Order is meaningful: the checklist reads top to bottom as the VM's
startup sequence, so the row that stops moving is the one that later fails.
-}
checkIds : List CheckId
checkIds =
    [ ObjectStore, WebPorts, Fetch, Network, Tls, Stack, Bridge, Store ]


wireName : CheckId -> String
wireName id =
    case id of
        ObjectStore ->
            "objectstore"

        WebPorts ->
            "webports"

        Fetch ->
            "fetch"

        Network ->
            "network"

        Tls ->
            "tls"

        Stack ->
            "stack"

        Bridge ->
            "bridge"

        Store ->
            "store"


{-| The one-word chip label. Deliberately not the wire name: the wire names the mechanism
(`objectstore`, `stack`), the chip names what the researcher loses if it is red.
-}
chipLabel : CheckId -> String
chipLabel id =
    case id of
        ObjectStore ->
            "storage"

        WebPorts ->
            "ports"

        Fetch ->
            "bundle"

        Network ->
            "network"

        Tls ->
            "TLS"

        Stack ->
            "services"

        Bridge ->
            "bridge"

        Store ->
            "results"


{-| The checklist's stage sentence, one per boot stage.
-}
stageLabel : CheckId -> String
stageLabel id =
    case id of
        ObjectStore ->
            "Object storage reachable"

        WebPorts ->
            "Web ports open"

        Fetch ->
            "Software downloaded"

        Network ->
            "Public address assigned"

        Tls ->
            "TLS certificate issued"

        Stack ->
            "Services coming up"

        Bridge ->
            "Bridge connected"

        Store ->
            "Results store ready"



-- DERIVED STATE


{-| The whole publisher's state in one token, worst-first:

  - `SomeFailing` — at least one check reports `fail`. Something is broken and named.
  - `SomeDegraded` — no failures, at least one `warn`. Reduced, not broken.
  - `StillStarting` — no failures or warnings, but at least one check has not reported.
  - `AllHealthy` — every check reported `ok`.

-}
type Overall
    = AllHealthy
    | SomeDegraded
    | SomeFailing
    | StillStarting


{-| Reduce a [`Health`](#Health) to its [`Overall`](#Overall) token.
-}
overall : Health -> Overall
overall health =
    if List.any (\check -> check.status == CheckFail) health.checks then
        SomeFailing

    else if List.any (\check -> check.status == CheckWarn) health.checks then
        SomeDegraded

    else if List.any (\check -> check.status == CheckPending) health.checks then
        StillStarting

    else
        AllHealthy


{-| How long a publisher may go without writing anything the host dates it by (`health.seq` or
the `exoext.v1.published` envelope stamp) before the host stops believing what it last said: ten
minutes.

The publisher refreshes its envelope on a timer (every five minutes for the reference bridge), so
silence is itself a signal — the VM may be off, wedged,
or out of credentials, and in every one of those cases the last values are a snapshot of a moment
that has passed. Ten minutes is comfortably longer than any sane health interval (so a slow or
briefly-throttled publisher is never called stale) and short enough that a VM that died mid-morning
is not still presenting its last green strip in the afternoon.

Staleness is a modifier, not a state: it dims whatever is on screen and rewrites the strip's
sentence, and it composes over healthy, degraded, failed and still-starting alike.

-}
staleAfterMillis : Int
staleAfterMillis =
    10 * 60 * 1000


{-| How far ahead of the browser's clock a publisher's `seq` may sit before the host stops treating
it as a timestamp at all: two minutes.

The two clocks are independent and neither is authoritative, so a small forward skew is ordinary and
must not be punished — inside this window the record is simply "as of now". Past it, the value is
not a slow clock, it is a timestamp that could not have been written yet, and the host declines to
date the record from it.

-}
clockSkewAllowanceMillis : Int
clockSkewAllowanceMillis =
    2 * 60 * 1000


{-| How old the publisher's last health write is, in millis, or `Nothing` when the record cannot be
dated at all — no `seq`, an unparseable one, or one implausibly far in the future (see
[`clockSkewAllowanceMillis`](#clockSkewAllowanceMillis)). A forward skew inside the allowance clamps
to zero rather than going negative, so a slightly-fast publisher reads as "just now" instead of
reporting from the future.
-}
ageMillis : Time.Posix -> Health -> Maybe Int
ageMillis now health =
    health.seq
        |> Maybe.andThen
            (\seq ->
                let
                    age =
                        Time.posixToMillis now - Time.posixToMillis seq
                in
                if age < negate clockSkewAllowanceMillis then
                    Nothing

                else
                    Just (max 0 age)
            )


{-| Whether the publisher's last health write is older than [`staleAfterMillis`](#staleAfterMillis)
— **or cannot be dated at all**, which counts the same way.

An undatable record fails CLOSED. The alternative reading, "absent evidence of when it spoke is not
evidence that it stopped", is true and beside the point: the question this answers is not whether
the publisher is alive, it is whether the values on screen may be presented as current. A record
with no timestamp offers no grounds for saying they are, and a publisher that cannot produce a
plausible clock reading is the last one whose green checks should be shown undimmed. The cost of
being wrong here is asymmetric — a live extension is dimmed and labelled undated for one poll, versus
a dead one presenting a confident healthy strip indefinitely.

-}
isStale : Time.Posix -> Health -> Bool
isStale now health =
    case ageMillis now health of
        Just age ->
            age > staleAfterMillis

        Nothing ->
            True



-- CHROME


{-| The health strip attached under a rendered extension card (§ the running / degraded / stale
states).

Collapsed it is one quiet line — a status dot, one plain sentence, and the freshness right-aligned
— because a healthy extension should read as furniture on a page that may host several of them.
It expands to the per-check chips exactly when a check is not `ok`, which is the moment
"which one" becomes the question. That is one component with two forms, not two components:
naming the failing check is the entire added value, and on a healthy card it is a row of things to read
for no information.

-}
strip : ExoPalette -> Time.Posix -> Health -> Element.Element msg
strip palette now health =
    let
        stale =
            isStale now health
    in
    -- Framed like the provenance marker above it, because it is the same kind of thing: a
    -- host-drawn box about the extension rather than a piece of it.
    Element.column
        [ Element.width Element.fill
        , Border.width 1
        , Border.color (SH.toElementColor palette.neutral.border)
        , Border.rounded 4
        , Element.clip
        ]
        [ stripLine palette [] now health (stripSentence now health) (freshnessLabel "checked" now health)
        , if stale || overall health == AllHealthy then
            Element.none

          else
            chipRow palette
                [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                , Border.color (SH.toElementColor palette.neutral.border)
                ]
                health
        ]


{-| The chrome for a publisher that has NOT produced a renderable manifest, rendered purely from
the health keys — which is what makes it work at the one moment nothing else does.

Three forms, chosen by what the checks say:

  - **Starting** — a live checklist of the boot stages, each row done / current / pending as the
    keys flip. The stages come free from the same keys, they turn a blank wait into visible
    progress, and the row that stops moving is the one that later fails.
  - **Checks done, interface not here yet** — the same checklist, fully ticked, under a headline
    that says what is actually being waited on. Every check passing while nothing renders is a
    REAL state (the manifest fetch is in flight, or the publisher wrote health before it wrote its
    UI), and it is the one the generic wording gets wrong in both directions: "Extension starting…"
    contradicts a row of green ticks, and the strip's "Extension healthy" contradicts an empty card. So
    it gets its own headline and its own sentence, both saying the same thing.
  - **Failed** — the standardized error piece: which checks failed as chips, each `.detail` string
    verbatim in monospace, and one neutral sentence. No advice, because the host does not know the
    publisher's internals; no blame on the researcher's own server, because nothing on it changed.

`sourceNoun` is the deployer's own word for a server (`localization.virtualComputer`), for the same
reason the provenance marker uses it: this sentence is Exosphere speaking in its own voice.

-}
placeholder : ExoPalette -> String -> Time.Posix -> Health -> Element.Element msg
placeholder palette sourceNoun now health =
    let
        stale =
            isStale now health

        dim =
            if stale then
                [ Element.alpha 0.62 ]

            else
                []

        booting title =
            [ Element.row [ Element.spacing spacer.px8 ]
                [ Spinner.sized 14 palette
                , headline palette Nothing title
                ]
            , checklist palette health
            ]

        -- A settled failure is a real card; a boot still in progress is deliberately provisional,
        -- so it takes the dashed "something is coming" border instead. `sentence` overrides the
        -- strip's default wording where the default would describe the extension rather than what
        -- this card is actually waiting for.
        { body, verb, borderStyle, sentence } =
            case overall health of
                SomeFailing ->
                    { body =
                        [ headline palette (Just palette.danger.textOnNeutralBG) "Extension failed to start"
                        , chipRow palette [ Element.paddingXY 0 0 ] health
                        , detailLines palette health
                        , Element.paragraph
                            [ Text.fontSize Text.Small
                            , Font.color (SH.toElementColor palette.neutral.text.subdued)
                            ]
                            [ Element.text ("The extension's " ++ sourceNoun ++ " reported an error during startup.") ]
                        ]
                    , verb = "reported"
                    , borderStyle = Border.solid
                    , sentence = Nothing
                    }

                AllHealthy ->
                    { body = booting "Extension interface loading…"
                    , verb = "checked"
                    , borderStyle = Border.dashed
                    , sentence = Just "All checks passed — waiting for the extension's interface"
                    }

                _ ->
                    { body = booting "Extension starting…"
                    , verb = "updated"
                    , borderStyle = Border.dashed
                    , sentence = Nothing
                    }
    in
    Element.column
        [ Element.width Element.fill
        , Background.color (SH.toElementColor palette.neutral.background.frontLayer)
        , Border.width 1
        , borderStyle
        , Border.color (SH.toElementColor palette.neutral.border)
        , Border.rounded 4
        , Element.clip
        ]
        [ Element.column
            ([ Element.width Element.fill
             , Element.spacing spacer.px8
             , Element.padding spacer.px12
             ]
                ++ dim
            )
            body
        , stripLine palette
            [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
            , Border.color (SH.toElementColor palette.neutral.border)
            ]
            now
            health
            -- Staleness always wins the sentence: an undated or long-silent record must not be
            -- described by what its checks last claimed, whatever this form would otherwise say.
            (if isStale now health then
                stripSentence now health

             else
                sentence |> Maybe.withDefault (stripSentence now health)
            )
            (freshnessLabel verb now health)
        ]


{-| The strip's own line: a status dot (or a clock, when stale), the sentence, and the freshness.
Tinted by the palette's state family so a degraded or failed strip reads at a glance, and left flat
on the front layer when everything is fine.
-}
stripLine : ExoPalette -> List (Element.Attribute msg) -> Time.Posix -> Health -> String -> Maybe String -> Element.Element msg
stripLine palette frameAttrs now health sentence freshness =
    let
        stale =
            isStale now health

        stateColors =
            if stale then
                Just palette.muted

            else
                case overall health of
                    SomeFailing ->
                        Just palette.danger

                    SomeDegraded ->
                        Just palette.warning

                    _ ->
                        Nothing

        ( background, foreground ) =
            case stateColors of
                Just colors ->
                    ( colors.background, colors.textOnColoredBG )

                Nothing ->
                    ( palette.neutral.background.frontLayer, palette.neutral.text.subdued )
    in
    Element.row
        ([ Element.width Element.fill
         , Element.spacing spacer.px8
         , Element.paddingXY spacer.px12 spacer.px8
         , Background.color (SH.toElementColor background)
         , Font.color (SH.toElementColor foreground)
         , Text.fontSize Text.Small
         ]
            ++ frameAttrs
        )
        [ if stale then
            Icon.sizedFeatherIcon 14 FeatherIcons.clock

          else
            statusDot palette (worstStatus health)
        , Element.paragraph [] [ Element.text sentence ]
        , case freshness of
            Just label ->
                Element.el [ Element.alignRight ] (Element.text label)

            Nothing ->
                Element.none
        ]


{-| The one plain sentence the collapsed strip carries. Staleness rewrites it outright: nothing the
publisher last said can be trusted as current, so the only honest headline is when it last spoke.
-}
stripSentence : Time.Posix -> Health -> String
stripSentence now health =
    let
        total =
            List.length health.checks

        counted status =
            health.checks |> List.filter (\check -> check.status == status) |> List.length

        ofTotal n =
            String.fromInt n ++ " of " ++ String.fromInt total ++ " checks "
    in
    if isStale now health then
        case ( ageMillis now health, health.seq ) of
            ( Just _, Just seq ) ->
                "Last heard from this extension " ++ DateFormat.Relative.relativeTime now seq ++ " — values may be out of date"

            _ ->
                -- Undatable: no seq, an unparseable one, or one from the future. The record cannot
                -- be placed in time at all, which is a different claim from "it has been a while".
                "This extension did not report when it last checked — values may be out of date"

    else
        case overall health of
            AllHealthy ->
                "Extension healthy"

            SomeDegraded ->
                if degradedTransportOnly health then
                    "Limited storage — results are using the fallback transport"

                else
                    ofTotal (counted CheckWarn) ++ "degraded"

            SomeFailing ->
                ofTotal (counted CheckFail) ++ "failing"

            StillStarting ->
                ofTotal (counted CheckOk) ++ "passed"


{-| Whether the only thing not `ok` is the results store, which is the wire's way of saying results
are coming through the fallback transport. Worth its own sentence because it is the one degraded
state that is not a malfunction — saying "1 of 6 checks degraded" there would imply something broke.
-}
degradedTransportOnly : Health -> Bool
degradedTransportOnly health =
    health.checks
        |> List.all
            (\check ->
                if check.id == Store then
                    check.status /= CheckFail

                else
                    check.status == CheckOk
            )


{-| The worst status present, which is what the strip's dot shows.
-}
worstStatus : Health -> Status
worstStatus health =
    case overall health of
        SomeFailing ->
            CheckFail

        SomeDegraded ->
            CheckWarn

        StillStarting ->
            CheckPending

        AllHealthy ->
            CheckOk


{-| `checked 40 seconds ago` / `reported 2 minutes ago`. Nothing at all while stale, where the
sentence itself carries the age — and an undatable record is always stale, so this never has to
invent a time it was not given.

It is rendered from the AGE rather than from `seq` directly, which is what keeps a publisher whose
clock runs slightly fast reading as "just now" instead of reporting from the future.

-}
freshnessLabel : String -> Time.Posix -> Health -> Maybe String
freshnessLabel verb now health =
    if isStale now health then
        Nothing

    else
        ageMillis now health
            |> Maybe.map
                (\age ->
                    verb
                        ++ " "
                        ++ DateFormat.Relative.relativeTime now (Time.millisToPosix (Time.posixToMillis now - age))
                )



-- PIECES


{-| The per-check chips, plus the publisher version at the right end when it published one.
An `ok` chip is muted on purpose: the row exists to make the non-`ok` ones findable.
-}
chipRow : ExoPalette -> List (Element.Attribute msg) -> Health -> Element.Element msg
chipRow palette frameAttrs health =
    Element.row
        ([ Element.width Element.fill
         , Element.paddingXY spacer.px12 spacer.px8
         , Element.spacing spacer.px8
         ]
            ++ frameAttrs
        )
        [ Element.wrappedRow
            [ Element.width Element.fill, Element.spacing spacer.px4 ]
            (List.map (checkChip palette) health.checks)
        , case health.version of
            Just version ->
                Element.el
                    [ Element.alignRight
                    , Text.fontSize Text.Tiny
                    , Font.color (SH.toElementColor palette.neutral.text.subdued)
                    ]
                    (Element.text version)

            Nothing ->
                Element.none
        ]


checkChip : ExoPalette -> Check -> Element.Element msg
checkChip palette check =
    let
        ( borderColor, fontColor ) =
            case check.status of
                CheckOk ->
                    ( palette.neutral.border, palette.neutral.text.subdued )

                CheckPending ->
                    ( palette.neutral.border, palette.neutral.text.subdued )

                _ ->
                    ( (tone palette check.status).border, (tone palette check.status).textOnColoredBG )

        backgroundColor =
            case check.status of
                CheckWarn ->
                    Just palette.warning.background

                CheckFail ->
                    Just palette.danger.background

                _ ->
                    Nothing
    in
    Element.row
        ([ Element.spacing spacer.px4
         , Element.paddingXY spacer.px4 spacer.px4
         , Border.width 1
         , Border.rounded 3
         , Border.color (SH.toElementColor borderColor)
         , Font.color (SH.toElementColor fontColor)
         , Text.fontSize Text.Tiny
         ]
            ++ (case backgroundColor of
                    Just color ->
                        [ Background.color (SH.toElementColor color) ]

                    Nothing ->
                        []
               )
            ++ (case check.status of
                    CheckPending ->
                        [ Border.dashed, Element.alpha 0.7 ]

                    _ ->
                        []
               )
        )
        [ statusDot palette check.status
        , Element.text (chipLabel check.id)
        ]


{-| The failure piece's body: one monospace line per check that has something to say, `<wire name>:
<detail>` with the publisher's string verbatim. Height-bounded and scrollable, so a chatty
publisher cannot push the rest of the page away.
-}
detailLines : ExoPalette -> Health -> Element.Element msg
detailLines palette health =
    let
        lines =
            health.checks
                |> List.filter (\check -> check.status == CheckFail || check.status == CheckWarn)
                |> List.filterMap
                    (\check -> check.detail |> Maybe.map (\detail -> ( check, detail )))
    in
    if List.isEmpty lines then
        Element.none

    else
        Element.column
            [ Element.width Element.fill
            , Element.height (Element.shrink |> Element.maximum 150)
            , Element.scrollbarY
            , Element.spacing spacer.px4
            ]
            (lines
                |> List.map
                    (\( check, detail ) ->
                        Element.el
                            ([ Element.width Element.fill
                             , Element.clip
                             , Element.paddingXY spacer.px8 spacer.px4

                             -- The outer half of the bidi defense (`normalizeDetail` is the inner
                             -- half, which strips the control characters themselves). An explicit
                             -- LTR direction plus a bidi ISOLATE means the strongly-RTL runs a
                             -- detail may legitimately contain are resolved entirely inside this
                             -- box: they can reorder their own line and nothing else, so the
                             -- surrounding chrome's direction cannot be flipped by its content.
                             , Element.htmlAttribute (Html.Attributes.dir "ltr")
                             , Element.htmlAttribute (Html.Attributes.style "unicode-bidi" "isolate")
                             ]
                                ++ Code.codeAttrs palette
                            )
                            (Element.paragraph
                                [ Text.fontSize Text.Small

                                -- A publisher's detail can be a single unbroken 255-character run
                                -- (a URL, a base64 blob, a stack frame). A paragraph only breaks at
                                -- whitespace, so without this the line escapes its bounded box
                                -- sideways instead of wrapping inside it.
                                , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "anywhere")
                                , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                                ]
                                [ Element.text (wireName check.id ++ ": " ++ detail) ]
                            )
                    )
            )


{-| The live boot checklist: every stage in order, done above, the current one spinning, the rest
waiting. "Current" is the first stage that has not reported `ok`, which is exactly where the boot
sequence is.
-}
checklist : ExoPalette -> Health -> Element.Element msg
checklist palette health =
    let
        currentIndex =
            health.checks
                |> List.indexedMap (\index check -> ( index, check ))
                |> List.filter (\( _, check ) -> check.status /= CheckOk)
                |> List.head
                |> Maybe.map Tuple.first
    in
    Element.column
        [ Element.width Element.fill, Element.spacing spacer.px4 ]
        (health.checks
            |> List.indexedMap
                (\index check ->
                    checklistRow palette (Just index == currentIndex) check
                )
        )


checklistRow : ExoPalette -> Bool -> Check -> Element.Element msg
checklistRow palette current check =
    let
        mark =
            case ( check.status, current ) of
                ( CheckOk, _ ) ->
                    Element.el
                        [ Font.color (SH.toElementColor palette.success.textOnNeutralBG) ]
                        (Icon.sizedFeatherIcon 14 FeatherIcons.check)

                ( CheckWarn, _ ) ->
                    statusDot palette CheckWarn

                ( _, True ) ->
                    Spinner.sized 14 palette

                _ ->
                    statusDot palette CheckPending

        waiting =
            check.status /= CheckOk && check.status /= CheckWarn && not current
    in
    Element.row
        ([ Element.width Element.fill
         , Element.spacing spacer.px8
         , Text.fontSize Text.Small
         ]
            ++ (if waiting then
                    [ Element.alpha 0.6 ]

                else
                    []
               )
        )
        [ Element.el [ Element.width (Element.px 14) ] mark
        , Element.paragraph [] [ Element.text (stageLabel check.id) ]
        ]


headline : ExoPalette -> Maybe Color.Color -> String -> Element.Element msg
headline palette color label =
    Element.paragraph
        [ Font.bold
        , Font.color
            (SH.toElementColor (color |> Maybe.withDefault palette.neutral.text.default))
        ]
        [ Text.body label ]


{-| The 8px status dot the strip, the chips and the checklist all share. `flex: none` keeps it
round: it is a flex item of its row, and a circle compressed on one axis is an oval.
-}
statusDot : ExoPalette -> Status -> Element.Element msg
statusDot palette status =
    Element.el
        [ Element.width (Element.px 8)
        , Element.height (Element.px 8)
        , Element.centerY
        , Border.rounded 999
        , Background.color (SH.toElementColor (tone palette status).default)
        , Element.htmlAttribute (Html.Attributes.style "flex" "none")
        ]
        Element.none


tone : ExoPalette -> Status -> UIStateColors
tone palette status =
    case status of
        CheckOk ->
            palette.success

        CheckWarn ->
            palette.warning

        CheckFail ->
            palette.danger

        CheckPending ->
            palette.muted



-- HELPERS


nonEmpty : String -> Maybe String
nonEmpty raw =
    if String.isEmpty (String.trim raw) then
        Nothing

    else
        Just (String.trim raw)


toDict : List OSTypes.MetadataItem -> Dict String String
toDict metadata =
    metadata |> List.map (\item -> ( item.key, item.value )) |> Dict.fromList
