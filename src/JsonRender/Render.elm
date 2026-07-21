module JsonRender.Render exposing
    ( Model, init
    , Msg, update
    , Effect(..)
    , view
    )

{-| Render a validated [`Spec`](JsonRender-Spec#Spec) to `Html`, driven by host-owned
state, with actions flowing back out as [`Effect`](#Effect) values.

This is a small TEA component. The host owns the json-render `state` and passes it to
[`view`](#view) on every frame; the renderer reads it but never mutates it behind the
host's back. The only local state the renderer owns is the pending confirm dialog. User
actions (button presses, checkbox toggles) surface to the host as `Effect`s the host
applies — exactly mirroring the framework-neutral host↔renderer contract.

Because the output is Elm `Html` (no `innerHTML`, no script escape hatch), the rendered
tree is **XSS-safe by construction**.


# State

@docs Model, init


# Messages

@docs Msg, update


# Effects (out to the host)

@docs Effect


# View

@docs view

-}

import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import JsonRender.Expr as Expr exposing (Context)
import JsonRender.Spec as Spec
    exposing
        ( ActionBinding
        , Confirm
        , Props(..)
        , Repeat
        , Spec
        , UIElement
        )
import Url


{-| Renderer-local UI state. Holds only the pending confirm dialog (if any); the
host owns everything else.
-}
type Model
    = Model { pendingConfirm : Maybe Emit }


{-| The initial (empty) renderer state: no dialog open.
-}
init : Model
init =
    Model { pendingConfirm = Nothing }


{-| A fully-resolved intent to emit an action. Params are resolved against the press-time
context (so per-row scope is captured); confirm strings are resolved too.
-}
type alias Emit =
    { verb : String
    , params : Value
    , confirm : Maybe ResolvedConfirm
    }


type alias ResolvedConfirm =
    { title : String
    , message : String
    , confirmLabel : String
    , cancelLabel : String
    , variant : String
    }


{-| Renderer messages. Opaque to the host: the host maps `Html Msg` into its own message
type and feeds `Msg`s back through [`update`](#update), acting on the returned
[`Effect`](#Effect).
-}
type Msg
    = Pressed Emit
    | ConfirmAccepted
    | ConfirmDismissed
    | Toggled String Bool


{-| What the renderer asks the host to do. The renderer never performs the side effect
itself.

  - `EmitAction { verb, params }` — a wired `on:press` fired (and any `confirm` was
    accepted). `params` are already expression-resolved. The host re-checks the verb
    against its allowlist and performs the side effect.
  - `EmitStateChange { path, value }` — a two-way input (`$bindState` / `$bindItem`
    checkbox) was toggled. The host treats this as the source of truth, writes it at the
    given absolute JSON Pointer, and re-projects state.

-}
type Effect
    = EmitAction { verb : String, params : Value }
    | EmitStateChange { path : String, value : Value }


{-| Advance renderer-local state in response to a `Msg`, optionally yielding an `Effect`
for the host to apply.

`confirm` is honored here: a press carrying a confirm opens the dialog and emits nothing;
the `EmitAction` only fires once the user accepts.

-}
update : Msg -> Model -> ( Model, Maybe Effect )
update msg (Model model) =
    case msg of
        Pressed emit ->
            case emit.confirm of
                Just _ ->
                    ( Model { model | pendingConfirm = Just emit }, Nothing )

                Nothing ->
                    ( Model model, Just (emitAction emit) )

        ConfirmAccepted ->
            case model.pendingConfirm of
                Just emit ->
                    ( Model { model | pendingConfirm = Nothing }, Just (emitAction emit) )

                Nothing ->
                    ( Model model, Nothing )

        ConfirmDismissed ->
            ( Model { model | pendingConfirm = Nothing }, Nothing )

        Toggled path value ->
            ( Model model, Just (EmitStateChange { path = path, value = Encode.bool value }) )


emitAction : Emit -> Effect
emitAction emit =
    EmitAction { verb = emit.verb, params = emit.params }



-- VIEW


{-| Render the spec against the current host-owned `state`. The returned `Html Msg`
includes the confirm dialog overlay when one is pending.

`allowedOrigins` is the host-provided iframe origin allowlist: an `Iframe` element renders
only when its resolved `src` is an https URL whose origin is an exact member of this list.
An empty list disables all iframes (fail-closed).

-}
view : List String -> Spec -> Value -> Model -> Html Msg
view allowedOrigins spec state (Model model) =
    Html.div [ Attr.class "jr-root" ]
        [ renderElement spec (Expr.rootContext allowedOrigins state) spec.root
        , confirmOverlay model.pendingConfirm
        ]


renderElement : Spec -> Context -> String -> Html Msg
renderElement spec ctx id =
    case Dict.get id spec.elements of
        Just element ->
            renderUIElement spec ctx element

        Nothing ->
            -- Cannot happen for a decoded spec (child refs are validated); fail-closed stub.
            Html.div [ Attr.class "jr-error" ]
                [ Html.text ("Missing element: " ++ id) ]


renderUIElement : Spec -> Context -> UIElement -> Html Msg
renderUIElement spec ctx element =
    let
        childrenHtml =
            case element.repeat of
                Just repeat ->
                    repeatChildren spec ctx element repeat

                Nothing ->
                    List.map (renderElement spec ctx) element.children
    in
    renderComponent ctx element childrenHtml


repeatChildren : Spec -> Context -> UIElement -> Repeat -> List (Html Msg)
repeatChildren spec ctx element repeat =
    let
        items =
            arrayAt repeat.statePath ctx.state

        renderRow index item =
            let
                rowCtx =
                    Expr.childContext repeat.statePath index item ctx
            in
            List.map (renderElement spec rowCtx) element.children
    in
    List.concat (List.indexedMap renderRow items)


renderComponent : Context -> UIElement -> List (Html Msg) -> Html Msg
renderComponent ctx element childrenHtml =
    case element.props of
        CardP props ->
            renderCard ctx props childrenHtml

        StackP props ->
            renderStack props childrenHtml

        TextP props ->
            Html.span [ Attr.class "jr-text" ]
                [ Html.text (Expr.resolveDisplay ctx props.value) ]

        BadgeP props ->
            renderBadge ctx props

        ButtonP props ->
            renderButton ctx element props

        CheckboxP props ->
            renderCheckbox ctx props

        FindingsTableP props ->
            renderFindingsTable ctx props

        IframeP props ->
            renderIframe ctx props

        TableP props ->
            renderTable ctx props

        AlertP props ->
            renderAlert ctx props

        DisclosureP props ->
            renderDisclosure ctx props childrenHtml


renderCard : Context -> Spec.CardProps -> List (Html Msg) -> Html Msg
renderCard ctx props childrenHtml =
    let
        titleHtml =
            case props.title of
                Just expr ->
                    [ Html.h2 [ Attr.class "jr-card__title" ]
                        [ Html.text (Expr.resolveDisplay ctx expr) ]
                    ]

                Nothing ->
                    []
    in
    Html.div [ Attr.class "jr-card" ] (titleHtml ++ childrenHtml)


renderStack : Spec.StackProps -> List (Html Msg) -> Html Msg
renderStack props childrenHtml =
    let
        directionClass =
            case props.direction of
                Spec.Row ->
                    "jr-stack--row"

                Spec.Col ->
                    "jr-stack--col"
    in
    Html.div
        [ Attr.class ("jr-stack " ++ directionClass)
        , Attr.attribute "data-gap" (String.fromInt props.gap)
        ]
        childrenHtml


{-| Render a `Disclosure` as a native `<details>`: a `<summary>` carrying the resolved `label`,
followed by the children inside a `jr-disclosure__body` div. `Attr.attribute "open" ""` is added
only when `open == True` (boolean-attribute presence sets the initial expanded state). Repeat and
context semantics are unchanged — the children walk normally.
-}
renderDisclosure : Context -> Spec.DisclosureProps -> List (Html Msg) -> Html Msg
renderDisclosure ctx props childrenHtml =
    let
        openAttr =
            if props.open then
                [ Attr.attribute "open" "" ]

            else
                []
    in
    Html.details (Attr.class "jr-disclosure" :: openAttr)
        [ Html.summary [ Attr.class "jr-disclosure__summary" ]
            [ Html.text (Expr.resolveDisplay ctx props.label) ]
        , Html.div [ Attr.class "jr-disclosure__body" ] childrenHtml
        ]


renderBadge : Context -> Spec.BadgeProps -> Html Msg
renderBadge ctx props =
    let
        state =
            Expr.resolveDisplay ctx props.value
    in
    Html.span
        [ Attr.class ("jr-badge jr-badge--" ++ badgeTone state)
        , Attr.attribute "data-state" state
        ]
        [ Html.text state ]


{-| Tone mapping from a per-row `scanState` string. Keyed on the leading whitespace-delimited
token so a value that carries a trailing detail suffix (e.g. `"scanning · 0:15"`, the live-scan
row's counting-up elapsed) still maps to its state tone. Mirrors the Track A island's state→tone
table (`pinned-format-reference.md` §"STILL-UNCERTAIN" item 3).
-}
badgeTone : String -> String
badgeTone state =
    case badgeToken state of
        "idle" ->
            "neutral"

        "queued" ->
            "info"

        "running" ->
            "info"

        "scanning" ->
            "info"

        "done" ->
            "success"

        "error" ->
            "danger"

        _ ->
            "neutral"


{-| The leading token of a badge value: everything before the first space. So `"scanning · 0:15"`
tones as `"scanning"` while a plain `"done"` is unchanged.
-}
badgeToken : String -> String
badgeToken state =
    state |> String.split " " |> List.head |> Maybe.withDefault state


renderButton : Context -> UIElement -> Spec.ButtonProps -> Html Msg
renderButton ctx element props =
    let
        handler =
            case pressEmit ctx element of
                Just emit ->
                    [ Events.onClick (Pressed emit) ]

                Nothing ->
                    []
    in
    Html.button
        (Attr.class "jr-button" :: Attr.type_ "button" :: handler)
        [ Html.text (Expr.resolveDisplay ctx props.label) ]


pressEmit : Context -> UIElement -> Maybe Emit
pressEmit ctx element =
    Dict.get "press" element.on
        |> Maybe.andThen List.head
        |> Maybe.map (buildEmit ctx)


buildEmit : Context -> ActionBinding -> Emit
buildEmit ctx binding =
    { verb = binding.action
    , params = Expr.resolveParams ctx binding.params
    , confirm = Maybe.map (resolveConfirm ctx) binding.confirm
    }


resolveConfirm : Context -> Confirm -> ResolvedConfirm
resolveConfirm ctx confirm =
    { title = blankToDefault "Confirm action" (Expr.resolveDisplay ctx confirm.title)
    , message = blankToDefault "Are you sure you want to continue?" (Expr.resolveDisplay ctx confirm.message)
    , confirmLabel = Maybe.withDefault "Confirm" confirm.confirmLabel
    , cancelLabel = Maybe.withDefault "Cancel" confirm.cancelLabel
    , variant = confirm.variant
    }


{-| Fail-closed confirm text: a confirm expression that resolves to nothing (a missing
`$item`/`$state`, so `resolveDisplay` yields an empty string) falls back to a sensible
generic string rather than showing a blank dialog. Resolution never emits raw directive
JSON, so this only guards the empty case.
-}
blankToDefault : String -> String -> String
blankToDefault default resolved =
    if String.trim resolved == "" then
        default

    else
        resolved


renderCheckbox : Context -> Spec.CheckboxProps -> Html Msg
renderCheckbox ctx props =
    let
        isChecked =
            props.checked |> Maybe.map (Expr.resolveBool ctx) |> Maybe.withDefault False

        writeBack =
            props.checked |> Maybe.andThen (Expr.writeBackPath ctx)

        handler =
            case writeBack of
                Just path ->
                    [ Events.onCheck (Toggled path) ]

                Nothing ->
                    []

        labelHtml =
            case props.label of
                Just expr ->
                    [ Html.span [ Attr.class "jr-checkbox__label" ]
                        [ Html.text (Expr.resolveDisplay ctx expr) ]
                    ]

                Nothing ->
                    []
    in
    Html.label [ Attr.class "jr-checkbox" ]
        (Html.input
            (Attr.type_ "checkbox" :: Attr.checked isChecked :: handler)
            []
            :: labelHtml
        )


renderFindingsTable : Context -> Spec.FindingsTableProps -> Html Msg
renderFindingsTable ctx props =
    let
        groups =
            Expr.resolve ctx props.bind
                |> decodeFindings
                |> groupFindings props.groupBy
                |> orderBySeverity
    in
    case groups of
        [] ->
            Html.div [ Attr.class "jr-findings jr-findings--empty" ]
                [ Html.text "No findings yet" ]

        _ ->
            let
                total =
                    List.sum (List.map Tuple.second groups)
            in
            Html.div [ Attr.class "jr-findings" ]
                (Html.span [ Attr.class "jr-findings__total" ]
                    [ Html.text (String.fromInt total ++ pluralizeFindings total) ]
                    :: List.map renderFindingPill groups
                )


decodeFindings : Value -> List (Dict.Dict String Value)
decodeFindings value =
    if isNull value then
        []

    else
        Decode.decodeValue (Decode.list (Decode.dict Decode.value)) value
            |> Result.withDefault []


pluralizeFindings : Int -> String
pluralizeFindings n =
    if n == 1 then
        " finding"

    else
        " findings"


{-| One at-a-glance severity pill: a severity-colored dot, the count, then the label. The
`jr-findings__pill--<label>` modifier drives the dot color from the host stylesheet.
-}
renderFindingPill : ( String, Int ) -> Html Msg
renderFindingPill ( label, count ) =
    Html.span [ Attr.class ("jr-findings__pill jr-findings__pill--" ++ String.toLower label) ]
        [ Html.span [ Attr.class "jr-findings__dot" ] []
        , Html.span [ Attr.class "jr-findings__count" ] [ Html.text (String.fromInt count) ]
        , Html.span [ Attr.class "jr-findings__label" ] [ Html.text label ]
        ]


groupFindings : String -> List (Dict.Dict String Value) -> List ( String, Int )
groupFindings groupBy findings =
    findings
        |> List.foldr
            (\finding acc ->
                let
                    key =
                        Dict.get groupBy finding
                            |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                            |> Maybe.withDefault "ungrouped"
                in
                Dict.update key (Maybe.withDefault 0 >> (+) 1 >> Just) acc
            )
            Dict.empty
        |> Dict.toList


{-| Order the grouped counts by severity rank (critical, high, medium, low, info) rather than
alphabetically, dropping any zero counts. Unrecognized labels sort after the known severities,
by descending count then name, so a non-severity `groupBy` still renders sensibly.
-}
orderBySeverity : List ( String, Int ) -> List ( String, Int )
orderBySeverity =
    List.filter (\( _, count ) -> count > 0)
        >> List.sortWith compareSeverityGroup


compareSeverityGroup : ( String, Int ) -> ( String, Int ) -> Order
compareSeverityGroup ( labelA, countA ) ( labelB, countB ) =
    case ( severityRank labelA, severityRank labelB ) of
        ( Just rankA, Just rankB ) ->
            compare rankA rankB

        ( Just _, Nothing ) ->
            LT

        ( Nothing, Just _ ) ->
            GT

        ( Nothing, Nothing ) ->
            case compare countB countA of
                EQ ->
                    compare labelA labelB

                order ->
                    order


severityRank : String -> Maybe Int
severityRank label =
    case String.toLower label of
        "critical" ->
            Just 0

        "high" ->
            Just 1

        "medium" ->
            Just 2

        "low" ->
            Just 3

        "info" ->
            Just 4

        _ ->
            Nothing



-- TABLE


{-| Render a `Table`: a `<thead>` of column labels and one `<tr>` per bound row. Each cell
reads `row[column.key]` (a missing key renders empty). The `jr-table` / `jr-table__header`
/ `jr-table__row` / `jr-table__cell` classes drive host styling.
-}
renderTable : Context -> Spec.TableProps -> Html Msg
renderTable ctx props =
    let
        rows =
            Expr.resolve ctx props.bind |> decodeFindings
    in
    Html.table [ Attr.class "jr-table" ]
        [ Html.thead []
            [ Html.tr [ Attr.class "jr-table__row" ]
                (List.map headerCell props.columns)
            ]
        , Html.tbody []
            (List.map (bodyRow ctx props.columns) rows)
        ]


headerCell : Spec.Column -> Html Msg
headerCell column =
    Html.th [ Attr.class "jr-table__header" ] [ Html.text column.label ]


bodyRow : Context -> List Spec.Column -> Dict.Dict String Value -> Html Msg
bodyRow ctx columns row =
    Html.tr [ Attr.class "jr-table__row" ]
        (List.map (bodyCell ctx row) columns)


bodyCell : Context -> Dict.Dict String Value -> Spec.Column -> Html Msg
bodyCell ctx row column =
    Html.td [ Attr.class "jr-table__cell" ]
        [ Html.text (cellText ctx (Dict.get column.key row)) ]


{-| A cell's display string, reusing the shared literal-display semantics (so numbers /
booleans / null render exactly as they do for `Text`). A missing column key = "".
-}
cellText : Context -> Maybe Value -> String
cellText ctx maybeValue =
    maybeValue
        |> Maybe.map (Expr.ELiteral >> Expr.resolveDisplay ctx)
        |> Maybe.withDefault ""



-- ALERT


{-| Render an `Alert`: `div.jr-alert.jr-alert--<tone>` with an optional title span followed
by the message span. The `jr-alert--<tone>` modifier drives host styling.
-}
renderAlert : Context -> Spec.AlertProps -> Html Msg
renderAlert ctx props =
    let
        titleHtml =
            case props.title of
                Just expr ->
                    [ Html.span [ Attr.class "jr-alert__title" ]
                        [ Html.text (Expr.resolveDisplay ctx expr) ]
                    ]

                Nothing ->
                    []
    in
    Html.div [ Attr.class ("jr-alert jr-alert--" ++ toneClass props.tone) ]
        (titleHtml
            ++ [ Html.span [ Attr.class "jr-alert__message" ]
                    [ Html.text (Expr.resolveDisplay ctx props.message) ]
               ]
        )


toneClass : Spec.Tone -> String
toneClass tone =
    case tone of
        Spec.Info ->
            "info"

        Spec.Warning ->
            "warning"

        Spec.Danger ->
            "danger"



-- IFRAME (origin-pinned, fail-closed)


{-| Render an `Iframe`, origin-pinned. The resolved `src` is emitted as an `<iframe>` ONLY
when it is a well-formed https URL whose origin (scheme + host + port) is an exact member of
the host-provided `ctx.allowedOrigins`. An **empty / unresolved** `src` renders **nothing** —
the element self-hides so the host can own the empty/loading/error affordance. A **non-empty
but disallowed** src (non-https scheme, unparseable URL, or off-allowlist origin) renders a
benign security placeholder instead.

When it does render, the frame is always preceded by a `jr-iframe__provenance` bar naming
the embedded origin. The bar is emitted unconditionally by the renderer, with no prop to
suppress it, so a manifest cannot hide that the content is unverified third-party. The
`<iframe>` sits inside a `jr-iframe__frame` wrapper that carries a distinct border in the
host stylesheet.

-}
renderIframe : Context -> Spec.IframeProps -> Html Msg
renderIframe ctx props =
    let
        url =
            Expr.resolveDisplay ctx props.src
    in
    if String.isEmpty url then
        -- An empty / unresolved `src` renders NOTHING. This is not an error to surface: the
        -- host owns the empty / loading / error affordance around the frame, and a "content
        -- unavailable" placeholder here would fight (and duplicate) that host chrome. The
        -- security placeholder below is reserved for a *non-empty but disallowed* src, which is
        -- a genuine "someone tried to point the frame off-allowlist" signal worth showing.
        Html.text ""

    else if isAllowedIframeSrc ctx.allowedOrigins url then
        -- The provenance bar is keyed by a constant so it never remounts. The frame wrapper is
        -- keyed by the iframe's full src so a changed URL REMOUNTS it: the embed token lives in
        -- the URL fragment (#token=...), and browsers do not reload an iframe when only the
        -- fragment changes, so without a keyed remount a fresh token would keep showing the
        -- stale page.
        Keyed.node "div"
            [ Attr.class "jr-iframe", Attr.style "width" "100%" ]
            [ ( "provenance", provenanceBar url )
            , ( url
              , Html.div [ Attr.class "jr-iframe__frame" ]
                    [ iframeElement ctx props url ]
              )
            ]

    else
        Html.div [ Attr.class "jr-iframe--blocked" ]
            [ Html.text "Embedded content is unavailable." ]


{-| The always-on provenance chrome: a slim bar naming the embedded origin, rendered above
the frame. Not suppressible from a manifest. Host stylesheets may restyle `jr-iframe__provenance`
but the bar is structurally present whenever an iframe renders.
-}
provenanceBar : String -> Html Msg
provenanceBar url =
    Html.div [ Attr.class "jr-iframe__provenance" ]
        [ Html.text
            ("Third-party content from "
                ++ iframeOrigin url
                ++ " — not verified by Exosphere"
            )
        ]


iframeElement : Context -> Spec.IframeProps -> String -> Html Msg
iframeElement ctx props url =
    Html.iframe
        [ Attr.src url
        , Attr.title (Expr.resolveDisplay ctx props.title)

        -- `allow-same-origin` is safe here BECAUSE the origin-pin guarantees the src is
        -- cross-origin to the host (Exosphere): the sandbox token grants the embedded app
        -- access to its OWN origin only, never the parent host origin.
        , Attr.attribute "sandbox" "allow-scripts allow-same-origin allow-forms"
        , Attr.attribute "referrerpolicy" "no-referrer"

        -- Force the embedded (cross-origin) app to render in light mode regardless
        -- of the viewer's OS `prefers-color-scheme`. Per the CSS Color Adjustment
        -- spec, Chromium derives the embedded page's used color-scheme from the
        -- embedding iframe element, so this pins CloudShield's own UI to light while
        -- Exosphere itself stays on the viewer's theme. (CloudShield's dark theme is
        -- broken: dark text on dark bg.) This is a presentation-only attribute and
        -- does not touch the origin-pin, sandbox, referrerpolicy, or keyed remount.
        , Attr.style "color-scheme" "light"
        , Attr.style "width" "100%"
        , Attr.style "height" "85vh"
        , Attr.style "min-height" "600px"
        , Attr.style "border" "0"
        ]
        []


{-| The origin (scheme + host + port) of a resolved iframe `src`, for the provenance bar.
Falls back to the full url if unparseable (cannot happen in the allowed branch, where the
origin-pin has already validated it).
-}
iframeOrigin : String -> String
iframeOrigin url =
    Url.fromString url
        |> Maybe.map originOf
        |> Maybe.withDefault url


{-| Fail-closed origin pin: `True` only when `src` is a non-empty, well-formed https URL
whose origin is an exact member of `allowedOrigins`. Origins are compared by exact string
membership (never substring), so `https://evil.com/https://ok` cannot slip through.
-}
isAllowedIframeSrc : List String -> String -> Bool
isAllowedIframeSrc allowedOrigins src =
    if String.isEmpty src then
        False

    else
        case Url.fromString src of
            Just url ->
                (url.protocol == Url.Https)
                    && List.member (originOf url) allowedOrigins

            Nothing ->
                False


{-| The origin string (scheme + host + optional port) of a parsed URL, matching the shape of
the host's allowlist entries (`https://host` or `https://host:port`).
-}
originOf : Url.Url -> String
originOf url =
    let
        scheme =
            case url.protocol of
                Url.Https ->
                    "https://"

                Url.Http ->
                    "http://"

        portPart =
            case url.port_ of
                Just p ->
                    ":" ++ String.fromInt p

                Nothing ->
                    ""
    in
    scheme ++ url.host ++ portPart



-- CONFIRM DIALOG


confirmOverlay : Maybe Emit -> Html Msg
confirmOverlay pending =
    case pending |> Maybe.andThen .confirm of
        Just confirm ->
            Html.div [ Attr.class ("jr-confirm jr-confirm--" ++ confirm.variant) ]
                [ Html.div [ Attr.class "jr-confirm__box" ]
                    [ Html.h3 [ Attr.class "jr-confirm__title" ] [ Html.text confirm.title ]
                    , Html.p [ Attr.class "jr-confirm__message" ] [ Html.text confirm.message ]
                    , Html.div [ Attr.class "jr-confirm__actions" ]
                        [ Html.button
                            [ Attr.class "jr-confirm__cancel"
                            , Attr.type_ "button"
                            , Events.onClick ConfirmDismissed
                            ]
                            [ Html.text confirm.cancelLabel ]
                        , Html.button
                            [ Attr.class "jr-confirm__confirm"
                            , Attr.type_ "button"
                            , Events.onClick ConfirmAccepted
                            ]
                            [ Html.text confirm.confirmLabel ]
                        ]
                    ]
                ]

        Nothing ->
            Html.text ""



-- HELPERS


arrayAt : String -> Value -> List Value
arrayAt pointer state =
    Expr.getByPath pointer state
        |> Maybe.andThen (Decode.decodeValue (Decode.list Decode.value) >> Result.toMaybe)
        |> Maybe.withDefault []


isNull : Value -> Bool
isNull value =
    Decode.decodeValue (Decode.null True) value
        |> Result.withDefault False
