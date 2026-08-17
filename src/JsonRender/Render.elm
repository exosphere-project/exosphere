module JsonRender.Render exposing
    ( Model, init
    , Msg, update
    , Effect(..)
    , Options, CountPillDefaults, defaultOptions, view
    , badgeTone
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

@docs Options, CountPillDefaults, defaultOptions, view


# Styling hooks

@docs badgeTone

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
import Svg
import Svg.Attributes as SvgAttr
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


{-| What the host tells the renderer about itself.

  - `allowedIframeOrigins` — the iframe origin allowlist: an `Iframe` element renders only when
    its resolved `src` is an https URL whose origin is an exact member of this list. An empty
    list disables all iframes (fail-closed).
  - `countPills` — the vocabulary a `CountPills` element falls back to.

-}
type alias Options =
    { allowedIframeOrigins : List String
    , countPills : CountPillDefaults
    }


{-| The words a `CountPills` element uses when its manifest does not name its own.

The renderer counts rows and groups them; it has no opinion about what a row IS. An adapter does:
it knows what one row is called, which field groups them, and which group belongs at the front. A
manifest published before `CountPills` could carry those keys cannot say so itself, so the adapter
that mounts the renderer supplies them and its already-published manifests keep reading correctly.

  - `groupBy` — the record field to group rows on.
  - `groupOrder` — group values in display order. Anything outside the list sorts after the
    listed ones by descending count then name, which is what an EMPTY list does to everything.
  - `itemNoun` / `itemNounPlural` — what one row and many rows are called.

-}
type alias CountPillDefaults =
    { groupBy : String
    , groupOrder : List String
    , itemNoun : String
    , itemNounPlural : String
    }


{-| Options for a host that has no extension vocabulary of its own: no iframes, no group order
(so counts fall back to descending), and rows that are simply "items".
-}
defaultOptions : Options
defaultOptions =
    { allowedIframeOrigins = []
    , countPills =
        { groupBy = "group"
        , groupOrder = []
        , itemNoun = "item"
        , itemNounPlural = "items"
        }
    }


{-| Render the spec against the current host-owned `state`. The returned `Html Msg`
includes the confirm dialog overlay when one is pending.
-}
view : Options -> Spec -> Value -> Model -> Html Msg
view opts spec state (Model model) =
    Html.div [ Attr.class "jr-root" ]
        [ renderElement opts spec (Expr.rootContext opts.allowedIframeOrigins state) spec.root
        , confirmOverlay model.pendingConfirm
        ]


renderElement : Options -> Spec -> Context -> String -> Html Msg
renderElement opts spec ctx id =
    case Dict.get id spec.elements of
        Just element ->
            renderUIElement opts spec ctx element

        Nothing ->
            -- Cannot happen for a decoded spec (child refs are validated); fail-closed stub.
            Html.div [ Attr.class "jr-error" ]
                [ Html.text ("Missing element: " ++ id) ]


renderUIElement : Options -> Spec -> Context -> UIElement -> Html Msg
renderUIElement opts spec ctx element =
    let
        childrenHtml =
            case element.repeat of
                Just repeat ->
                    repeatChildren opts spec ctx element repeat

                Nothing ->
                    List.map (renderElement opts spec ctx) element.children
    in
    renderComponent opts.countPills ctx element childrenHtml


repeatChildren : Options -> Spec -> Context -> UIElement -> Repeat -> List (Html Msg)
repeatChildren opts spec ctx element repeat =
    let
        items =
            arrayAt repeat.statePath ctx.state

        renderRow index item =
            let
                rowCtx =
                    Expr.childContext repeat.statePath index item ctx
            in
            List.map (renderElement opts spec rowCtx) element.children
    in
    List.concat (List.indexedMap renderRow items)


renderComponent : CountPillDefaults -> Context -> UIElement -> List (Html Msg) -> Html Msg
renderComponent countPillDefaults ctx element childrenHtml =
    case element.props of
        CardP props ->
            renderCard ctx props childrenHtml

        StackP props ->
            renderStack ctx element props childrenHtml

        TextP props ->
            Html.span (Attr.class "jr-text" :: pressableAttrs ctx element)
                [ Html.text (Expr.resolveDisplay ctx props.value) ]

        BadgeP props ->
            renderBadge ctx element props

        ButtonP props ->
            renderButton ctx element props

        CheckboxP props ->
            renderCheckbox ctx props

        CountPillsP props ->
            renderCountPills countPillDefaults ctx props

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


renderStack : Context -> UIElement -> Spec.StackProps -> List (Html Msg) -> Html Msg
renderStack ctx element props childrenHtml =
    let
        directionClass =
            case props.direction of
                Spec.Row ->
                    "jr-stack--row"

                Spec.Col ->
                    "jr-stack--col"
    in
    Html.div
        (Attr.class ("jr-stack " ++ directionClass)
            :: Attr.attribute "data-gap" (String.fromInt props.gap)
            :: pressableAttrs ctx element
        )
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


renderBadge : Context -> UIElement -> Spec.BadgeProps -> Html Msg
renderBadge ctx element props =
    let
        label =
            Expr.resolveDisplay ctx props.value

        -- `data-state` carries the styling token: an explicit `variant` when the manifest
        -- supplies one, else the display text itself (the historical behavior). The tone class
        -- stays keyed on the display text so a variant never changes the visible tone.
        state =
            case props.variant of
                Just variantExpr ->
                    Expr.resolveDisplay ctx variantExpr

                Nothing ->
                    label
    in
    Html.span
        (Attr.class ("jr-badge jr-badge--" ++ badgeTone label)
            :: Attr.attribute "data-state" state
            :: pressableAttrs ctx element
        )
        [ Html.text label ]


{-| Tone mapping from a per-row state string. Keyed on the leading whitespace-delimited token so a
value that carries a trailing detail suffix (e.g. `"running · 0:15"`, a live run's counting-up
elapsed) still maps to its state tone. Mirrors the Track A island's state→tone table
(`pinned-format-reference.md` §"STILL-UNCERTAIN" item 3).

The arms are the §4.4 run states and nothing else. A publisher whose own vocabulary has a word this
table does not know gets the neutral fall-through, and supplies a `variant` if it wants a specific
tone — which is the mechanism that exists for exactly this, and is why no extension's private verb
needs an arm of its own here.

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

        "stopping" ->
            -- In flight like the three above, and toned the same on purpose: a run that has been
            -- asked to stop has NOT stopped yet, and the neutral tone this used to fall through to
            -- read as already-over — which is the one thing this state must not say.
            "info"

        "done" ->
            "success"

        "error" ->
            "danger"

        _ ->
            "neutral"


{-| The leading token of a badge value: everything before the first space, minus a trailing
ellipsis. So `"running · 0:15"` tones as `"running"` and `"stopping…"` tones as `"stopping"`,
while a plain `"done"` is unchanged.

The ellipsis is dropped because it is punctuation on the display word, not part of the state: a
manifest that writes an in-flight state as `"stopping…"` means the same state as one that writes
`"stopping"`, and toning the two differently would make the tone table depend on the publisher's
typography.

-}
badgeToken : String -> String
badgeToken state =
    let
        leading =
            state |> String.split " " |> List.head |> Maybe.withDefault state
    in
    if String.endsWith "…" leading then
        String.dropRight 1 leading

    else
        leading


{-| Render a `Button`. A truthy `disabled` expression makes it inert three ways over: the native
`disabled` attribute, a `jr-button--disabled` class for the host stylesheet, and **no press handler
at all** — the last is the one that actually holds, since the others are only presentation.

An **empty resolved `label` renders NOTHING**, the same rule [`renderIframe`](#renderIframe) applies
to an empty `src`. The catalog has no `visible` prop (element-level visibility is deliberately
refused), so resolving the label to `""` is how a manifest says "this action does not apply to this
row" — a per-row `$cond` chain collapsing to the empty string. Emitting a button anyway produced an
invisible control with a live press handler on every row the action did NOT apply to, which is a
phantom clickable: the empty-label case is exactly the one where the press carries no id.

The exclusion has to be structural, not a stylesheet rule. The badge equivalent IS CSS
(`.jr-badge[data-state=""] { display: none }`) and that is fine, because a hidden badge is only
unreadable — a hidden button is still clickable.

An **`icon` is the one exception**, and the only one: a button carrying a glyph is visible and
meaningful with no text at all, so `label: ""` + `icon` renders icon-only rather than nothing. That
is a manifest saying "this control is a shape", not "this control does not apply". Suppression
still works for icon buttons the same way it works everywhere else — a manifest that wants the
control gone on some rows uses a row where the button is not emitted, not an empty label. The
glyph makes the accessible name the renderer's job: an icon-only control the manifest cannot name
would be invisible to a screen reader, so `aria-label` and `title` come from the icon set.

-}
renderButton : Context -> UIElement -> Spec.ButtonProps -> Html Msg
renderButton ctx element props =
    let
        label =
            Expr.resolveDisplay ctx props.label

        glyph =
            Maybe.andThen iconGlyph props.icon
    in
    case ( String.isEmpty label, glyph ) of
        ( True, Nothing ) ->
            Html.text ""

        _ ->
            let
                isDisabled =
                    props.disabled |> Maybe.map (Expr.resolveBool ctx) |> Maybe.withDefault False

                handler =
                    case ( isDisabled, pressEmit ctx element ) of
                        ( False, Just emit ) ->
                            [ pressClick emit ]

                        _ ->
                            []

                disabledClass =
                    if isDisabled then
                        " jr-button--disabled"

                    else
                        ""

                iconOnly =
                    String.isEmpty label

                iconClass =
                    case ( glyph, iconOnly ) of
                        ( Nothing, _ ) ->
                            ""

                        ( Just _, False ) ->
                            " jr-button--icon"

                        ( Just _, True ) ->
                            " jr-button--icon jr-button--icon-only"

                nameAttrs =
                    case ( glyph, iconOnly ) of
                        ( Just g, True ) ->
                            [ Attr.attribute "aria-label" g.name, Attr.title g.name ]

                        _ ->
                            []

                children =
                    case glyph of
                        Nothing ->
                            [ Html.text label ]

                        Just g ->
                            if iconOnly then
                                [ iconSvg g ]

                            else
                                [ iconSvg g, Html.text label ]
            in
            Html.button
                (Attr.class ("jr-button" ++ iconClass ++ disabledClass)
                    :: Attr.type_ "button"
                    :: Attr.disabled isDisabled
                    :: nameAttrs
                    ++ handler
                )
                children


{-| The closed icon set: the accessible name for each shape, and the path that draws it. The
names match [`JsonRender.Spec`](JsonRender.Spec)'s decoder, which rejects anything else, so
`Nothing` is unreachable through a decoded manifest — and if it were ever reached the button falls
back to the plain label rendering rather than to a nameless empty control.

The paths are Feather's, stroked rather than filled, on Feather's 24×24 grid. They are written out
here rather than imported so the renderer keeps no icon-library dependency: the catalog owns its
glyphs the same way it owns its components.

-}
iconGlyph : String -> Maybe { key : String, name : String, path : String }
iconGlyph icon =
    case icon of
        "trash" ->
            Just
                { key = "trash"
                , name = "Remove"
                , path = "M3 6h18M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2M10 11v6M14 11v6"
                }

        "close" ->
            Just
                { key = "close"
                , name = "Close"
                , path = "M18 6 6 18M6 6l12 12"
                }

        "external" ->
            Just
                { key = "external"
                , name = "Open"
                , path = "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6M15 3h6v6M10 14 21 3"
                }

        "refresh" ->
            Just
                { key = "refresh"
                , name = "Refresh"
                , path = "M23 4v6h-6M1 20v-6h6M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15"
                }

        _ ->
            Nothing


{-| A glyph as inline SVG: 16px, `currentColor`, stroked, so it takes the button's own color in
both the light and the dark theme without the host shipping an icon font or a sprite.
`aria-hidden` keeps it out of the accessibility tree — the BUTTON carries the name, the drawing is
decoration. The `jr-icon--<name>` modifier is the host's per-glyph hook, so the stylesheet can
give the destructive shape a destructive hover without the manifest saying so.
-}
iconSvg : { key : String, name : String, path : String } -> Html Msg
iconSvg glyph =
    Svg.svg
        [ SvgAttr.class ("jr-icon jr-icon--" ++ glyph.key)
        , SvgAttr.width "16"
        , SvgAttr.height "16"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ Svg.path [ SvgAttr.d glyph.path ] [] ]


{-| Attributes that turn a non-`Button` element carrying an `on.press` binding into a
keyboard-operable button: `role="button"`, `tabindex="0"`, the `jr-pressable` class (the
host's styling hook for cursor / hover / focus), a click handler, and Enter/Space keydown
wired to the same emit. `confirm` still runs through the usual `buildEmit` path.

An element without a press binding gets **no** attributes at all, so it renders exactly as
it did before this existed.

Both handlers stop propagation, so a pressable nested inside a pressable (a row `Stack`
holding a pressable `Text`, say) fires exactly one action: the innermost one. A `Checkbox`
nested in a pressable `Stack` is the exception, since its click is not a press binding: it
toggles _and_ presses the enclosing stack.

-}
pressableAttrs : Context -> UIElement -> List (Html.Attribute Msg)
pressableAttrs ctx element =
    case pressEmit ctx element of
        Just emit ->
            [ Attr.class "jr-pressable"
            , Attr.attribute "role" "button"
            , Attr.tabindex 0
            , pressClick emit
            , pressKeydown emit
            ]

        Nothing ->
            []


pressClick : Emit -> Html.Attribute Msg
pressClick emit =
    Events.stopPropagationOn "click" (Decode.succeed ( Pressed emit, True ))


{-| Enter and Space activate a pressable, matching native button behavior. Any other key
fails the decoder, so the event is left entirely alone (no emit, no `preventDefault`).
Space is `" "` per the UI Events spec; `"Spacebar"` is the legacy name still emitted by
older Edge/IE. `preventDefault` keeps Space from scrolling the page.
-}
pressKeydown : Emit -> Html.Attribute Msg
pressKeydown emit =
    Events.custom "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" || key == " " || key == "Spacebar" then
                        Decode.succeed
                            { message = Pressed emit
                            , stopPropagation = True
                            , preventDefault = True
                            }

                    else
                        Decode.fail "not an activation key"
                )
        )


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


{-| Render a `CountPills` element: one pill per group, or the empty state.

The renderer counts and orders; the WORDS are the publisher's. Each of `groupBy`, `groupOrder`,
`itemNoun` and `itemNounPlural` comes from the manifest when it names one and from the host's
[`CountPillDefaults`](#CountPillDefaults) when it does not, so a manifest published before those
keys existed still reads in the vocabulary its adapter set.

The empty state is the manifest's to name too. `emptyLabel` resolves to the text shown when there
are no groups; absent it falls back to "No <plural> yet", and resolving to an empty string renders
NO node at all (for a row that already says "failed" elsewhere, a second empty-state line would be
double-messaging).

-}
renderCountPills : CountPillDefaults -> Context -> Spec.CountPillsProps -> Html Msg
renderCountPills fallback ctx props =
    let
        plural =
            Maybe.withDefault fallback.itemNounPlural props.itemNounPlural

        groups =
            Expr.resolve ctx props.bind
                |> decodeRows
                |> countByGroup (Maybe.withDefault fallback.groupBy props.groupBy)
                |> orderGroups (Maybe.withDefault fallback.groupOrder props.groupOrder)
    in
    case groups of
        [] ->
            let
                emptyLabel =
                    props.emptyLabel
                        |> Maybe.map (Expr.resolveDisplay ctx)
                        |> Maybe.withDefault ("No " ++ plural ++ " yet")
            in
            if String.isEmpty emptyLabel then
                Html.text ""

            else
                Html.div [ Attr.class "jr-counts jr-counts--empty" ]
                    [ Html.text emptyLabel ]

        _ ->
            let
                total =
                    List.sum (List.map Tuple.second groups)

                noun =
                    if total == 1 then
                        Maybe.withDefault fallback.itemNoun props.itemNoun

                    else
                        plural
            in
            Html.div [ Attr.class "jr-counts" ]
                (Html.span [ Attr.class "jr-counts__total" ]
                    [ Html.text (String.fromInt total ++ " " ++ noun) ]
                    :: List.map countPill groups
                )


decodeRows : Value -> List (Dict.Dict String Value)
decodeRows value =
    if isNull value then
        []

    else
        Decode.decodeValue (Decode.list (Decode.dict Decode.value)) value
            |> Result.withDefault []


{-| One at-a-glance pill: a group-colored dot, the count, then the group. The
`jr-counts__pill--<group>` modifier is what lets the host stylesheet tint the dot per group.
-}
countPill : ( String, Int ) -> Html Msg
countPill ( label, count ) =
    Html.span [ Attr.class ("jr-counts__pill jr-counts__pill--" ++ String.toLower label) ]
        [ Html.span [ Attr.class "jr-counts__dot" ] []
        , Html.span [ Attr.class "jr-counts__count" ] [ Html.text (String.fromInt count) ]
        , Html.span [ Attr.class "jr-counts__label" ] [ Html.text label ]
        ]


countByGroup : String -> List (Dict.Dict String Value) -> List ( String, Int )
countByGroup groupBy rows =
    rows
        |> List.foldr
            (\row acc ->
                let
                    key =
                        Dict.get groupBy row
                            |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                            |> Maybe.withDefault "ungrouped"
                in
                Dict.update key (Maybe.withDefault 0 >> (+) 1 >> Just) acc
            )
            Dict.empty
        |> Dict.toList


{-| Order the grouped counts by the publisher's `groupOrder` rather than alphabetically, dropping
any zero counts. Groups the order does not name sort after the ones it does, by descending count
then name — which is what an EMPTY order does to every group, and is the sensible reading of a
grouping whose values the publisher never enumerated.
-}
orderGroups : List String -> List ( String, Int ) -> List ( String, Int )
orderGroups order =
    List.filter (\( _, count ) -> count > 0)
        >> List.sortWith (compareGroup (groupRank order))


compareGroup : (String -> Maybe Int) -> ( String, Int ) -> ( String, Int ) -> Order
compareGroup rankOf ( labelA, countA ) ( labelB, countB ) =
    case ( rankOf labelA, rankOf labelB ) of
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


groupRank : List String -> String -> Maybe Int
groupRank order label =
    let
        lowered =
            String.toLower label
    in
    order
        |> List.indexedMap (\index name -> ( String.toLower name, index ))
        |> List.filter (\( name, _ ) -> name == lowered)
        |> List.head
        |> Maybe.map Tuple.second



-- TABLE


{-| Render a `Table`: a `<thead>` of column labels and one `<tr>` per bound row. Each cell
reads `row[column.key]` (a missing key renders empty). The `jr-table` / `jr-table__header`
/ `jr-table__row` / `jr-table__cell` classes drive host styling.
-}
renderTable : Context -> Spec.TableProps -> Html Msg
renderTable ctx props =
    let
        rows =
            Expr.resolve ctx props.bind |> decodeRows
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
        -- embedding iframe element, so this pins the embedded app's own UI to light
        -- while Exosphere itself stays on the viewer's theme, which is what an embedded
        -- app with a broken dark theme needs. This is a presentation-only attribute and
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
