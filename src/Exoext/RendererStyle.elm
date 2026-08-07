module Exoext.RendererStyle exposing (Tokens, tokens, stylesheet)

{-| The host's stylesheet for the json-render renderer's `jr-*` classes.

The renderer emits plain `Html` with stable `jr-*` class names and no styling of its own, so the
HOST is what makes a rendered manifest look native. This module is that host stylesheet, and it is
deliberately **extension-agnostic**: it styles the catalog surface (`JsonRender.Spec` components)
and nothing else. Every rule here is one any extension's manifest would want, because every rule
here is about a component, never about a particular manifest's layout.

Colors come from the active [`ExoPalette`](Style-Types#ExoPalette) rather than literals, so the
card reads correctly in both the light and the dark theme.


# The extension-specific layer

A manifest's own layout — "history is the second column", "this row state gets an accent stripe" —
cannot be expressed in a component stylesheet, because those are structural selectors over one
extension's specific element tree. So the base is not the whole stylesheet: [`stylesheet`](#stylesheet)
takes a function from [`Tokens`](#Tokens) to extra rules and appends them AFTER the base, where they
win on equal specificity. An adapter contributes its own block by passing that function; a host with
no extension-specific styling passes `always []`.

The [`Tokens`](#Tokens) record is what makes an extension block palette-correct without reaching
for the palette itself: the adapter writes CSS against the same derived color strings the base uses.

@docs Tokens, tokens, stylesheet

-}

import Color
import Html
import Style.Types exposing (ExoPalette)


{-| Palette-derived CSS color strings, shared by the base rules and by any extension block.

The state families (`info` / `success` / `danger` / `warning`) carry the palette's own tinted
`background` / `border` / `textOnColoredBG`, which read correctly on both light and dark pages, so
badges and pills stay legible either way. The `*Dot` values are the solid variants, for small
filled shapes. The `primary*` / `danger*` tint and line values are `color-mix` accents derived
ENTIRELY from the palette (never a hardcoded hue), so they flip with the theme too.

-}
type alias Tokens =
    { text : String
    , muted : String
    , border : String
    , frontBg : String
    , primary : String
    , infoBg : String
    , infoText : String
    , infoBorder : String
    , successBg : String
    , successText : String
    , successBorder : String
    , dangerBg : String
    , dangerText : String
    , dangerBorder : String
    , warningBg : String
    , warningText : String
    , warningBorder : String
    , dangerDot : String
    , warningDot : String
    , infoDot : String
    , neutralDot : String
    , primaryTint : String
    , primaryTintStrong : String
    , primaryLine : String
    , dangerTint : String
    , dangerLine : String
    }


{-| Derive the [`Tokens`](#Tokens) for a palette.
-}
tokens : ExoPalette -> Tokens
tokens palette =
    let
        c =
            Color.toCssString

        primary =
            c palette.primary

        dangerDot =
            c palette.danger.default
    in
    { text = c palette.neutral.text.default
    , muted = c palette.neutral.text.subdued
    , border = c palette.neutral.border
    , frontBg = c palette.neutral.background.frontLayer
    , primary = primary
    , infoBg = c palette.info.background
    , infoText = c palette.info.textOnColoredBG
    , infoBorder = c palette.info.border
    , successBg = c palette.success.background
    , successText = c palette.success.textOnColoredBG
    , successBorder = c palette.success.border
    , dangerBg = c palette.danger.background
    , dangerText = c palette.danger.textOnColoredBG
    , dangerBorder = c palette.danger.border
    , warningBg = c palette.warning.background
    , warningText = c palette.warning.textOnColoredBG
    , warningBorder = c palette.warning.border
    , dangerDot = dangerDot
    , warningDot = c palette.warning.default
    , infoDot = c palette.info.default
    , neutralDot = c palette.muted.default

    -- Interactive/active accent, derived ENTIRELY from the Exosphere primary (never a
    -- hardcoded hue): a faint fill, a stronger fill, and a hairline. `color-mix` keeps these
    -- correct in both themes because `primary` itself flips with the palette.
    , primaryTint = "color-mix(in srgb, " ++ primary ++ " 10%, transparent)"
    , primaryTintStrong = "color-mix(in srgb, " ++ primary ++ " 16%, transparent)"
    , primaryLine = "color-mix(in srgb, " ++ primary ++ " 55%, transparent)"

    -- The same tint/line idiom in the danger family, so a danger-toned surface reads in the
    -- same visual language as the active one. `color-mix` keeps both correct in either theme.
    , dangerTint = "color-mix(in srgb, " ++ dangerDot ++ " 9%, transparent)"
    , dangerLine = "color-mix(in srgb, " ++ dangerDot ++ " 55%, transparent)"
    }


{-| The `<style>` node to mount alongside a rendered manifest: the base catalog rules, then the
extension's own block appended after them (so an extension override wins at equal specificity).

`extraRules` is the adapter's contribution, written against the same [`Tokens`](#Tokens) the base
uses. Pass `always []` for a host that has no extension-specific layout to style.

-}
stylesheet : ExoPalette -> (Tokens -> List String) -> Html.Html msg
stylesheet palette extraRules =
    let
        t =
            tokens palette
    in
    Html.node "style"
        []
        [ Html.text (String.join "\n" (baseRules t ++ extraRules t)) ]


{-| The catalog stylesheet: one block per `JsonRender.Spec` component, scoped to `.jr-*` names.
-}
baseRules : Tokens -> List String
baseRules t =
    [ ".jr-root { font-family: inherit; color: " ++ t.text ++ "; }"
    , ".jr-card { display: flex; flex-direction: column; gap: 10px; padding: 4px 0; }"
    , ".jr-card__title { font-size: 1.05em; margin: 0 0 4px 0; font-weight: 600; }"
    , ".jr-stack { display: flex; gap: 10px; }"
    , ".jr-stack--row { flex-direction: row; align-items: center; }"
    , ".jr-stack--col { flex-direction: column; align-items: stretch; }"
    , ".jr-text { }"
    , ".jr-button { padding: 4px 12px; border: 1px solid " ++ t.border ++ "; border-radius: 4px; background: " ++ t.frontBg ++ "; color: " ++ t.text ++ "; cursor: pointer; font-size: 0.9em; }"
    , ".jr-button:hover { border-color: " ++ t.primary ++ "; color: " ++ t.primary ++ "; }"

    -- A `disabled` action. Dimmed and not-allowed rather than hidden, so the control stays where
    -- the eye expects it and the press reads as temporarily unavailable, not missing. The hover
    -- rule is re-neutralized because `:hover` still fires over a disabled button.
    , ".jr-button--disabled, .jr-button--disabled:hover { opacity: 0.45; cursor: not-allowed; border-color: " ++ t.border ++ "; color: " ++ t.muted ++ "; }"
    , ".jr-checkbox { display: inline-flex; align-items: center; gap: 6px; }"
    , ".jr-checkbox input { accent-color: " ++ t.primary ++ "; }"
    , ".jr-badge { padding: 1px 9px; border-radius: 999px; font-size: 0.8em; border: 1px solid transparent; }"

    -- In-progress badges get a small spinning ring before the label so an active state reads as
    -- moving. `currentColor` inherits the badge tone color. `stopping` belongs here for a reason
    -- the others do not have: it is the ONLY feedback a stop press gets, because the stop control
    -- withdraws with the press. Drawn still, the row would read as already stopped while the
    -- publisher is in fact still winding down, and a still-working row that looks finished is what
    -- gets pressed a second time. Prefix selectors, so a state carrying a display suffix or an
    -- ellipsis (`stopping…`) still matches.
    --
    -- `flex: none` is what keeps the ring ROUND. Every round box in this stylesheet is a
    -- fixed-size pseudo-element sitting inside a badge or row that is itself a flex container, so
    -- it is a flex ITEM: its default `flex-shrink: 1` lets the main axis (here the width) be
    -- compressed when the line runs out of room, while the cross axis keeps its declared size, and
    -- a circle compressed on one axis is an oval. Nothing about `border-radius: 50%` prevents that
    -- — 50% of an oval is an oval.
    , ".jr-badge[data-state^=\"queued\"]::before, .jr-badge[data-state^=\"running\"]::before, .jr-badge[data-state^=\"stopping\"]::before { content: \"\"; display: inline-block; flex: none; width: 10px; height: 10px; margin-right: 5px; vertical-align: -1px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
    , "@keyframes jr-badge-spin { to { transform: rotate(360deg); } }"

    -- A badge whose resolved `data-state` is empty draws nothing at all. This is how a manifest
    -- says "no state here" for one row of a repeat without having to omit the element.
    , ".jr-badge[data-state=\"\"] { display: none; }"
    , ".jr-badge--neutral { background: " ++ t.frontBg ++ "; color: " ++ t.muted ++ "; border-color: " ++ t.border ++ "; }"
    , ".jr-badge--info { background: " ++ t.infoBg ++ "; color: " ++ t.infoText ++ "; border-color: " ++ t.infoBorder ++ "; }"
    , ".jr-badge--success { background: " ++ t.successBg ++ "; color: " ++ t.successText ++ "; border-color: " ++ t.successBorder ++ "; }"
    , ".jr-badge--danger { background: " ++ t.dangerBg ++ "; color: " ++ t.dangerText ++ "; border-color: " ++ t.dangerBorder ++ "; }"

    -- Findings summary: a single clean row of severity pills (dot + count + label), ordered by
    -- the renderer; an extension tints the per-group dot from its own block.
    , ".jr-findings { display: flex; flex-wrap: wrap; align-items: center; gap: 8px; }"
    , ".jr-findings--empty { color: " ++ t.muted ++ "; font-size: 0.9em; font-style: italic; }"
    , ".jr-findings__total { color: " ++ t.muted ++ "; font-size: 0.85em; font-weight: 600; margin-right: 2px; }"
    , ".jr-findings__pill { display: inline-flex; align-items: center; gap: 6px; padding: 2px 10px; border-radius: 999px; background: " ++ t.frontBg ++ "; border: 1px solid " ++ t.border ++ "; font-size: 0.85em; }"
    , ".jr-findings__dot { width: 8px; height: 8px; border-radius: 50%; background: " ++ t.neutralDot ++ "; flex: none; }"
    , ".jr-findings__count { font-weight: 700; color: " ++ t.text ++ "; }"
    , ".jr-findings__label { color: " ++ t.muted ++ "; text-transform: capitalize; }"

    -- Table: a plain data grid. Header rule is heavier than row rules; all lines use
    -- the neutral border so it reads as quiet structure, not a colored callout.
    , ".jr-table { width: 100%; border-collapse: collapse; font-size: 0.85em; }"
    , ".jr-table__header { text-align: left; font-weight: 600; color: " ++ t.muted ++ "; padding: 6px 10px; border-bottom: 2px solid " ++ t.border ++ "; }"
    , ".jr-table__cell { padding: 6px 10px; color: " ++ t.text ++ "; border-bottom: 1px solid " ++ t.border ++ "; }"

    -- Alert: a tinted callout box, one tone per level. Same bg/text/border palette
    -- families as the badges so tones stay consistent across the card in both themes.
    , ".jr-alert { padding: 10px 12px; border-radius: 6px; border: 1px solid transparent; font-size: 0.9em; }"
    , ".jr-alert__title { display: block; font-weight: 600; margin-bottom: 3px; }"
    , ".jr-alert__message { display: block; line-height: 1.45; }"
    , ".jr-alert--info { background: " ++ t.infoBg ++ "; color: " ++ t.infoText ++ "; border-color: " ++ t.infoBorder ++ "; }"
    , ".jr-alert--warning { background: " ++ t.warningBg ++ "; color: " ++ t.warningText ++ "; border-color: " ++ t.warningBorder ++ "; }"
    , ".jr-alert--danger { background: " ++ t.dangerBg ++ "; color: " ++ t.dangerText ++ "; border-color: " ++ t.dangerBorder ++ "; }"

    -- Iframe chrome: the provenance bar reads as quiet host chrome (subdued text on
    -- the front layer), and the frame carries a distinct-but-neutral border so the
    -- embed is visibly framed as third-party without alarming. Works in both themes.
    , ".jr-iframe { display: flex; flex-direction: column; }"
    , ".jr-iframe__provenance { padding: 5px 10px; font-size: 0.78em; color: " ++ t.muted ++ "; background: " ++ t.frontBg ++ "; border: 1px solid " ++ t.border ++ "; border-bottom: 0; border-radius: 6px 6px 0 0; }"
    , ".jr-iframe__frame { border: 1px solid " ++ t.border ++ "; border-radius: 0 0 6px 6px; overflow: hidden; }"
    , ".jr-confirm { position: fixed; inset: 0; background: rgba(0,0,0,0.55); display: flex; align-items: center; justify-content: center; z-index: 1000; }"

    -- `white-space: normal` re-establishes wrapping inside the dialog: the card is raw
    -- HTML embedded in an elm-ui tree, and elm-ui sets `white-space: pre` on every `el`,
    -- which inherits into the overlay — an unwrapped message overflows the box.
    , ".jr-confirm__box { background: " ++ t.frontBg ++ "; color: " ++ t.text ++ "; padding: 20px 22px; border-radius: 8px; max-width: 380px; white-space: normal; border: 1px solid " ++ t.border ++ "; box-shadow: 0 8px 40px rgba(0,0,0,0.5); }"
    , ".jr-confirm__title { margin: 0 0 8px 0; font-size: 1.1em; font-weight: 600; }"
    , ".jr-confirm__message { margin: 0; color: " ++ t.muted ++ "; line-height: 1.45; }"
    , ".jr-confirm__actions { display: flex; gap: 8px; justify-content: flex-end; margin-top: 18px; }"
    , ".jr-confirm__cancel, .jr-confirm__confirm { padding: 6px 16px; border-radius: 4px; cursor: pointer; font-size: 0.9em; }"
    , ".jr-confirm__cancel { background: transparent; color: " ++ t.text ++ "; border: 1px solid " ++ t.border ++ "; }"
    , ".jr-confirm__cancel:hover { background: " ++ t.frontBg ++ "; }"
    , ".jr-confirm__confirm { background: " ++ t.primary ++ "; color: #fff; border: 1px solid " ++ t.primary ++ "; }"
    , ".jr-confirm__confirm:hover { filter: brightness(1.08); }"
    , ".jr-error, .jr-error-stub { border: 1px solid " ++ t.dangerBorder ++ "; padding: 8px 10px; border-radius: 4px; color: " ++ t.dangerText ++ "; background: " ++ t.dangerBg ++ "; }"
    ]
