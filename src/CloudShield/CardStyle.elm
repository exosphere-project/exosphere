module CloudShield.CardStyle exposing (extraRules)

{-| CloudShield's own block of the renderer stylesheet.

[`Exoext.RendererStyle`](Exoext-RendererStyle) styles the catalog — the components any manifest is
built from. It cannot style THIS manifest's layout, because that layout is structure, not
components: a two-column row whose second column is the run history, rows that take an accent
stripe when their state badge says the results session is open, a scan phase this extension names
itself. Those are selectors over one specific element tree and one specific vocabulary, so they
belong to the adapter that publishes that tree.

The host appends these rules after the base ones, so an override here wins at equal specificity
(`.jr-card { gap }` is exactly that). Colors come from
[`RendererStyle.Tokens`](Exoext-RendererStyle#Tokens), the same palette-derived strings the base
uses, so this block stays theme-correct without reaching for the palette itself.

**Selector anchoring.** The columns row, the toolbar, and the history rows are ALL Stack rows, so
these rules key strictly on depth from `.jr-card`:

  - COLUMNS = `.jr-card > .jr-stack--row` (the ONLY row that is a direct card child)
  - TARGETS = COLUMNS `> .jr-stack--col:nth-child(1)`
  - HISTORY = COLUMNS `> .jr-stack--col:nth-child(2)`
  - scroll = TARGETS/HISTORY `> .jr-stack--col` (the repeat container in a column)

The one per-row hook is the rowState Badge's `data-state`, which `:has()` reads to style the whole
row.

@docs extraRules

-}

import Exoext.RendererStyle as RendererStyle


{-| The CloudShield card's structural and vocabulary rules, appended after the base stylesheet.
-}
extraRules : RendererStyle.Tokens -> List String
extraRules t =
    [ -- `scanning` is THIS extension's word for the phase where the clone is being read,
      -- composed by `scanningRowLabel`. The renderer's tone table knows only the §4.4 run states,
      -- so the word falls through to the neutral tone there — which would read as settled on the
      -- one row that is actively working. The adapter tones its own vocabulary here instead,
      -- matching `.jr-badge--info` exactly and reusing the base in-progress ring, which is the
      -- whole reason the generic layer does not need an arm for it.
      ".jr-badge[data-state^=\"scanning\"]::before { content: \"\"; display: inline-block; flex: none; width: 10px; height: 10px; margin-right: 5px; vertical-align: -1px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
    , ".jr-badge[data-state^=\"scanning\"] { background: " ++ t.infoBg ++ "; color: " ++ t.infoText ++ "; border-color: " ++ t.infoBorder ++ "; }"

    -- Severity is CloudShield's grouping, so the per-group dot colors are CloudShield's too: the
    -- base draws a neutral dot and this maps the CVSS bands onto the palette's state families.
    , ".jr-counts__pill--critical .jr-counts__dot, .jr-counts__pill--high .jr-counts__dot { background: " ++ t.dangerDot ++ "; }"
    , ".jr-counts__pill--medium .jr-counts__dot { background: " ++ t.warningDot ++ "; }"
    , ".jr-counts__pill--low .jr-counts__dot { background: " ++ t.infoDot ++ "; }"
    , ".jr-counts__pill--info .jr-counts__dot { background: " ++ t.neutralDot ++ "; }"

    -- REDESIGN v2 — a TWO-COLUMN desktop layout. Provenance (above) and the results region
    -- (below) are host chrome outside the manifest; INSIDE the manifest the card is a two-column
    -- row (scan targets | scan history) followed by the full-width results findings + iframe.
    , ".jr-card { gap: 16px; }"

    -- The two-column grid: targets narrower than history (history rows carry more —
    -- date, target, pills, state, action). On a narrow card it collapses to one column
    -- (see the @media at the end). A hairline divider runs down the gutter.
    , ".jr-card > .jr-stack--row { display: grid; grid-template-columns: 5fr 7fr; gap: 0; align-items: stretch; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col { min-width: 0; gap: 8px; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) { padding-right: 26px; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) { padding-left: 26px; border-left: 1px solid " ++ t.border ++ "; }"

    -- Column headers: an uppercase muted rubric carrying the live count. The history
    -- overflow note ("showing latest 20 of M") is the history column's 2nd Text child.
    , ".jr-card > .jr-stack--row > .jr-stack--col > .jr-text:first-child { text-transform: uppercase; letter-spacing: 0.07em; font-size: 0.72em; font-weight: 700; color: " ++ t.muted ++ "; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col > .jr-text:nth-child(2) { font-size: 0.78em; color: " ++ t.muted ++ "; margin-top: -2px; }"

    -- The scroll areas: BOTH the targets list and the history rows are height-bounded and
    -- scroll internally, so the two columns stay EQUAL height and can never be lopsided
    -- regardless of item counts. Grid `align-items: stretch` grows the shorter column's
    -- scroll area to match the taller; the taller one caps at ~6 rows and scrolls. This
    -- is the key requirement (the researcher may have "unlimited" instances and scans).
    , ".jr-card > .jr-stack--row > .jr-stack--col > .jr-stack--col { flex: 1 1 auto; max-height: 16rem; overflow-y: auto; gap: 2px; }"
    , "[data-exoext-history-loading=\"true\"] .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col { min-height: 3rem; display: flex; flex-direction: row; align-items: center; gap: 8px; padding: 9px 10px; color: " ++ t.muted ++ "; background: " ++ t.frontBg ++ "; border-radius: 8px; }"
    , "[data-exoext-history-loading=\"true\"] .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col::before { content: \"\"; display: inline-block; flex: none; width: 12px; height: 12px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
    , "[data-exoext-history-loading=\"true\"] .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col::after { content: \"Loading history…\"; font-size: 0.9em; }"

    -- Scan-target rows (TARGETS scroll rows): the name grows; the Scan button sits right.
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--col > .jr-stack--row { align-items: center; gap: 10px; padding: 7px 10px; border-radius: 8px; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--col > .jr-stack--row:hover { background: " ++ t.frontBg ++ "; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--col > .jr-stack--row > .jr-text { flex: 1; min-width: 0; font-weight: 500; }"

    -- Toolbar (Select all | Scan selected): the targets column's own row child; the
    -- button pushes to the right edge.
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--row { align-items: center; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--row .jr-button { margin-left: auto; }"

    -- History rows (HISTORY scroll rows): a two-line main block (when + sub) that grows,
    -- then pills, then the state badge, then the action.
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row { align-items: center; gap: 12px; padding: 9px 10px; border-radius: 9px; border-left: 3px solid transparent; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:hover { background: " ++ t.frontBg ++ "; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-stack--col { flex: 1; min-width: 0; gap: 1px; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-stack--col > .jr-text:first-child { font-size: 0.95em; font-weight: 600; color: " ++ t.text ++ "; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-stack--col > .jr-text:last-child { font-size: 0.82em; color: " ++ t.muted ++ "; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) .jr-counts { flex: 0 0 auto; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) .jr-counts__total { display: none; }"

    -- The base hides a badge whose rowState is empty (manifest v1, the common case). Manifest v2
    -- projects the idle token instead of an empty string, so this twin hides the idle history
    -- badge too — scoped to the history column so it can't hide a targets-column "idle" scanState
    -- badge (which stays visible).
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"idle\"] { display: none; }"

    -- Active / now-viewing row: accent stripe + primary tint + a pulsing flag, action
    -- flips to Refresh. Keyed on the history Badge's data-state. Each rule carries BOTH
    -- selectors: the manifest-v1 display string ("Now viewing") and the manifest-v2
    -- token ("viewing", from the Badge `variant`). The token twins are history-scoped
    -- (never a bare global) so they cannot collide with a targets-column scanState. Both
    -- selector sets live side by side until the demo VM redeploys v2.
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Now viewing\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"viewing\"]) { background: " ++ t.primaryTint ++ "; border-left-color: " ++ t.primary ++ "; box-shadow: inset 0 0 0 1px " ++ t.primaryLine ++ "; }"
    , ".jr-badge[data-state=\"Now viewing\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"viewing\"] { display: inline-flex; align-items: center; gap: 5px; background: " ++ t.primaryTintStrong ++ "; color: " ++ t.primary ++ "; border-color: " ++ t.primaryLine ++ "; text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.7em; font-weight: 700; }"
    , ".jr-badge[data-state=\"Now viewing\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"viewing\"]::before { content: \"\"; flex: none; width: 6px; height: 6px; border-radius: 50%; background: " ++ t.primary ++ "; animation: jr-viewing-pulse 2s ease-in-out infinite; }"
    , "@keyframes jr-viewing-pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.4; } }"
    , "@media (prefers-reduced-motion: reduce) { .jr-badge[data-state=\"Now viewing\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"viewing\"]::before { animation: none; } }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Now viewing\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"viewing\"]) .jr-button { background: transparent; border-color: " ++ t.primaryLine ++ "; color: " ++ t.primary ++ "; }"

    -- Opening / loading row: this row's getEmbed is in flight. Same accent surface as
    -- "Now viewing" so the clicked row reads as the one taking over, but the badge carries
    -- a spinning ring (the codebase's in-progress idiom, shared with queued/running) and
    -- the action button is de-emphasized and non-interactive (pointer-events: none) while
    -- the bridge mints the fresh embed.
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Opening…\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"opening\"]) { background: " ++ t.primaryTint ++ "; border-left-color: " ++ t.primaryLine ++ "; }"
    , ".jr-badge[data-state=\"Opening…\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"opening\"] { display: inline-flex; align-items: center; gap: 6px; background: " ++ t.primaryTintStrong ++ "; color: " ++ t.primary ++ "; border-color: " ++ t.primaryLine ++ "; text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.7em; font-weight: 700; }"
    , ".jr-badge[data-state=\"Opening…\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"opening\"]::before { content: \"\"; display: inline-block; flex: none; width: 9px; height: 9px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
    , "@media (prefers-reduced-motion: reduce) { .jr-badge[data-state=\"Opening…\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"opening\"]::before { animation: none; } }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Opening…\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"opening\"]) .jr-button { background: transparent; border-color: " ++ t.primaryLine ++ "; color: " ++ t.primary ++ "; opacity: 0.6; pointer-events: none; }"

    -- Embed-error row: this batch's last getEmbed failed / timed out. Danger-toned badge
    -- + stripe/tint (mirroring the primary active/loading idiom, just danger-colored), and
    -- the action flips to a danger-outlined "Retry" that re-fires getEmbed. Distinct from
    -- a FAILED SCAN below (which has nothing to view): here the scan is fine, only the
    -- results session failed to open, so the button stays and reads "Retry".
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Couldn't open\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"error\"]) { background: " ++ t.dangerTint ++ "; border-left-color: " ++ t.dangerDot ++ "; }"
    , ".jr-badge[data-state=\"Couldn't open\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"error\"] { display: inline-flex; align-items: center; gap: 5px; background: " ++ t.dangerBg ++ "; color: " ++ t.dangerText ++ "; border-color: " ++ t.dangerBorder ++ "; text-transform: uppercase; letter-spacing: 0.04em; font-size: 0.7em; font-weight: 700; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Couldn't open\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"error\"]) .jr-button { background: transparent; border-color: " ++ t.dangerLine ++ "; color: " ++ t.dangerDot ++ "; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Couldn't open\"]) .jr-button:hover, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"error\"]) .jr-button:hover { border-color: " ++ t.dangerDot ++ "; }"

    -- Failed scan row: de-emphasized, a small danger 'failed' pill, and no pills/action
    -- (nothing to view). The host also blanks its actionLabel; this hides the button.
    , ".jr-badge[data-state=\"failed\"] { background: " ++ t.dangerBg ++ "; color: " ++ t.dangerText ++ "; border-color: " ++ t.dangerBorder ++ "; text-transform: uppercase; letter-spacing: 0.04em; font-size: 0.7em; font-weight: 700; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) { opacity: 0.72; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) .jr-button { display: none; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) .jr-counts { display: none; }"

    -- Expired session row (the expired-session fix): the row's result session lapsed, so
    -- the host reverts it from "Now viewing"/"Refresh" to a muted neutral "Expired" badge
    -- and a plain View (reopen). It keeps a FAINT highlight — the same palette-derived
    -- primary accent as the active row, just at a lower alpha — so the researcher still
    -- sees which scan they were on. Neutral toned (like the failed pill but not danger).
    , ".jr-badge[data-state=\"Expired\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"expired\"] { background: " ++ t.frontBg ++ "; color: " ++ t.muted ++ "; border-color: " ++ t.border ++ "; text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.7em; font-weight: 700; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Expired\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"expired\"]) { background: color-mix(in srgb, " ++ t.primary ++ " 5%, transparent); border-left-color: " ++ t.primaryLine ++ "; }"
    , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Expired\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"expired\"]) .jr-button { background: transparent; border-color: " ++ t.border ++ "; color: " ++ t.muted ++ "; }"

    -- Responsive: on a narrow card the two columns stack (targets, then history), each
    -- keeping its own bounded scroll. The divider moves from a left border to a top one.
    , "@media (max-width: 720px) { .jr-card > .jr-stack--row { grid-template-columns: 1fr; } .jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) { padding-right: 0; padding-bottom: 14px; } .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) { padding-left: 0; border-left: 0; border-top: 1px solid " ++ t.border ++ "; padding-top: 14px; } }"
    ]
