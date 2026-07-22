# CloudShield dynamic-UI card — Phase 1 (browser side)

This is the Exosphere-side integration of the CloudShield × Exosphere **dynamic-UI extension
mechanism**: a VM publishes a [json-render](https://github.com/vercel-labs/json-render) UI
manifest, Exosphere discovers it via Nova server metadata, validates it fail-closed against a
fixed catalog, and renders it inside that instance's detail page. A button maps to a host verb
(`cloudshield.startScan`) that drops a scan-request; polled status drives the badges live.

Built to the **frozen Phase 0 contract** (`~/dev/bnr/phase-0-spec.md`) on the **no-Jetstream2
POC transport** (server metadata + console log, spec §7 / §7.1). Everything is gated behind
`context.experimentalFeaturesEnabled`, so normal operation is unaffected.

## What's here

| File | Role |
|------|------|
| `../JsonRender.elm`, `../JsonRender/{Spec,Expr,Render}.elm` | **Vendored** native Elm json-render renderer (`avantelogic/elm-json-render`, Track B — durable, no-build, fail-closed). Source-vendored because it isn't on the package registry yet. Only deps are `elm/core`, `elm/html`, `elm/json` (already in Exosphere). |
| `Card.elm` | CloudShield-specific host wiring: owns the json-render `state`, projects it for the renderer (§1.2), handles renderer `Effect`s (start scans, checkbox write-backs), draws the provenance "?" marker (§5.2) + opt-in affordance (§5.3), and embeds the frozen `card.json`. |
| `../Exoext/Discovery.elm` | Generic exoext sentinel detection (`exoext.v1.kind`, §3.1), manifest-body assembly from metadata chunks (§7.1), and the §2.4 `$instances` eligibility filter (ACTIVE-only, exclude self). |
| `../Exoext/Transport.elm` | Generic exoext POC wire framing (§7.1): §4.1 scan-request encode, the `exoext.v1.req.*` request slot, and the `exoext.v1.run.*` / `exoext.v1.res.*` status/result read. |
| `../../tests/Tests/CloudShield/Card.elm` | 19 unit tests over the pure logic (manifest validates fail-closed; projection; discovery; eligibility; transport framing). |

Integration point: `Page/ServerDetail.elm` (`serverDetail_` adds a `cloudShieldCard` tile;
`Model`/`Msg`/`init`/`update` gain a `cloudShield` field + `CloudShieldMsg`).

## Run it locally

```sh
cd ~/dev/exosphere
npm install            # if not already
npm start              # elm-watch hot at app.exosphere.localhost
```

Then in the browser:

1. **Enable experimental features**: Settings → toggle "experimental features" on (the card is
   gated on `context.experimentalFeaturesEnabled`, off by default).
2. Open any **instance/server detail page**. A **"CloudShield (extension)"** tile appears.
3. The tile shows the **opt-in affordance** first (§5.3 — off until acknowledged). Click
   **"Enable CloudShield extension."**
4. The card renders: the **provenance "?" marker** ("Published by the VM …, not by Exosphere"),
   a **discovery note** (sentinel present/absent + store mode), **Select all**, and a **live
   per-row list of the project's other ACTIVE instances** with a checkbox, name, status
   **Badge**, and per-row **Scan** button.

### Manual verification checklist

- [ ] Card is **absent** until experimental features are on **and** the extension is enabled.
- [ ] **Provenance marker** names the source VM and is not part of the manifest body.
- [ ] **Select all** toggles every row's checkbox; per-row checkboxes update select-all.
- [ ] Clicking **Scan** (row or "Scan selected") opens the **confirm dialog**; on confirm the
      targeted rows flip to a **`queued`** badge (info tone).
- [ ] With dev-tools open, the confirmed scan issues `POST …/servers/<uuid>/metadata` writes
      for `exoext.v1.req.seq` + `exoext.v1.req.body.0…` (the §7.1 request slot) on the viewed VM.
- [ ] An **off-catalog manifest** (e.g. a `ScriptInjector` node) renders the **error stub**, never a
      partial tree (covered by `Tests.CloudShield.Card`; `JsonRender` decode is fail-closed).

## What works

- **Vendored renderer compiles + renders** inside Exosphere's elm-ui tree (`Element.html`),
  XSS-safe by construction (no `innerHTML`).
- **Fail-closed catalog validation** is the security gate (`JsonRender.decodeString`): off-catalog
  type / bad prop / dangling child / oversized → error stub.
- **Discovery** reads the metadata sentinel and assembles a `store=metadata` manifest body; the
  card uses the real transport body when present, else the embedded frozen `card.json`.
- **Live `$instances`** is the real, eligibility-filtered project server list (not the VM's data).
- **Action round-trip**: a confirmed `startScan` re-resolves the target against Exosphere's own
  list (§5.4), encodes the §4.1 request, and writes the §7.1 metadata request slot on the
  CloudShield VM; targeted rows go `queued` optimistically.
- **Live status**: polled `exoext.v1.run.state` (read on each render from the VM's metadata) is
  projected onto its target row by `seq`, overriding the optimistic badge.
- `elm-test-rs` (159 + 12), optimized prod build, `elm-format`, and `elm-review` all green.

## What's stubbed / POC-only (dropped at `store=swift`, Phase 1b)

- **`requestId` / `createdAt`** in the written request are seq-derived / placeholder. Real
  UUID + timestamp need the generators that live in `State.elm`, threaded in a follow-up. The
  §4.1 JSON shape and §7.1 framing are exact and unit-tested.
- **Single-in-flight (§7.1):** a batch/select-all writes the request for the **first** target
  only; the rest sit `queued` until the host paces the next one (live pacing not yet wired).
- **`store=console`** manifest/result reads: `Discovery`/`Transport` cover `store=metadata`;
  the console path needs a new branch in the `ReceiveConsoleLog` handler (`State.elm:4339`
  currently discards the raw console text) — not yet added.
- **Opt-in persistence:** opt-in is per-session model state; persisting it per-instance in
  `localStorage` (§5.3) is not yet wired.
- **Size/count caps (§5.5)** beyond the renderer's structural decode (depth/node/row caps) are
  not yet enforced host-side.

## Needs Phase 1b (Jetstream2 / object storage)

- Object-storage manifest/request/result bodies (`store=swift`), CORS-via-proxy on JS2, and
  tightly-scoped app-cred `access_rules` — all deferred per Phase 0 §7. The metadata/console
  framing here is the throwaway layer; the manifest, catalog, message JSON, and lifecycle carry
  over unchanged.
