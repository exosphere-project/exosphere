# Extension UI (`exoext`) — the browser side

An instance in a researcher's own project can publish a UI manifest describing a small panel, and
Exosphere renders that panel on that instance's detail page. The panel is described in the
[json-render](https://github.com/vercel-labs/json-render) format and is validated fail-closed
against a fixed catalog of elements, so a publisher can only compose what this build already knows
how to draw.

Nothing here talks to a VM directly. Discovery, the manifest body, requests and results all travel
over OpenStack APIs that Exosphere already speaks: Nova server metadata for the small envelope and
object storage for anything large.

Everything is gated behind `context.experimentalFeaturesEnabled` and behind a per-instance opt-in,
so normal operation is unaffected.

## What's here

| File | Role |
|------|------|
| `../JsonRender.elm`, `../JsonRender/{Spec,Expr,Render}.elm` | The native Elm json-render renderer: decode a manifest against the catalog, resolve `$state` bindings, draw the tree, and emit effects. Fail-closed by construction. Depends only on `elm/core`, `elm/html`, `elm/json`. |
| `Discovery.elm` | The `exoext.v1.kind` sentinel in an instance's own metadata, manifest-body assembly from metadata chunks, and the eligibility filter behind an `$instances` binding (ACTIVE only, never the publisher itself). |
| `Transport.elm` | The wire envelope: the single request slot, chunk framing, the run status and cancel channels, the result pointer, and the size caps. It never knows what a request is for. |
| `Lifecycle.elm` | The extension-agnostic request and result-session state machines: one request in flight, run correlation, terminal states, and the opening / open / stale / failed decision for a result session. |
| `Health.elm` | The `exoext.v1.health.*` report a publisher can write, and the host chrome that reads it. |
| `Card.elm` | The adapter: the host for one card. Owns the renderer state, projects it for the renderer, turns renderer effects into requests, and draws the trust chrome (the provenance marker and the opt-in affordance) around the rendered manifest. |
| `Messages.elm` | The reference extension's own payloads: the bodies that go inside the envelope and the object names its results live at. A second extension replaces this module and reuses `Transport` unchanged. |
| `Reader.elm` | The read side of the same adapter: what the wire says, turned into what the card shows. |
| `CardStyle.elm`, `RendererStyle.elm` | The stylesheet the renderer emits, plus the card's own block of it. |

Tests live in `../../tests/Tests/Exoext/`. The generic host behavior is in `Host.elm` and
`Requests.elm`; the adapter's own behavior is in `Card.elm`, `ServerDetail.elm` and `Messages.elm`.

Integration point: `Page/ServerDetail.elm`, which adds the card tile and gains an `exoextCard`
field plus an `ExtensionCardMsg` branch.

## Run it locally

```sh
npm install
npm start
```

Then in the browser:

1. Settings, turn on experimental features. The card is gated on it and off by default.
2. Open the detail page of an instance that publishes a sentinel. The tile is titled "Extension"
   for every publisher: `kind` is publisher-controlled data and is never painted into Exosphere's
   own chrome.
3. The tile shows the opt-in affordance first. Enabling is remembered per instance and can be
   forgotten again from the same card or from Settings.
4. The card renders, with the provenance marker above it naming the instance that published it.

### Manual verification checklist

- [ ] The card is absent until experimental features are on and the instance is enabled.
- [ ] The provenance marker names the publishing instance and is host chrome, not manifest content.
- [ ] A confirmed action opens the confirm dialog the manifest declares, and on confirm the
      targeted rows flip to their queued state.
- [ ] With dev tools open, a confirmed action writes the request slot on the publishing instance's
      metadata rather than contacting the instance directly.
- [ ] A manifest using an element this build does not have renders the error notice, never a
      partial tree. `Tests.Exoext.Card` covers this.

## Trust model

- The manifest is decoded against a fixed catalog. An unknown element type, a bad prop, a dangling
  child reference or an oversized tree all fail the whole decode.
- A manifest may only place UI on the page of the instance that published it. An envelope naming
  another instance is dropped.
- Actions are declarative verbs resolved against a table the adapter owns. An action name that is
  neither a generic verb nor a known alias is ignored.
- Every action that writes is confirm-gated by a dialog the host draws.
- Approval is per instance UUID and persisted locally, so a different instance publishing the same
  name gets its own prompt.

## Known gaps

- `store=console` reads are not wired. Discovery and Transport cover metadata and object storage.
- Size and count caps beyond the renderer's structural decode are not enforced host-side.
- A batched request whose continuation is never claimed has no per-request expiry of its own.
