# CloudShield × Exosphere Integration — Review Brief

> **Purpose & reviewer instructions.** This document captures a problem, the facts we
> gathered from reading both codebases, our analysis, and our *tentative* conclusions.
> It is meant as input for an **independent review**.
>
> Please analyze the problem **from scratch**. Treat the "Solution space" and "Our
> current leanings" sections as one team's working notes, **not** as constraints or as
> the answer. We explicitly want you to:
> - challenge our assumptions and find factual errors (we cite files so you can verify),
> - identify options or OpenStack/CloudShield primitives we missed,
> - flag security, reliability, cost, or operational problems we underweighted,
> - tell us if our framing of the "core tension" is wrong.
>
> Do not assume our preferred direction is correct. If a completely different approach is
> better, say so.

---

## 1. The general problem

We run (or have access to) two systems:

- **Exosphere** — an Elm, browser-only client for OpenStack clouds (we operate it on
  Jetstream2). It is a static single-page app: no application backend of its own.
- **CloudShield** — a multi-scanner security product (SAST + dependency CVEs + optional
  LLM remediation) that scans a target host over SSH and presents ranked findings in a
  dashboard, and can embed an instance-scoped results panel into a partner UI via iframe.

**Goal.** From inside Exosphere, let a user trigger (on-demand and on a schedule) a
vulnerability review of one of their cloud instances, and see the results in Exosphere.
The intended scan method is **snapshot-clone**: snapshot the instance, boot a throwaway
isolated VM from the snapshot, scan that clone over SSH, then delete it (so production is
untouched).

**Hard constraints given to us:**
1. **Exosphere must stay 100% static** for these features — *no server may be added on
   the Exosphere side.* (Adding Elm code is fine; it's still a static SPA.)
2. **We should not edit the CloudShield codebase** if avoidable. Editing it makes us
   de-facto maintainers of someone else's product. If a CloudShield change is truly
   required, we want it stated precisely so we can ask their team.
3. Servers **are** allowed on the OpenStack / cloud side (CloudShield itself already runs
   in the cloud). The OpenStack deployment provides **S3-compatible / Swift object
   storage** and the usual OpenStack services.
4. A secondary goal: use this as the first example of a **general "extension" mechanism**
   for Exosphere (so future third-party panels can be added, ideally via config).

---

## 2. What we found (verified from the code)

### 2.1 CloudShield (repo: `cloudshield`)

- **Stack.** Next.js app + Postgres (Prisma, `@prisma/adapter-pg`) + Redis/Valkey
  (`ioredis`) + Clerk (auth) + Sentry (optional). `next.config.ts` sets
  `output: "standalone"` → **not Vercel-locked**; Redis client is generic → **not
  Upstash-locked**; Postgres is generic. The Python scanner worker already runs on a
  Jetstream instance (Docker).
- **Scan pipeline.** Browser → enqueue job to Redis (`cloudshield:scan-queue`). SSH
  credentials are encrypted (AES-256-GCM), stored in Redis with a **30-min TTL** and
  claimed **single-use** (atomic GETDEL). The Python worker `BLMOVE`s the job, claims
  creds, SSHes the target, rsyncs source + collects packages, runs 8 scanners
  (semgrep, snyk_code, bandit, gosec, brakeman, cppcheck, flawfinder, trivy), normalizes
  + dedups, applies a heuristic false-positive filter (`v12.1`, ~25 rules), and POSTs
  findings back via an internal API authed by an `X-Worker-Secret` header. *(SSH usage on
  the target is read-only in practice: cat/find/list + rsync pull; no remote writes.)*
- **Embed control plane (the key piece).** Designed **server-to-server**:
  - A partner holds a long-lived secret `cs_embed_…` (stored as SHA-256 hash on the
    CloudShield side).
  - The partner's **backend** calls `POST /api/v1/embed-sessions` with that secret and
    instance context; CloudShield returns a **short-lived HMAC JWT (~15 min)** plus an
    iframe URL `/embed/instance#token=…`.
  - Embed APIs are authed by that bearer JWT; capabilities are intersected; origins are
    allow-listed.
  - **External users are not CloudShield accounts** — they are recorded as
    `ExternalActor` rows under the partner's single organization.
  - There is **no browser-facing auth path** to mint a session: minting requires the
    secret, which is meant to stay on a server. Docs: `docs/embedded-control-plane.md`.
- **Inference / LLM remediation.** A separate worker calls **vLLM** (OpenAI-compatible)
  on a **GPU** (A100), serving **local open-weight models** (Qwen3-27B, Gemma) from disk.
  This is **self-hosted inference, not an external AI API.** Remediation is **optional**
  (`ScanConfig.remediationEnabled` defaults false). The LLM call has **no tools** — it
  cannot act, only return text (relevant to prompt-injection risk, since it ingests
  attacker-controllable code snippets).
- **Public API keys (`cs_live_…`).** The `ApiKey` model exists in the schema but is
  **completely unimplemented** (no routes, no auth middleware, no UI). So "give each user
  an API token" is not currently a supported path.

### 2.2 Exosphere (repo: `exosphere`)

- **Architecture.** Elm SPA, browser-only. Talks **directly to OpenStack APIs** through a
  **CORS proxy** (one of the "two small proxies" the project mentions; the other proxies
  interactive instance access). Persists state (incl. tokens) in **localStorage**.
  Per-deployment config is injected via `config.js` / `cloud_configs.js` and decoded into
  a `CloudSpecificConfig` per cloud — **configurable without recompiling**.
- **OpenStack capabilities it already has:**
  - **Application Credentials** (Keystone) — created and used for auth
    (`src/Rest/Keystone.elm`). This is OpenStack's revocable, scoped delegation primitive.
  - **Create server**, **create image from a server** (= snapshot, `requestCreateServerImage`
    in `src/Rest/Nova.elm`), **keypairs** + **cloud-init `user_data`** injection.
  - **Console-log read** via Nova (`src/OpenStack/ConsoleLog.elm`), already parsed for
    charts / setup status. **MR !1128 ("MOTD")** uses Ansible to run a Python script *on
    the instance*, which writes JSON to the console log; Exosphere reads that log and
    renders it. This is an existing **serverless data-passing pattern**: compute on the
    instance → write to an OpenStack-native channel → browser reads via OpenStack API.
- **What it lacks:**
  - **No Swift / S3 object-storage support** (absent today; would be new Elm code).
  - **No iframe embedding** anywhere today (would be a new pattern).
  - **No Barbican** (secret-store) support.

---

## 3. Our analysis of the core tension

A secure, *serverless-on-the-Exosphere-side* embed runs into three structural problems:

1. **The embed secret cannot live in a browser.** CloudShield's embed model requires a
   non-browser "front desk" to hold `cs_embed_…` and mint short-lived passes. A static
   SPA shipped to all users has nowhere safe to keep that secret. (We believe storing it
   in Barbican does **not** solve this: whatever reads it back to use it must itself be a
   non-browser process; Barbican relocates the secret, it doesn't remove the need for a
   front desk. **Please challenge this if wrong.**)
2. **Scheduling needs a clock.** A static SPA only runs while a tab is open, so it cannot
   perform truly unattended scheduled scans. Something always-on must own the schedule.
3. **Unattended snapshot/boot needs delegated cloud credentials**, and **inference needs
   a GPU** — both are real, persistent, server-side resources.

The constraint "no Exosphere server" is satisfiable **only if** the always-on pieces live
on the OpenStack/CloudShield side (which is permitted). The open question is *which*
always-on component plays each role, and whether that forces a CloudShield change.

---

## 4. Solution space (options we identified — presented neutrally)

For each: what changes, and what it requires to work. (We are not asserting one is right.)

- **Option A — Use CloudShield's hosted service + iframe.**
  Requires CloudShield's team to add a **browser-presentable auth path** (e.g. accept a
  user's **Keystone token**, validate it server-side, mint the embed pass) **plus CORS**
  for the Exosphere origin. No infra or secret on our side; richest UI. Cost: depends on
  another team's roadmap. Also: network reachability of their hosted scanner worker to a
  Jetstream clone is unverified.

- **Option B — Self-host CloudShield on Jetstream + a small "sidecar" front-desk.**
  We run CloudShield ourselves; a tiny separate service (the sidecar — a process next to
  CloudShield, not necessarily its own VM) holds the embed secret, validates the Keystone
  token, and mints the pass. **No edits to CloudShield's code.** Cost: we operate the full
  stack (Postgres, Redis, scanner worker, **GPU for inference**, Clerk dependency).

- **Option C — Self-host a modified fork** with the Keystone-token bridge built into
  CloudShield itself (no sidecar). Cost: we maintain a fork (rebases) + operate the stack
  + GPU.

- **Option D — "Native tile" (MOTD-style), no iframe / no secret.**
  The scan writes a **results summary** to an OpenStack-native channel (object storage, or
  the clone's console log); Exosphere reads it and renders a **native tile**. **Zero
  CloudShield changes, zero secrets, fully serverless today.** Cost: summary only — not
  CloudShield's interactive UI; no remediation unless inference is added somewhere.

Cross-cutting facts that apply across options:
- **Display modes:** native tile (no auth) vs iframe (needs the pass).
- **Scheduling:** either an always-on scheduler (CloudShield-side cron, or an
  OpenStack-native scheduler if one exists) or "on-demand / when-Exosphere-is-open" only.
- **A secret that legitimately needs storage (e.g. Barbican):** a delegated **Application
  Credential** for unattended snapshot/boot — *not* the CloudShield embed secret.
- **Inference/GPU:** theirs (A), ours (B/C), or none/optional (D).

A phased combination is possible (e.g. ship D first, add B/A later for the rich panel).

---

## 5. Our current leanings (transparency only — please don't anchor on this)

- We lean toward **D as a first deliverable** (it ships with no dependencies and proves
  the UX and the extension mechanism), then **B** for the rich interactive panel once we
  accept operating the stack — with **A** as the lighter path *if* CloudShield's team is
  willing to add the Keystone-token endpoint quickly.
- We believe Application Credentials + (encrypted-in-object-storage or Barbican) is the
  right home for the unattended-orchestration secret.

These are working hypotheses, not conclusions we're confident in.

---

## 6. Open questions we want the reviewer to address

1. Is the "secret cannot live in the browser" framing correct, and is there genuinely no
   secure way to embed CloudShield from a static SPA **without** a CloudShield change?
2. Are there **OpenStack-native primitives** we overlooked that change the picture — e.g.
   Mistral (workflow/scheduler), Zun, Heat, Swift **TempURL / presigned** objects,
   Barbican, application-credential `access_rules`, object-storage CORS?
3. Is **snapshot-clone** the right isolation model vs. read-only in-place scanning?
   Consider quota/cost, time, and network-isolation requirements for the clone.
4. For **Option A's** hosted scanner reaching a Jetstream clone — what's the network/
   firewall/floating-IP story, and does it break the model?
5. **Security:** prompt-injection into the LLM (snippets are attacker-controllable);
   data-egress if inference is pointed at an external endpoint; token/credential handling;
   the clone's blast radius.
6. **Scheduling** with zero Exosphere server: what's the cleanest owner of the clock, and
   what are the failure modes?
7. Is the **MOTD/console-log pattern** (or object storage) a sound transport for richer
   results, or does it have limits (size, latency, structure) that make the iframe
   unavoidable for anything beyond a summary?
8. For the **extensibility goal**: what's a good, minimal, config-driven extension model
   for Exosphere (iframe panels and/or native tiles) that doesn't bloat the core?
9. What did we get **factually wrong** about either codebase?

---

## 7. How to verify our claims (key files)

**CloudShield**
- Embed model & API: `docs/embedded-control-plane.md`,
  `app/api/v1/embed-sessions/route.ts`, `app/api/embed/**`, `app/lib/embed-*.ts`,
  `app/lib/services/embed-*.ts`
- Auth (dashboard): `app/lib/auth.ts`, `proxy.ts` (Clerk middleware)
- Scan pipeline / creds: `app/lib/services/{scan,credential}-service.ts`,
  `app/lib/queue.ts`, `worker/` (`job_consumer.py`, `scan_runner.py`, `ssh/`, `scanners/`)
- Inference: `remediation-worker/` (`runner.py`, `llm/inference.py`, `llm/prompt.py`),
  `docker-compose.yml` (`cloudshield-vllm` service)
- API keys (unimplemented): `prisma/schema.prisma` (`ApiKey` model) — no routes use it
- Deploy independence: `next.config.ts` (`output: "standalone"`), `app/lib/valkey.ts`

**Exosphere**
- Auth / app credentials: `src/Rest/Keystone.elm`, `src/Helpers/Credentials.elm`
- Snapshot / server create / keypairs / cloud-init: `src/Rest/Nova.elm`,
  `src/ServerDeploy.elm`
- Console-log channel: `src/OpenStack/ConsoleLog.elm`, `src/Helpers/Helpers.elm`;
  MOTD pattern: GitLab MR !1128 (Ansible script → console log → parsed in UI)
- Config injection / per-cloud config: `config.js`, `cloud_configs.js`,
  `src/Types/Flags.elm`, `src/Types/HelperTypes.elm` (`CloudSpecificConfig`),
  `src/State/Init.elm`
- No Swift / no iframe / no Barbican today (absence — confirm by searching `src/`)

**Companion diagram:** `cloudshield-integration-options.excalidraw` (per-option wiring,
boxes = processes, arrows = calls; red dashed = needs a CloudShield change).
