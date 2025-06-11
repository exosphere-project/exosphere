# ADR 7: Modularity of Exosphere Guest Agent

## Status

Accepted

## Context

- So far, Exosphere has used a mono-repo strategy: all components of the system (Elm app, Ansible code, etc.) in one Git repository, i.e. one GitLab project.
- There are benefits to the coupling that a mono-repo provides: introducing a new feature only requires one MR.
  - When Exosphere chose a mono-repo strategy, this resolved a pain point that maintainers previously had with a different project (Atmosphere), where making a feature change required separate coordinated PRs across 2-3 different projects.
  - Less administrative overhead, etc
- That said, there are benefits to the de-coupling that separate repos/projects provide.
  - Development can occur on different parts of the overall project at different paces.
  - Like we've found with the Ansible code, it could be useful outside of Exosphere
    - See example of somebody [forking Exosphere just to use our Ansible](https://github.com/MorphoCloud/exosphere/tree/morpho-cloud-portal-2024.07.17-78a7e2d93)
    - By making something that's useful outside Exosphere, we could grow the larger ecosystem
  - Testing and Deployment (CI/CD pipelines) are easier to implement and orchestrate
  - If clients other than Elm are implemented, would these still live under the Exosphere project?
    - A Rust client (with WASM support) is already generated within the codebase
  - What else?

- If decoupling, should ADRs specific to the Agent be delegated to the Agent's primary repository?

## Choices

### 1. Complete separation: Agent & Agent clients live in their own separate repositories
  
  - Requires orchestration of agent and client repositories to synchronize releases
  - Exosphere uses the client either through elm-git-install, a git submodule, or deployment to https://package.elm-lang.org
  - Testing clients (assuming future development of non-elm clients) can be isolated

### 2. Birepo: Agent & Agent clients live in a single repository outside of Exosphere

  - Agent and Client releases are inherently synchronized
  - Installing client code from git requires the entire repository, and support for using packages in a sub-path (elm-git-install does not support this)

### 3. Agent Client (Elm) lives in the Exosphere repository, but Agent lives in its own repository

  - Synchronizing Client development with the Agent is more onerous
  - Usage of the client outside of Exosphere is more difficult

### 4. Monorepo: Agent & Agent Client both live in Exosphere repository

  - Client code changes are updated live
  - Usage of the client outside of Exosphere is more difficult

### Additional questions

- If separated, do ADRs specific to the Agent reside in exosphere/docs/adr, or will they be delegated to the Agent repository?
- If separated, are issues unified in the primary Exosphere issue tracker, or kept separately

## Decision and Consequences

- Create a sub-group to house projects related specifically to the Guest Utilities
  - [exosphere/guest-utilities](https://gitlab.com/exosphere/guest-utilities)

- Move the current repositories under this new group
  - [exosphere/guest-utilities/guest-agent](https://gitlab.com/exosphere/guest-utilities/guest-agent)
  - [exosphere/guest-utilities/client-elm](https://gitlab.com/exosphere/guest-utilities/client-elm)
- For issues related to the guest utilities, use Exosphere's [main issue tracking system](https://gitlab.com/exosphere/exosphere/-/issues) (and a "guest-utilities" issue label). Disable the issue trackers in the guest utilities sub-projects.

- Delegate ADRs specific to the Guest Agent to the Agent repository
  - Create the documentation structure within the main guest-agent repository, with the README.md linking to the relevant ADRs in the Exosphere repository.
  - Add a note in the Exosphere ADR README.md linking to the guest-agent ADRs path

- Preliminary governance falls under the Exosphere project governance.
  - @julianpistorius, @cmart, and @kageurufu will have a maintainer role over the sub-group [exosphere/guest-utilities](https://gitlab.com/exosphere/guest-utilities)
  - Until the Agent is deploying on live user instances, merge requests may be merged by any single maintainer, and maintainers will be able to directly push to the repository.
  - After the Agent begins to be deployed, this will change to the Exosphere standard "1 Maintainer and 1 Contributor" rule.
