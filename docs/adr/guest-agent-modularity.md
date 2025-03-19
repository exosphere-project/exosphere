# ADR 7: Modularity of Exosphere Guest Agent

## Status

Draft

## Context

We are in the process of developing an Exosphere Guest Agent (referred to as 'guest agent', or 'agent' in the rest of the document.)
See [ADR 6: Exosphere Guest Utilities service on instances](guest-utilities.md) for more information.

Open questions:

1. What is an appropriate level of forward and backward compatibility between the Agent and the Agent Client, and how do we achieve it? (Group A)
2. How do we test the Agent and the Agent Client? (Group B)
3. How do we compile/build the Agent? (Group B)
4. How do we package the Agent and Agent Client? (Group C)
5. How do we distribute the Agent and Agent Client? (Group D, depends on Groups A, C)
6. Should we keep the Agent in its own repository, or should we bring it into the Exosphere application repository? (Group E)
7. Where do the Agent Client code bases live? (Group E)
8. Governance - If separate repositories for Agent and/or Agent Clients (Group F, depends on Group E)

### Group A: Guest Agent Compatibility

ADR Dependencies: None

#### Context

The Agent and Exosphere (i.e. Agent Client) are two pieces of software that communicate with each other. This forms at least an implicit API contract. We will encounter situations where:

- The Agent Client is newer than the Agent, because
  - An Exosphere user updated their Agent, whether using a newer Exosphere version or through system updates
  - An Exosphere user has multiple open copies of the app, and while one is newer, one may be older
- The Agent is newer than the Agent Client, possibly because
  - An Exosphere deployer is slow to update their deployment (JS on the server)
  - An Exosphere user has had the app open / not refreshed for a long time, and there is a client update

As new features are developed (and existing features are modified), there will be mismatched code on client and server side (i.e. agent client and agent side). When this happens, we want to gracefully degrade to the level of common/shared functionality when one end of the connection is older than the other. That said, backward/forward compatibility is not a must-have at all times. There may be cases where pursuing it is not worth the cost/effort.

#### Choices

TODO

- Develop with a strict "add-only" methodology.
  - New fields on existing method results must be ignored by older clients (easy)
  - Changing an existing field is a breaking change
  - New methods must be ignored by clients without support
  - Backwards incompatible changes could be a new method
  - e.g. implement `cephfs.mount_v2`, and either maintain `cephfs.mount` or change it to be a wrapper object
  - The OpenRPC schema has a "deprecated" flag for methods that could be used here
- Implement an API versioning system, such as that used by Stripe
  - Clients inform the server of their supported version, and the server accepts parameters and returns responses matching the requested version. I believe some of this could be magically handled by Rust's type system (`impl Into<MethodRequest> for VersionedMethodRequest<const Version>`, `impl Into<VersionedMethodResult<const VersionCode>> for MethodResult`)

- Note: Final decision and implementation can be deferred until needed

#### Decision and Consequences

TODO

- [frank] Suggestion:
  - All methods start life with an "unstable" or "experimental" tag
  - When an MR to Exosphere adding use of a unstable method is created, a companion MR to the Agent removing the tag is created
  - The companion MR should trigger a semver bump, and a release.
  - When we reach the point where a method may need to be deprecated, revisit this ADR to consider use of named `_v2` methods versus implementing the request versioning system


### Group B: Guest Agent Building & Testing

ADR Dependencies: None

#### Context

Exosphere is (so far) an Elm, Ansible, and Python project. The guest agent will be Rust code, which is new to us. We need to understand then build and test process.

- We must build against old libc versions, to ensure compatibility with a variety of older distributions. GLibc maintains backwards compatibility, e.g. applications built against 2.20 will still run on a distribution providing libc 2.36
  - This can be done in Docker, using old images such as `debian:10` which provides 2.28. CentOS 7 uses glibc 2.17, CentOS 8 uses 2.28
- Building on-host (during ansible setup) is possible, however would heavily impact instant startup


#### Choices

##### Building

- Build during a CI pipeline
  - [cross](https://github.com/cross-rs/cross) automates builds inside docker
  - Currently supports libc 2.17 for the x86_64-unknown-linux-gnu target using CentOS7

  - [cargo-zigbuild](https://github.com/rust-cross/cargo-zigbuild) can leverage zig to build against arbitrary libc versions
  - Pros:
    - Faster build times
  - Cons:
    - Adds dependency on `zig`
    - Managing C libraries is difficult, we currently link against openssl and pam

##### Testing

- Use native Rust test tooling for testing
  - [unit testing](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
  - [integration testing](https://doc.rust-lang.org/rust-by-example/testing/integration_testing.html)
  - [testcontainers](https://docs.rs/testcontainers/latest/testcontainers/) can run tests within docker containers
    - Need a container with systemd running, and a local user configured with a password
    - The Agent CI could build and publish this docker image, client CI jobs can depend on this container on gitlab-ci as a `service`
  - Instrumenting OpenStack to create an instance for running specific tests could also work

- Build-time generation of sample request / response data could be used for unit testing clients
  - OpenRPC specification should have defined examples anyway, this is already something on the roadmap
  - Needs some work on jsonrpsee-openrpc to allow `#[schemars(example = ...)]` on the method level

- Integration testing for clients should probably remain language specific
  - elm-test does not allow actual http calls, requires full-stack integration testing

#### Decision and Consequences

- [frank] Suggestion:
  - Builds on the CI pipeline
  - Unit tests where reasonable for rust code
  - Already using cargo-nextest, should add cargo-llvm-cov to the workflow
  - Integration tests initially using `testcontainers`
  - Launch an instance of the server, using the generated Rust client to interface with it
  - Add examples to all RPC types, which can be extracted (minimally using a bash script with `jq`) for client unit testing
  - Clients must implement unit tests against the current extracted examples
  - Clients should implement integration tests against the Agent

### Group C: Packaging Guest Agent

ADR Dependencies: None

#### Context

- People will install the Agent on different Linux distributions
- Exosphere itself specifies Ubuntu & flavors of Red Hat (requires systemd, and support for .deb & .rpm package files)
- This could be happen automatically (cloud-init & Ansible of Exosphere instance configuration) or manually on existing instances
- Using system packages will make managing dependencies (currently libpam) simple, as well as giving a simple removal process
  - `cargo-generate-rpm` and `cargo-deb` are two convenient cargo plugins which make generating packages relatively trivial, including adding relevant system files (udev rules, systemd services, etc).

#### Choices

- Generate .deb and .rpm packages on Gitlab Actions or similar, publish to repositories
  - May need a way for Exosphere to trigger a system package update before initial release
  ```json
  { "method": "packages.update", "params": {"packages": ["exosphere-guest-agent"]} }
  ```
- Generate .tar.gz, manually install during setup
  - [self_update](https://docs.rs/self_update/latest/self_update) can enable automatic updates from Gitlab Releases, or any S3-compatible object store
  - Easier initial

#### Decision and Consequences

- [frank] Suggestion:
  - Publish .deb and .rpm packages, leveraging the package manager to handle dependencies and updates
  - Methods wrapping the packagekit api could be easily implemented, System76 published [packagekit-zbus](https://docs.rs/packagekit-zbus/latest/packagekit_zbus/) giving us an easy type-safe api

### Group D: Distributing Guest Agent

ADR Dependencies: A, C

#### Context

- New instances need a way to install the guest agent.
- The Agent must be kept up to date on existing instances to ensure continued secure operation
- It's nice to not bring in more dependency services into our solution stack, but also, it's nice to use tools designed-for-purpose.

#### Choices

- If using system packages:
  - Distribution:
  - Host statically generated APT and RPM repositories.
    - Could be hosted on gitlab pages or anywhere static files can be served
    - yum uses `createrepo` to generate the `repodata` path
    - apt uses `dpkg-scanpackages` to generate the `Packages` metadata
    - Needs a lot of scripting work to automate updating the repositories
  - Self-host https://github.com/openkilt/openrepo
    - Requires managing a new hosted service
    - Far easier to deploy new packages (REST API)
  - Self-host https://pulpproject.org/
    - More involved than OpenRepo, but can provide more benefits long term
    - Supports Debian, Rpm, Containers, Ansible, arbitrary files, and more
  - [Package Cloud](https://packagecloud.io/); [TurboVNC](https://turbovnc.org) uses it
    - Deprecated, now redirects to https://buildkite.com/platform/package-registries/ which only offers paid plans
  - Google Cloud natively supports .deb and .rpm on the Artifact Registry, paid
    - https://cloud.google.com/artifact-registry/docs/os-packages/debian/store-apt
    - https://cloud.google.com/artifact-registry/docs/os-packages/rpm/store-rpm
  - Gitlab Packages
    - No support for RPM, would still need to manage repository metadata [Related Epic](https://gitlab.com/groups/gitlab-org/-/epics/5128)
    - [Debian support](https://docs.gitlab.com/user/packages/debian_repository/) is still considered an experiment [Related Epic](https://gitlab.com/groups/gitlab-org/-/epics/6057)

  - Installation:
  - Install using cloud-init directly
  ```cloud-config
  apt:
    sources:
      exosphere-guest-agent:
        source: deb http://repo.exosphere.app/apt/ main stable
        key: "ASCII PGP BLOCK"
  yum_repos:
    exosphere-guest-agent:
      baseurl: http://repo.exosphere.app/rpm/stable/$basearch
      gpgkey: http://repo.exosphere.app/ascii.key
      gpgcheck: true
  packages:
    - exosphere-guest-agent
  ```
    - Earlier in the setup process, if we want to leverage the Agent for more of instance setup
        - e.g. could monitor the ansible process, allowing Exosphere more introspection into the setup process
  - Install using Ansible
    - Matches existing setup process more closely

- If using .tar.gz
  - Distribution:
  - Gitlab Releases
    - Releases
  - Installation:
  - cloud-init could download and extract the .tar.gz, and ensure package dependencies
  - Ansible could do the same

#### Decision and Consequences

- [frank] Suggestion:
  - System packages, self-hosted using OpenRepo or Pulp, and installed through cloud-init
  - Packages will also be artifacts on gitlab-ci for testing, and published on gitlab releases
  - Client code should be published in a way that makes development versions easy to use

### Group E: Repositories for Guest Agent & Agent Clients

ADR Dependencies: None

#### Context

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

#### Choices

- Complete separation: Agent & Agent clients live in their own separate repositories
  - Requires orchestration of agent and client repositories to synchronize releases
  - Exosphere uses the client either through elm-git-install, a git submodule, or deployment to https://package.elm-lang.org
  - Testing clients (assuming future development of non-elm clients) can be isolated

- Birepo: Agent & Agent clients live in a single repository outside of Exosphere
  - Agent and Client releases are inherently synchronized
  - Installing client code from git requires the entire repository, and support for using packages in a sub-path (elm-git-install does not support this)

- Agent Client (Elm) lives in the Exosphere repository, but Agent lives in its own repository
  - Synchronizing Client development with the Agent is more onerous
  - Usage of the client outside of Exosphere is more difficult

- Monorepo: Agent & Agent Client both live in Exosphere repository
  - Client code changes are updated live
  - Usage of the client outside of Exosphere is more difficult

- If separated, do ADRs specific to the Agent reside in exosphere/docs/adr, or will they be delegated to the Agent repository?

#### Decision and Consequences

- [frank] Suggestion:
  - Complete separation will allow for the greatest flexibility, both in development and client usage.
  - This will enable elm-git-install or git submodules to be used in Exosphere, allowing easy integration against development versions
  - Client testing can be triggered from the Agent CI pipeline, passing the current build artifacts downstream
  See <https://docs.gitlab.com/ci/pipelines/downstream_pipelines/?tab=Multi-project+pipeline#fetch-artifacts-from-an-upstream-pipeline>
    - For unit testing, this can include the development `openrpc.json`
    - For integration testing, the debug or release binary

  - Create a subgroup under the Exosphere project
  https://gitlab.com/exosphere/guest-agent
  - Transfer <https://gitlab.com/kageurufu/exosphere-guest-agent> to https://gitlab.com/exosphere/guest-agent/agent (or guest-agent, exosphere-guest-agent, e.g.)
    - Copy over any relevant docs from the Exosphere project (code-of-conduct.md)
    - Write a contributing.md similar to the Exosphere document, but specific to Rust
  - Create https://gitlab.com/exosphere/guest-agent/client-elm, moving the initial code from <https://gitlab.com/kageurufu/exosphere-guest-agent/-/tree/client/elm/clients/elm?ref_type=heads>
  - Create MRs to the new guest-agent/agent
  - Agent-specific ADRs should probably be delegated to the primary Agent repository
  - This ADR and the following (Group E and F) would land in the Exosphere repository, specify said delegation, and link to the Agent docs/adr folder
  - Link to the initial adr/guest-utilities.md in the agent/docs/adr/readme.md
  - The other ADRs in this document would be at guest-agent/docs/adr/...


### Group F: Governance of Guest Agent and Agent Clients

ADR Dependencies: E

#### Context

From [What is open source project governance?](https://opensource.com/article/20/5/open-source-governance):

> In short, governance is the rules or customs by which projects decide who gets to do what or is supposed to do what, how they're supposed to do it, and when.
> ...
> When you define governance for a project, you need to identify the following five things:
>
> 1. What roles can contributors play in the project?
> 2. What qualifications, duties, privileges, and authority are associated with each role?
> 3. How do people get assigned to (and removed from) roles?
> 4. How can role definitions be changed?
> 5. What are the project's collected policies and procedures?

#### Choices

- Consider the Agent and Agent Client (collectively, "new components") as just part of the Exosphere project -- use the existing Exosphere governance and processes.
  - Good because: fewest decisions to make, we have something that works
- Consider the new components as a sub-project, still under the Exosphere governance umbrella.
  - Possibly, delegate maintainership of the new components to people different from the overall Exosphere maintainers.
  - Good because: may reduce maintenance burden on two busy people by spreading it to others
  - Bad because: more decisions to make now, need to expand Exosphere governance document to support the notion of sub-projects, perhaps similar to CNCF [GOVERNANCE-subprojects.md](https://github.com/cncf/project-template/blob/main/GOVERNANCE-subprojects.md)
- Consider the new components as a separate software project completely outside the Exosphere governance umbrella.
  - ??? wild west or insert governance here.

#### Decision and Consequences

- [frank] Suggestion:
  - Governance under the Exosphere project is simple and works.
  - I do however think ADRs specific to the Agent should live within the Agent repository though
