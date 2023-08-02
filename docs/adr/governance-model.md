# ADR 5: Governance model

## Status

WIP

## Context

Exosphere is a community-driven open source project.

We have an [interim governance model](https://exosphere.app/docs/governance/). From [!750 Documenting interim governance for Exosphere](https://gitlab.com/exosphere/exosphere/-/merge_requests/750):

> #815 tracks the effort to investigate potential governance models for Exosphere. In that issue, @julianpistorius linked to the CNCF [project template](https://github.com/cncf/project-template), specifically the [simple maintainer governance](https://github.com/cncf/project-template/blob/main/GOVERNANCE-maintainer.md).
>
> We both agreed that the template was somewhat close to how the Exosphere project has already been governed since its inception, and short enough to understand and modify with ease. So, we removed the parts that didn't already apply, and tweaked the parts which do.
>
> This doesn't solve #815 or #819 -- it just serves to capture approximately what we already have, as a starting point to evolve it.

In [POSE 3.2.1: Investigate governance models #815](https://gitlab.com/exosphere/exosphere/-/issues/815#note_1404718326) we have a fairly detailed investigation of four governance models. Specifically we evaluated how/if each model addresses governance acceptance criteria required by the [Open Source Security Foundation (OpenSSF) Best Practices](https://bestpractices.coreinfrastructure.org/en/criteria.ce) program, as well as the [Open Source Collective](https://docs.oscollective.org/getting-started/acceptance-criteria).

### What would success / a fix look like?

From our POSE Phase I proposal, a governance model that provides well-defined mechanisms for stakeholders to:

1. Act in neutral forums
2. Encourage participation
3. Guide discussions
4. Build consensus
5. Resolve conflicts

That results in us achieving [our sustainability goals](https://exosphere.app/docs/sustainability-goals/):

> - Is responsive to changing needs of its community
> - Has maintainers who set strategic direction to meet community needs, and ensure the long-term health of the project
> - Has project members regularly engaged and available to respond to community requests
> - Recruits new project members to compensate for attrition
> - Grows existing project members toward becoming maintainers
> - Maintains a secure, trustworthy open source product

## Choices

### 1. Status quo

We continue with our [interim governance model](https://exosphere.app/docs/governance/), evolving over time in response to community needs.

TODO: 

- What are the pros and cons of this choice?
- Does this meet our governance acceptance criteria?

### 2. Apache Software Foundation (ASF) model

In Exosphere this would mean:

- Studying [the incubator cookbook](https://incubator.apache.org/cookbook/) in detail to decide if the ASF is a good fit, then going through the steps to join the [ASF Incubator](https://incubator.apache.org/).
- Execute a formal [Software Grant Agreement (SGA)](https://www.apache.org/licenses/contributor-agreements.html#grants) to transfer the Exosphere codebase to the ASF
- Adopting the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0)
  - We will have to contact all past & present contributors to get their explicit permission to relicense their contributions. If we can't get permission from everyone, we'll have to remove their contributions from the codebase.
- Adopting the [Apache Contributor License Agreement (CLA)](https://www.apache.org/licenses/#clas)
- Adopting the [Apache Way](https://www.apache.org/theapacheway/), specifically:
    - Set up a project management committee (PMC) to oversee the project, which would involve board meetings, reporting, etc.
    - All technical decisions and the great majority of the work should take place on public mailing lists. Decisions SHALL NOT be made in other media, like IRC, Slack channels, face to face at conferences, nor presumably on GitLab issues or MRs.
- Move to an ASF-blessed code platform (GitHub or Apache's GitBox) and use Apache's CI/CD infrastructure
- ...and more

- Good: The ASF is a well-established, well-respected, and well-funded organization with a proven track record of successfully incubating and managing open source projects. The ASF has a well-defined governance model that addresses all of our governance acceptance criteria.
- Bad: The ASF is a large, bureaucratic organization with a lot of overhead. It's not clear that the ASF is a good fit for Exosphere, and it's not clear that we would be accepted into the ASF Incubator.

### 3. Cloud Native Computing Foundation (CNCF) model

TODO

### 4. Open Infrastructure Foundation model

TODO

### 5. C4 (Collective Code Construction Contract) model

TODO

### 6. Something else?

TODO

## Decision

TODO

## Consequences

TODO
