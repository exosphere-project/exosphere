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

For Exosphere this would mean:

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

- Good: The ASF is a well-established, well-respected, and well-funded organization with a proven track record of successfully incubating and managing open source projects.
- Bad: The ASF is a large, bureaucratic organization with a lot of overhead.
- Bad: We would have to relicense all past contributions to the codebase.
- Bad: We would have to move to a new code platform and CI/CD infrastructure.

### 3. Cloud Native Computing Foundation (CNCF) model

For Exosphere this would mean:

- Follow the [project proposal process](https://github.com/cncf/toc/blob/main/process/project_proposals.md)
- Adopt [IP policy](https://github.com/cncf/foundation/blob/main/charter.md#11-ip-policy), including:
  - Decide if we want to adopt a CLA
  - Ensure that all new code contributions are accompanied by a DCO (Developer Certificate of Origin sign-off) (https://developercertificate.org) and made under the Apache License, Version 2.0 (Note: Not clear if we would need to relicense existing contributions)
  - Transfer ownership of trademark and logo assets to the Linux Foundation
- Meet the [incubating stage criteria](https://github.com/cncf/toc/blob/main/process/graduation_criteria.md#incubating-stage), including:
  - Document that it is being used successfully in production by at least three independent direct adopters which, in the TOC’s judgement, are of adequate quality and scope. For the definition of an adopter, see https://github.com/cncf/toc/blob/main/FAQ.md#what-is-the-definition-of-an-adopter.
  - Have a healthy number of committers. A committer is defined as someone with the commit bit; i.e., someone who can accept contributions to some or all of the project.
  - Demonstrate a substantial ongoing flow of commits and merged contributions.
  - Since these metrics can vary significantly depending on the type, scope and size of a project, the TOC has final judgement over the level of activity that is adequate to meet these criteria
  - A clear versioning scheme.
  - Clearly documented security processes explaining how to report security issues to the project, and describing how the project provides updated releases or patches to resolve security vulnerabilities
- Meet the [graduation criteria](https://github.com/cncf/toc/blob/main/process/graduation-proposal-template.md#graduation-state-criteria), including:
  - Have committers from at least two organizations
  - Achieve and maintain a [Core Infrastructure Initiative Best Practices Badge](https://bestpractices.coreinfrastructure.org/)
  - Have completed an independent and third party security audit with results published of similar scope and quality as [this example](https://github.com/envoyproxy/envoy#security-audit) which includes all critical vulnerabilities and all critical vulnerabilities need to be addressed before graduation.
  - Explicitly define a project governance and committer process. The committer process should cover the full committer lifecycle including onboarding and offboarding or emeritus criteria. This preferably is laid out in a GOVERNANCE.md file and references an OWNERS.md file showing the current and emeritus committers.
  - Explicitly define the criteria, process and offboarding or emeritus conditions for project maintainers; or those who may interact with the CNCF on behalf of the project. The list of maintainers should be preferably be stored in a MAINTAINERS.md file and audited at a minimum of an annual cadence.

- Good: The CNCF is a well-established, well-respected, and well-funded organization with a proven track record of successfully incubating and managing open source projects.
- Good: The CNCF governance process is not as heavy as the ASF's.
- Good: We can continue to use GitLab and our existing CI/CD infrastructure.
- Bad: Unclear if Exosphere "fits" in the Cloud Native landscape.

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
