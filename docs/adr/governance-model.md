# ADR 5: Selecting a Governance Model for Exosphere

## Status

WIP

## Context

## Context

Exosphere is an open-source project that provides a user-friendly interface for non-proprietary cloud infrastructure. It empowers researchers and other non-IT professionals to deploy code and run services on OpenStack-based cloud systems, without requiring advanced virtualization or networking knowledge. Exosphere fills the gap between OpenStack interfaces built for system administrators and intuitive-but-proprietary services like DigitalOcean and Amazon Lightsail. It also enables cloud operators to deliver a friendly, powerful interface to their community with customized branding, nomenclature, and single sign-on integration.

As a community-driven project, Exosphere is committed to maintaining an open and inclusive governance model that encourages participation, guides discussions, builds consensus, and resolves conflicts. We currently operate under an [interim governance model](https://exosphere.app/docs/governance/), which has served us well so far. However, as the project grows and evolves, we recognize the need for a more formal and sustainable governance model.

In our pursuit of a new governance model, we have conducted a detailed investigation of several potential models, evaluating how each one addresses the governance acceptance criteria required by the [Open Source Security Foundation (OpenSSF) Best Practices](https://bestpractices.coreinfrastructure.org/en/criteria.ce) program, as well as the [Open Source Collective](https://docs.oscollective.org/getting-started/acceptance-criteria).

Our goal is to adopt a governance model that provides well-defined mechanisms for stakeholders to act in neutral forums, encourage participation, guide discussions, build consensus, and resolve conflicts. Ultimately, we aim to achieve [our sustainability goals](https://exosphere.app/docs/sustainability-goals/), ensuring that Exosphere remains responsive to the changing needs of its community, recruits and grows new project members, and maintains a secure, trustworthy open source product.

### Success Criteria

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

We have identified several potential governance models for Exosphere. Each of these models has its own pros and cons, which we have outlined below.

### 1. Interim Governance Model

We continue with our [interim governance model](https://exosphere.app/docs/governance/), evolving over time in response to community needs.

- Good: No change to the way we currently operate
- Bad: Not sure that the current model is sustainable in the long term
- Bad: Current model will likely prevent some corporations from adopting, promoting, and contributing to Exosphere 

### 2. Apache Software Foundation (ASF) model

#### Joining the ASF Incubator

For Exosphere, this would involve studying [the incubator cookbook](https://incubator.apache.org/cookbook/) in detail to decide if the ASF is a good fit, then going through the steps to join the [ASF Incubator](https://incubator.apache.org/).

#### Transferring the Exosphere Codebase

We would need to execute a formal [Software Grant Agreement (SGA)](https://www.apache.org/licenses/contributor-agreements.html#grants) to transfer the Exosphere codebase to the ASF.

#### Adopting the Apache License 2.0

This would involve adopting the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0). We would have to contact all past & present contributors to get their explicit permission to relicense their contributions. If we can't get permission from everyone, we'll have to remove their contributions from the codebase.

#### Adopting the Apache Contributor License Agreement (CLA)

We would also need to adopt the [Apache Contributor License Agreement (CLA)](https://www.apache.org/licenses/#clas).

#### Adopting the Apache Way

This would involve adopting the [Apache Way](https://www.apache.org/theapacheway/), specifically:
- Setting up a project management committee (PMC) to oversee the project, which would involve board meetings, reporting, etc.
- Ensuring that all technical decisions and the great majority of the work take place on public mailing lists. Decisions SHALL NOT be made in other media, like IRC, Slack channels, face to face at conferences, nor presumably on GitLab issues or MRs.

#### Moving to an ASF-blessed Code Platform

We would need to move to an ASF-blessed code platform (GitHub or Apache's GitBox) and use Apache's CI/CD infrastructure.

**Pros & Cons**

- Good: The ASF is a well-established, well-respected, and well-funded organization with a proven track record of successfully incubating and managing open source projects
- Good: More appealing for corporations to adopt, promote, and contribute to Exosphere
- Bad: The ASF is a large, bureaucratic organization with a lot of overhead
- Bad: We would have to relicense all past contributions to the codebase
- Bad: We would have to move to a new code platform and CI/CD infrastructure

### 3. Cloud Native Computing Foundation (CNCF) model

#### Following the Project Proposal Process

For Exosphere, this would mean following the [project proposal process](https://github.com/cncf/toc/blob/main/process/project_proposals.md).

#### Adopting IP Policy

This would involve adopting the CNCF's [IP policy](https://github.com/cncf/foundation/blob/main/charter.md#11-ip-policy), which includes deciding if we want to adopt a CLA, ensuring that all new code contributions are accompanied by a DCO (Developer Certificate of Origin sign-off) and made under the Apache License, Version 2.0, and transferring ownership of trademark and logo assets to the Linux Foundation.

#### Meeting the Incubating Stage Criteria

We would need to meet the [incubating stage criteria](https://github.com/cncf/toc/blob/main/process/graduation_criteria.md#incubating-stage), which includes demonstrating successful production usage by at least three independent adopters, having a healthy number of committers, demonstrating a substantial ongoing flow of commits and merged contributions, and having a clear versioning scheme.

#### Meeting the Graduation Criteria

Finally, we would need to meet the [graduation criteria](https://github.com/cncf/toc/blob/main/process/graduation-proposal-template.md#graduation-state-criteria), which includes having committers from at least two organizations, achieving and maintaining a Core Infrastructure Initiative Best Practices Badge, and having completed an independent and third party security audit with results published.

**Pros & Cons**

- Good: The CNCF is a well-established, well-respected, and well-funded organization with a proven track record of successfully incubating and managing open source projects
- Good: The CNCF governance process is more modern, and not as heavy as the ASF's
- Good: We can continue to use GitLab and our existing CI/CD infrastructure
- Good: More appealing for corporations to adopt, promote, and contribute to Exosphere
- Bad: Unclear if Exosphere "fits" in the Cloud Native landscape

### 4. Open Infrastructure Foundation model

#### Determining Project Fit

For Exosphere, we would first need to determine if we fit best as a project under the wider Open Infrastructure umbrella, and not OpenStack specifically.

#### Working on a Governance Process

We would need to work with the OpenInfra Foundation on a governance process, as each Open Infrastructure Project is governed separately by procedures approved by the Board of Directors according to the [bylaws](https://openinfra.dev/legal/bylaws).

#### Adopting the OpenInfra Contributor License Agreement (CLA) and Apache 2.0 License

This would involve adopting the [OpenInfra Contributor License Agreement (CLA)](https://openinfra.dev/cla/) and Apache 2.0 License. However, we _might_ be able to avoid relicensing to Apache 2.0 based on [Bylaws Article VII. Intellectual Property Policy, 7.1 (c)](https://openinfra.dev/legal/bylaws).

#### Migrating to OpenDev

We would likely need to migrate to [OpenDev](https://opendev.org/) for code hosting, code review, and CI/CD (Gitea, Gerrit, and Zuul respectively), even though in theory this is negotiable.

#### Using Mailing Lists and IRC

We would need to use mailing lists and IRC as the main means of communication and decision making.

**Pros & Cons**

- Good: The OpenInfra Foundation is a well-established, well-respected, and well-funded organization with a proven track record of successfully managing open source projects
- Good: Exosphere could be a good fit, because it both builds on, and enhances the appeal of OpenStack
- Good: More appealing for corporations to adopt, promote, and contribute to Exosphere
- Bad: Not many examples of successfully incubating non-OpenStack projects
- Bad: We would probably have to move to a new code platform and CI/CD infrastructure

### 5. C4 (Collective Code Construction Contract) model

For Exosphere this would mean mostly business as usual, with some notable exceptions below:

- Use a share-alike license such as the MPLv2, or a GPLv3 variant thereof (GPL, LGPL, AGPL), unless we can modify C4 to be compatible with the BSD 3-Clause license (see below)
  - There is no copyright assignment or CLA, and all patches are owned by their authors, and shall use the same license as the project
  - Unknown: Is this compatible with the BSD 3-Clause license?
- A patch (merge request) SHOULD be a minimal and accurate answer to exactly one identified and agreed problem
- The release history of the project SHALL be a list of meaningful issues logged and solved
- Standardize formatting of commit messages
- Maintainers SHALL NOT make value judgments on correct patches
- Maintainers SHALL merge correct patches from other Contributors rapidly
- Any Contributor who has value judgments on a patch SHOULD express these via their own patches
- Maintainers SHOULD close user issues that are left open without action for an uncomfortable period of time

**Pros & Cons**

- Good: Very simple and lightweight governance process ([one short page](https://rfc.zeromq.org/spec/42/))
- Good: Goals are closely aligned with our requirements
- Good: Workflow is very close to what we are used to
- Good and Bad: No organization to join, this is self-governance. So no benefits of being part of a larger organization, but also none of the downsides.
- Unknown: Not sure if this is compatible with the BSD 3-Clause license
- Unknown: Because we are used to tighter control over the codebase, the policy of no value judgments and rapid merging of correct patches might require some adjustment and cause conflict in the short term. It could be beneficial in the long term if it pays off in more contributions and more contributors, as long as we can ensure that the quality of the codebase remains high.
- Bad: Less appealing for corporations to adopt, promote, and contribute to Exosphere

### 6. Something else?

TODO

## Decision

TODO

## Consequences

TODO
