# ADR 5: Selecting a Governance Model for Exosphere

## Status

WIP

## Context

Exosphere is an open-source project that provides a user-friendly interface for non-proprietary cloud infrastructure. It
empowers researchers and other non-IT professionals to deploy code and run services on OpenStack-based cloud systems,
without requiring advanced virtualization or networking knowledge. Exosphere fills the gap between OpenStack interfaces
built for system administrators and intuitive-but-proprietary services like DigitalOcean and Amazon Lightsail. It also
enables cloud operators to deliver a friendly, powerful interface to their community with customized branding,
nomenclature, and single sign-on integration.

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

Before selecting a governance model, we first formalized and documented our existing governance in 
an [interim governance document](https://exosphere.app/docs/governance/), based on a [CNCF template for maintainer-governed projects](https://github.com/cncf/project-template/blob/main/GOVERNANCE-maintainer.md).

In our pursuit of a suitable governance model, we have conducted a detailed investigation of several potential models. We evaluated how each one addresses the governance acceptance criteria required by
the [Open Source Security Foundation (OpenSSF) Best Practices](https://bestpractices.coreinfrastructure.org/en/criteria.ce)
program, as well as the [Open Source Collective](https://docs.oscollective.org/getting-started/acceptance-criteria).

### Success Criteria

From our POSE Phase I proposal, we will adopt a governance model that provides well-defined mechanisms for stakeholders to:

1. Act in neutral forums
2. Encourage participation
3. Guide discussions
4. Build consensus
5. Resolve conflicts

Ultimately we aim to achieve [our sustainability goals](https://exosphere.app/docs/sustainability-goals/):

> - Is responsive to changing needs of its community
> - Has maintainers who set strategic direction to meet community needs, and ensure the long-term health of the project
> - Has project members regularly engaged and available to respond to community requests
> - Recruits new project members to compensate for attrition
> - Grows existing project members toward becoming maintainers
> - Maintains a secure, trustworthy open source product

## Choices

We have identified several potential governance models for Exosphere. Each of these models has its own pros and cons,
which we have outlined below.

### 1. Interim Governance Model

We continue with our [interim governance model](https://exosphere.app/docs/governance/), evolving over time in response
to community needs.

**Pros & Cons**

- Pro: No change to the way we currently operate, which would minimize disruption and maintain continuity.

### 2. Apache Software Foundation (ASF)

#### Joining the ASF Incubator

For Exosphere, this would involve studying [the incubator cookbook](https://incubator.apache.org/cookbook/) in detail to
decide if the ASF is a good fit, then going through the steps to join
the [ASF Incubator](https://incubator.apache.org/).

#### Transferring the Exosphere Codebase

We would need to execute a
formal [Software Grant Agreement (SGA)](https://www.apache.org/licenses/contributor-agreements.html#grants) to transfer
the Exosphere codebase to the ASF.

#### Adopting the Apache License 2.0

This would involve adopting the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0). We would have to
contact all past & present contributors to get their explicit permission to relicense their contributions. If we can't
get permission from everyone, we'll have to remove their contributions from the codebase.

#### Adopting the Apache Contributor License Agreement (CLA)

We would also need to adopt the [Apache Contributor License Agreement (CLA)](https://www.apache.org/licenses/#clas).

#### Adopting the Apache Way

This would involve adopting the [Apache Way](https://www.apache.org/theapacheway/), specifically:

- Setting up a project management committee (PMC) to oversee the project, which would involve board meetings, reporting,
  etc.
- Ensuring that all technical decisions and the great majority of the work take place on public mailing lists. Decisions
  SHALL NOT be made in other media, like IRC, Slack channels, face to face at conferences, nor presumably on GitLab
  issues or MRs.

#### Moving to an ASF-blessed Code Platform

We would need to move to an ASF-blessed code platform (GitHub or Apache's GitBox) and use Apache's CI/CD infrastructure.

**Pros & Cons**

- Pro: The ASF is a well-established, well-respected, and well-funded organization with a proven track record of
  successfully incubating and managing open source projects.
- Pro: Joining the ASF could make Exosphere more appealing to corporations, potentially expanding our project's reach and
  impact.
- Mixed: The ASF is a large organization with a lot of overhead, which could provide valuable resources and support but
  also introduce bureaucracy and slow down decision-making.
- Con: We would have to relicense all past contributions to the codebase, which could be a complex and time-consuming
  process.
- Con: We would have to move to a new code platform and CI/CD infrastructure, which could disrupt our current workflows
  and require additional resources to implement.

### 3. Cloud Native Computing Foundation (CNCF)

#### Following the Project Proposal Process

For Exosphere, this would mean following
the [project proposal process](https://github.com/cncf/toc/blob/main/process/project_proposals.md).

#### Adopting IP Policy

This would involve adopting the
CNCF's [IP policy](https://github.com/cncf/foundation/blob/main/charter.md#11-ip-policy), which includes deciding if we
want to adopt a CLA, ensuring that all new code contributions are accompanied by a DCO (Developer Certificate of Origin
sign-off) and made under the Apache License, Version 2.0, and transferring ownership of trademark and logo assets to the
Linux Foundation.

#### Meeting the Incubating Stage Criteria

We would need to meet
the [incubating stage criteria](https://github.com/cncf/toc/blob/main/process/graduation_criteria.md#incubating-stage),
which includes demonstrating successful production usage by at least three independent adopters, having a healthy number
of committers, demonstrating a substantial ongoing flow of commits and merged contributions, and having a clear
versioning scheme.

#### Meeting the Graduation Criteria

Finally, we would need to meet
the [graduation criteria](https://github.com/cncf/toc/blob/main/process/graduation-proposal-template.md#graduation-state-criteria),
which includes having committers from at least two organizations, achieving and maintaining a Core Infrastructure
Initiative Best Practices Badge, and having completed an independent and third party security audit with results
published.

**Pros & Cons**

- Pro: The CNCF is a well-established, well-respected, and well-funded organization with a proven track record of
  successfully incubating and managing open source projects.
- Pro: The CNCF governance process is more modern and less bureaucratic than the ASF's, which could make it a better fit
  for our project's culture and values.
- Pro: We can continue to use GitLab and our existing CI/CD infrastructure, which could minimize disruption and maintain
  continuity.
- Pro: Joining the CNCF could make Exosphere more appealing to corporations, potentially expanding our project's reach and
  impact.
- Con: It's unclear whether Exosphere fits within the Cloud Native landscape, which could complicate our application
  process and limit the benefits we receive from joining the CNCF.

### 4. Open Infrastructure Foundation

#### Determining Project Fit

For Exosphere, we would first need to determine if we fit best as a project under the wider Open Infrastructure
umbrella, and not OpenStack specifically.

#### Working on a Governance Process

We would need to work with the OpenInfra Foundation on a governance process, as each Open Infrastructure Project is
governed separately by procedures approved by the Board of Directors according to
the [bylaws](https://openinfra.dev/legal/bylaws).

#### Adopting the OpenInfra Contributor License Agreement (CLA) and Apache 2.0 License

This would involve adopting the [OpenInfra Contributor License Agreement (CLA)](https://openinfra.dev/cla/) and Apache
2.0 License. However, we _might_ be able to avoid relicensing to Apache 2.0 based
on [Bylaws Article VII. Intellectual Property Policy, 7.1 (c)](https://openinfra.dev/legal/bylaws).

#### Migrating to OpenDev

We would likely need to migrate to [OpenDev](https://opendev.org/) for code hosting, code review, and CI/CD (Gitea,
Gerrit, and Zuul respectively), even though in theory this is negotiable.

#### Using Mailing Lists and IRC

We would need to use mailing lists and IRC as the main means of communication and decision making.

**Pros & Cons**

- Pro: The OpenInfra Foundation is a well-established, well-respected, and well-funded organization with a proven track
  record of successfully managing open source projects.
- Pro: Exosphere could be a good fit for the OpenInfra Foundation, as it both builds on and enhances the appeal of
  OpenStack.
- Pro: Joining the OpenInfra Foundation could make Exosphere more appealing to corporations (especially those already using
  OpenStack), potentially expanding our project's reach and impact.
- Mixed: There are not many examples of the OpenInfra Foundation successfully incubating non-OpenStack projects, which
  could make it harder for us to predict and navigate the incubation process.
- Con: We might have to move to a new code platform and CI/CD infrastructure, which could disrupt our current
  workflows and require additional resources to implement.

### 5. C4 (Collective Code Construction Contract) model

#### Adopting a Share-alike License

For Exosphere, this would mean using a share-alike license such as the MPLv2, or a GPLv3 variant thereof (GPL, LGPL,
AGPL), unless we can modify C4 to be compatible with the BSD 3-Clause license. There is no copyright assignment or CLA,
and all patches are owned by their authors, and shall use the same license as the project.

#### Ensuring Minimal and Accurate Patches

A patch (merge request) SHOULD be a minimal and accurate answer to exactly one identified and agreed problem.

#### Maintaining a List of Meaningful Issues

The release history of the project SHALL be a list of meaningful issues logged and solved.

#### Standardizing Commit Messages

We would need to standardize the formatting of commit messages.

#### Avoiding Value Judgments on Patches

Maintainers SHALL NOT make value judgments on correct patches. Any Contributor who has value judgments on a patch SHOULD
express these via their own patches.

#### Merging Correct Patches Rapidly

Maintainers SHALL merge correct patches from other Contributors rapidly.

#### Closing User Issues

Maintainers SHOULD close user issues that are left open without action for an uncomfortable period of time.

**Pros & Cons**

- Pro: The C4 model offers a simple and lightweight governance
  process ([one short page](https://rfc.zeromq.org/spec/42/)), which can be beneficial for a project that values agility
  and minimal bureaucracy.
- Pro: The goals of the C4 model align closely with our requirements, suggesting that it could be a good fit for our
  project's needs and values.
- Pro: The workflow prescribed by the C4 model is very similar to our current practices, which could make the transition
  smoother and less disruptive for our team.
- Unknown: It's unclear whether the C4 model is compatible with the BSD 3-Clause license. We would need to investigate
  this further to ensure we're not violating any licensing terms.
- Unknown: The C4 model's policy of not making value judgments and rapidly merging correct patches could require some
  adjustment from our team, as we're used to having tighter control over the codebase. While this could potentially lead
  to conflicts in the short term, it could also result in more contributions and contributors in the long term, provided
  we can maintain the high quality of our codebase.

## Decision

TODO

## Consequences

TODO

---

## Appendix A: Governance Model Evaluation Rubric

### Using Structured Decision-Making to Inform Intuition

The process of selecting a governance model can be complex and subjective. To ensure a comprehensive and unbiased evaluation, we will use a structured decision-making approach inspired by the Mediating Assessments Protocol (MAP) from Kahneman et al.'s paper "A Structured Approach to Strategic Decisions". The steps are as follows:

1. **Identify Key Factors**: We have identified key factors that will influence our decision. These factors serve as our scoring rubric and include aspects like 'Builds Consensus', 'Responsiveness to Community Needs', and 'Corporate Adoption'.

2. **Score Each Option**: For each governance model, we will score it independently on each factor. We will use as much factual information as possible to avoid bias.

3. **Delay the Final Decision**: We will wait until all key factors have been scored for all governance models before making our final decision. This approach helps to prevent early impressions or biases from unduly influencing our decision.

4. **Make the Final Decision**: Once all governance models have been scored on all factors, we can make our final decision. This decision should be based on the scores each model received.

This structured approach ensures that our decision is based on a comprehensive, unbiased evaluation of all governance models. For more details on this approach, refer to the paper ["A Structured Approach to Strategic Decisions"](https://ses.library.usyd.edu.au/handle/2123/28501).

### Table with Criteria

| Criteria                                  | ASF | CNCF | OpenInfra | C4 | Interim |
|-------------------------------------------|-----|------|-----------|----|---------|
| 1. Builds Consensus                       |     |      |           |    |         |
| 2. Responsiveness to Community Needs      |     |      |           |    |         |
| 3. Security and Trustworthiness           |     |      |           |    |         |
| 4. Contributor Recruitment and Retention  |     |      |           |    |         |
| 5. Licensing                              |     |      |           |    |         |
| 6. Corporate Adoption                     |     |      |           |    |         |
| 7. Overhead and Bureaucracy               |     |      |           |    |         |

Score each model for all criteria on a scale of 1 (poor fit) to 5 (excellent fit), based on your research and
understanding of its rules, processes, customs, track record, and examplar projects. Remember that this is a subjective process, and it's okay if different people have different opinions. The goal is to facilitate discussion and guide your decision-making process.

### Scoring Scale

- **1 (Poor Fit)**: Does not meet this criterion at all or does so very poorly
- **2 (Below Average Fit)**: Somewhat meets this criterion, but there are significant shortcomings
- **3 (Average Fit)**: Meets this criterion to an acceptable degree, but there may be some
  shortcomings
- **4 (Above Average Fit)**: Meets this criterion well, with only minor shortcomings
- **5 (Excellent Fit)**: Meets this criterion exceptionally well.

### Scoring Guidelines

**1. Builds Consensus:** Decision-making processes, conflict resolution mechanisms, structures for
facilitating discussion and building agreement among stakeholders

**2. Responsiveness to Community Needs:** Mechanisms for gathering and responding to community feedback, flexibility
and adaptability to changing needs and circumstances

**3. Security and Trustworthiness:** Security policies and procedures, mechanisms for ensuring the quality and reliability of the product

**4. Contributor Recruitment and Retention:** Outreach strategies, onboarding processes, incentives, acknowledgment, rewards, mentorship programs and other
mechanisms for supporting member growth and advancement

**5. Licensing:** Compatibility with the project's current
license (BSD-3 Clause), any onerous relicensing requirements

**6. Corporate Adoption:** Permissive license (with possible patent grant), mechanisms for corporate involvement (like sponsorship or partnership opportunities), track record of successful
corporate adoption.

**7. Overhead and Bureaucracy:** Administrative requirements, reporting processes, code hosting & CI/CD requirements, and any other bureaucratic elements that could add
complexity or slow down the project