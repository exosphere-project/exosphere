# Exosphere Governance

Herein, "Exosphere" and "the project" are defined as:

- Everything in the GitLab namespace <https://gitlab.com/exosphere>, including (but not limited to) the Exosphere open-source codebase, issue tracker, and merge requests.
- The Exosphere hosted sites as they are enumerated in the [Acceptable Use Policy](acceptable-use-policy.md).
- The `exosphere.app` domain name, DNS, and everything that is served within it.

Exosphere is dedicated to [these goals](values-goals.md). This goverance explains how the project is run.

This is a living document that will evolve per section "Modifying this Charter". It was originally adapted from the Cloud Native Computing Foundation [simple maintainer governance template](https://github.com/cncf/project-template/blob/main/GOVERNANCE-maintainer.md).

## Maintainers

Exosphere maintainers have write access to the [project GitLab namespace](https://gitlab.com/exosphere).
They can approve and merge contributions, per the policy set in [Contributing to Exosphere](../contributing.md).
Maintainers collectively manage the project's resources and contributors.

The current Maintainers for Exosphere are:

| Name             | GitLab username | Employer               |
| ---------------- | --------------- | ---------------------- |
| Chris Martin     | cmart           | Indiana University     |
| Julian Pistorius | julianpistorius | CodeCove Solutions LLC |

This privilege is granted with some expectation of responsibility: maintainers are people who care about Exosphere and want to help it grow and improve. A maintainer is not just someone who can make changes, but someone who has demonstrated their ability to collaborate with the team, get the most knowledgeable people to review code and docs, contribute high-quality code, and follow through to fix issues (in code or tests).

A maintainer is a contributor to the project's success and a citizen helping the project succeed.

The collective team of all Maintainers is known as the Maintainer Council, which is the governing body for the project.

### Becoming a Maintainer

To become a Maintainer you need to demonstrate the following:

- commitment to the project:
  - participate in discussions, contributions, code and documentation reviews for 6 months or more,
  - perform reviews for at least 10 non-trivial pull requests,
  - contribute at least 10 non-trivial pull requests and have them merged,
- ability to write code and/or documentation conformant with the [MR Quality Checklist](quality-checklist.md),
- ability to collaborate with the team,
- understanding of how the team works (policies, processes for testing and code review, etc),
- understanding of the project's code base and coding and documentation style.

A new Maintainer must be proposed by an existing maintainer by sending a message to the other existing maintainers. A simple majority vote of existing Maintainers approves the application. (If there are two maintainers, both must approve.) Maintainer nominations will be evaluated without prejudice to employer or demographics.

Maintainers who are selected will be granted the necessary GitLab permissions, and invited to the [private maintainer chat](https://matrix.to/#/#exosphere-maint:matrix.org) group.

### Removing a Maintainer

Maintainers may resign at any time if they feel that they will not be able to continue fulfilling their project duties.

Maintainers may also be removed after being inactive, failure to fulfill their Maintainer responsibilities, violating the Code of Conduct, or other reasons. Inactivity is defined as a period of very low or no activity in the project for a year or more, with no definite schedule to return to full Maintainer activity.

A Maintainer may be removed at any time by a 2/3 vote of the other maintainers.

Depending on the reason for removal, a Maintainer may be converted to Emeritus status. Emeritus Maintainers will still be consulted on some project matters, and can be rapidly returned to Maintainer status if their availability changes.

## Meetings

Time zones permitting, Maintainers are expected to participate in the community meeting, the time and coordinates of which are stated in [README.md](../README.md#collaborate-with-us).

Maintainers will also have closed meetings in order to discuss security reports or Code of Conduct violations. Such meetings should be scheduled by any Maintainer on receipt of a security issue or CoC violation report. All current Maintainers must be invited to such closed meetings.

## Code of Conduct

[Code of Conduct](code-of-conduct.md) violations by community members will be discussed and resolved on the [private maintainer chat](https://matrix.to/#/#exosphere-maint:matrix.org).

## Security Response Team

The Maintainers will appoint a Security Response Team to handle security reports. This committee may simply consist of the Maintainer Council themselves. If this responsibility is delegated, the Maintainers will appoint a team of at least two contributors to handle it. The Maintainers will review who is assigned to this at least once per year.

The Security Response Team is responsible for handling all reports of security issues (e.g. vulnerabilities and breaches).

## Voting

While most decisions in Exosphere are made via the 2-maintainer merge request review rule, periodically the Maintainers may need to vote on specific actions or changes.

A vote can be taken on a community-facing chat, or the [private Maintainer chat](https://matrix.to/#/#exosphere-maint:matrix.org) for security or conduct matters. Votes may also be taken at the aforementioned developer meeting. Any Maintainer may demand a vote be taken.

Most votes require a simple majority of all existing Maintainers to succeed, except where otherwise noted in this policy. Two-thirds majority votes mean at least two-thirds of all existing maintainers.

## Modifying this Charter

Changes to this Governance and its supporting documents must be approved by a 2/3 vote of the Maintainers.
