# Issue Triage Process

## Who and When

Project maintainers alternate triage duty, based on the day an issue was submitted. @julianpistorius triages issues submitted on odd days, and @cmart triages issues submitted on even days. If a maintainer is on a vacation that is entered into the Exosphere collaborator calendar, the other maintainer triages issues submitted on those vacation days. The exception to this is if a maintainer is also the submitter of an issue -- the maintainer self-triages regardless of the day.

The designated maintainer should triage an issue within two working days of its creation. Issue submitters are welcome to self-triage to the extent that they can. In that case, the maintainer's job is only to ensure the triage process was applied correctly.

If the issue is not well-specified enough for a maintainer to triage it, they should assign it to the submitter and ask them to clarify as needed. If the submitter does not provide adequate clarification within 14 calendar days, the maintainer should close the issue.

## Deduplication

Ensure the new issue does not substantially duplicate any existing open issue. If it does, decide which issue to close.

Your default choice should be to close the new issue _unless_ (1) the new issue is a better description of the same problem, and (2) the existing issue does not contain much discussion or linked work.

Whichever you decide, close it with a comment to the effect of "Closing as duplicate of #XXX, please use that issue and update it as needed.", where XXX is the issue left open.

## Resolving Disagreements

If a maintainer disagrees with how another maintainer applied the triage process, they should work with the person who triaged the issue to reach agreement.

## Quick Triage Checklist

- Description is adequately populated
- Weight is set
- Appropriate labels are applied, including type, severity and priority

## Detailed Triage Checklist

### Description

Ensure the description template was followed.

Ensure that it describes only one problem. A list of unrelated problems deserves a separate issue for each one.

### Weight

Set the weight to estimate the complexity or the amount of work required to solve the issue. It is only loosely coupled with implementation time for a solution.

Weight is a linear scale so that it's meaningful to sum the weights of issues.

- 1 is a "good first issue". An experienced contributor can solve this quickly, with trivial design work. Example: changing some text in the UI.
- 2 is a "good second issue".
- 5 is a "good second issue" for an ambitious new contributor. It may require familiarity with the layout of the codebase.
- 10 may require the contributor to have context of the problem domain, including figuring out something with OpenStack. The solution may require collaborative design or scoping work between the contributor, maintainers, and possibly the issue submitter.
- 20 example: adding a new page with new functionality. May require a combination of instance configuration (Ansible) and front-end (Elm) work.
- 40 example: adding a mid-sized new feature which may involve multiple pages in the app.
- 80 example: adding a large or complex new feature
- 160 The solution requires exploratory work, planning, consensus, and implementation in phases. Example: Rewriting a large fundamental part of Exosphere, which might lead to many hard problems to solve.

If something is very large, it should probably be split up in multiple issues or chunks. You can simply omit the weight of the parent issue and set weights to child issues.

### Labels

Among the prioritized labels (those starting with a capital letter), select those that apply to the issue. See the label description for an explanation of each.

#### Label: Type

Set the type label according to the sort of issue. The urge to set multiple types for an issue suggests that that one should split it into multiple issues.

- Bug: the app doesn't work as advertised.
- Feature: opportunity for the app to do something new, or make an existing feature bigger or better.
- Maintenance: technical debt, documentation, or project management.

#### Label: Severity

Set the severity label to communicate the impact of an issue to the people affected.

Issue severity applies only to "Bug" and "Maintenance" type issues. Severity should not be set for "Feature" issues.

1. Blocker
  - ~Bug: Broken feature with data loss, or no workaround.
  - ~UI/UX ~Bug: User would say "I can't figure this out, likely to make risky errors, or ask for support."
  - ~Maintenance: Blocks development (e.g. broken CI/CD), or has turned away contributors.
2. Critical
  - ~Bug: Broken feature with an unacceptably complex workaround.
  - ~UI/UX ~Bug: User would say "This workaround is painful or it significantly delays me."
  - ~Maintenance: Causes unacceptable friction to development, or confusion that is unlikely to be resolved by studying docs or code.
3. Major
  - ~Bug: Broken feature with a workaround.
  - ~UI/UX ~Bug: User would say "This still works, but I have to make changes to my process."
  - ~Maintenance: Causes confusion that is likely to be resolved by studying docs or code.
4. Low
  - ~Bug: Functionality is inconvenient.
  - ~UI/UX ~Bug: User would say "There is a small inconvenience, inconsistency, or cosmetic issue."
  - ~Maintenance: Causes some annoyance to contributors.

#### Label: Priority

Set the priority label to indicate the importance and guide the scheduling of the issue. We determine priority based on the project direction, the number of impacted users and capacity of the team.

Unless you have a good reason to set priority 1 through 3, set priority 4. If you are unsure, consult co-maintainer(s).

1. Urgent: We will address this as soon as possible regardless of the limit on our team capacity, and discuss it at every weekly meeting.
2. High: We will address this soon, will provide capacity from our team for it, and discuss it at least once per month.
3. Medium: We want to address this but may have other higher priority items. No timeline designated.
4. Low: We don't know when this will be addressed. No timeline designated.

#### Label: "Triaged"

Finally, apply the "Triaged" label to indicate completion of the triage process.