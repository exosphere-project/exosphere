# Exosphere Sustainability Goals, Metrics, and Measurement

## Overview: What does sustainability look like?

A sustainable open-source software project:

- Is responsive to changing needs of its community
- Has maintainers who set strategic direction to meet community needs, and ensure the long-term health of the project
- Has project members regularly engaged and available to respond to community requests
- Recruits new project members to compensate for attrition
- Grows existing project members toward becoming maintainers
- Maintains a secure, trustworthy open source product

## Goals and Metrics

### Responsiveness to Community Needs

Metric: Response time to triage and respond to new issues.

- Current: Need to measure
- Target: 90% of issues reviewed within 2 working days
- Plan to measure:
  - Choose a time period to measure (e.g. previous calendar month)
  - Look at each new issue created in that time period (e.g. <https://gitlab.com/exosphere/exosphere/-/issues/?sort=created_date&state=all>)
  - For each issue, note the period of time that passed between creation and application of "Triaged" label
  - Order these values
  - Take the 90th percentile value

Metric: Response time to review merge requests (code contributions).

- Current: Need to measure
- Target: 90% of merge requests reviewed within 1 working week
- Plan to measure:
  - Choose a time period to measure (e.g. previous calendar month)
  - Look at each new merge request created in that time period (e.g. <https://gitlab.com/exosphere/exosphere/-/merge_requests?scope=all&sort=created_date&state=all>)
  - For each merge request, note the period of time that passed between creation and response from a project maintainer
  - Order these values
  - Take the 90th percentile value

### Sustainable Maintainership

Goal: Exosphere should have a sufficient number of maintainers.

Metric: Number of maintainers.

- Current: 2
- Target: 2 or more
- Plan to measure: Count the number of maintainers in <https://exosphere.app/docs/governance/#maintainers>

### Sustainable Contributorship

Goal: Exosphere should have regular contributors covering all of the core [Exosphere Contributor Skills](https://exosphere.app/docs/contributor-skills).

Metric: Percentage of contributor skills covered by regular contributors.

- Current: 100%
- Target: 100%
- Plan to measure:
  - Choose a time period to measure (e.g. previous 6 calendar months)
  - Make a list of people who caused [activity on the GitLab project](https://gitlab.com/exosphere/exosphere/activity)
  - For each skill in the [Contributor Skills](contributor-skills.md) table, determine whether one of more of the above contributors possess that skill
  - Take the percentage of covered skills

### Project Security Posture

Metric: OpenSSF Best Practices.

- Current: 78%
- Target: 85%
- Plan to measure:
  - Review <https://bestpractices.coreinfrastructure.org/en/projects/7368>
  - Update any answers to the questions that may have changed recently
  - Note the new percentage of best practice coverage