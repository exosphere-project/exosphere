# Contribution (Merge Request) Review Policy

## Approval

We use GitLab's [approvals](https://docs.gitlab.com/ee/user/project/merge_requests/approvals/) feature. Normally, a contribution needs two approvals in order to merge. One approval must be from a project maintainer. The other can be from either a maintainer or a core contributor (who are appointed by the maintainer council).

Any contributor is also encouraged to review and approve MRs, even if their approval has no effect on mergeability. There is no harm in over-approving.

Push the approve button when you believe a contribution passes the [Quality Checklist](quality-checklist.md), and you don't have further concerns that you think _must_ be addressed before merging.

## How to Raise Concerns

When asking for changes to an MR, consider that we want to keep moving forward, because long-lived MRs have [compounding penalties](https://gitlab.com/exosphere/exosphere/-/issues/968) like accumulation of merge conflicts. If you see a problem that will negatively affect users or add a lot of technical/maintenance debt, withhold your approval and request a revision. Otherwise, it's better to merge an imperfect MR that is still net-beneficial to the project than to exchange several rounds of comment and revision. We can fix any minor issues with a subsequent MR.

(The foregoing is _especially_ true for code used only in features that users only see upon enabling "experimental features" in the app settings. As long as a change doesn't break non-experimental features, err on the side of approving.)

It's fine to approve an MR _and_ suggest changes ("nitpicks") at the same time. This provides the submitter an opportunity to revise the MR prior to merge, but doesn't obligate them to do so.

If you know how to fix your own concern, please feel empowered to submit the fix, either as:

- A sub-MR to the MR branch on the submitter's fork project, if it seems the MR also needs other revisions prior to merge.
- A follow-up MR to the upstream project, if it seems the MR is about to be merged.

## When to Seek More Review

Suppose an MR has all required approvals. Should a maintainer merge the contribution, or seek further review?

- Seek further review if the MR appears to require further testing or discussion (to resolve ambiguity or conflict).
- If there's reason to believe a given maintainer will have concerns, seek their review specifically.

Otherwise, just merge it! 🚢

## When to Stop Waiting

If an MR awaits review from a specific person, and two of that person's regular workdays have since passed, construe this as passive non-dissent, and move forward without waiting longer for a response.

As a reviewer, it is also okay to say (e.g.) "Please wait a few more days, I need more time to review".

## When to Bend the Rules

If one or more maintainers are on vacation such that only one maintainer is left tending the project, this maintainer may temporarily relax the approval rule so that the project can continue moving forward. In this case, the sole maintainer-on-duty is encouraged to seek approval from at least one core contributor prior to merging an MR.