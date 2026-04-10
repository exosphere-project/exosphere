# AI Policy

Effective: 2026-04-10

## Principles

We are grateful for your time, energy, & compute. Your contributions are welcome & valuable.

Exosphere's culture is one of thoughtful, deliberate code, built to underpin a stable, reliable, & self-sovereign system.

Now imagine that each month a charitable development community of 100 developers decides where to focus their efforts. This month they've chosen Exosphere. They work hard & submit MRs to close every single issue in the backlog. This would be amazing, but what challenges would the project face accepting these contributions? The advent of AI moves makes this thought experiment a practical possibility.

This policy exists to address those challenges & safeguard Exosphere's culture, while respecting the tooling choices of its contributors.

## Policy

Responsibility lies with a human. You are fully accountable for the correctness, safety, licensing & maintainability of any code you submit, regardless of how it was produced.

All contributions must be:

- Submitted under your own GitLab account.
- Human-readable & explainable by the author.
- Compatible with the project's [license](../LICENSE).
- Compliant with existing contribution requirements (ref. [MR Quality Checklist](quality-checklist.md) & [Contribution Review Policy](review-policy.md)).
- Wary of imposing excessive review cost. Reviewer time is a limited resource & a potential bottleneck.

Merge Requests (MRs) not adhering to the policy may be closed with minimal feedback and/or a request for rework.

## Guidelines

### Use Your Fork as a Scratchpad

Use your own fork for experimentation, AI-assisted reviews, bot submissions, etc. When you're confident in the result, submit a polished MR to the upstream project.

### Don't Request Automated Reviews on MRs

Do not request automated reviews (e.g. GitLab Duo or similar) on merge requests submitted to the Exosphere project. These do not replace human review & often generate noise that distracts maintainers.

### Attribution

Use your best judgement in your approach to AI attribution. Git trailers such as `Co-Authored-By` might be appropriate.

### Licensing

Ensure that the terms & conditions of tools you use do not place contractual restrictions on how their output can be used which are inconsistent with the project's license or intellectual property expectations.

### Security Reports

If you use AI tools to assist with [vulnerability reports](vulnerability-disclosure.md), include a short self-certification that you reviewed & validated the report to the best of your ability. Reports that are clearly unverified AI-generated output have been known to contain inaccurate or fictitious content, & can be closed without investigation.
