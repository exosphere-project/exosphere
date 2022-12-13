# ADR 3: Storing, versioning, and consuming instance provisioning code

## Status

Accepted

## Context

When Exosphere launches a new instance, that instance is [provisioned](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/adr/config-mgt-instance-provisioning.md#decision-and-consequences) (Guacamole installed+configured, desktop environment set up, etc) by downloading and running Ansible code from the [instance-config-mgt](https://gitlab.com/exosphere/instance-config-mgt) repository.

Right now, Exosphere consumes an explicit commit from that repo by specifying its hash in [`src/ServerDeploy.elm`](https://gitlab.com/exosphere/exosphere/-/blob/master/src/ServerDeploy.elm#L107). Right now, @cmart (the sole committer to this repo yet) has made changes by pushing commits to the master branch of instance-config-mgt, then proposing the changes be brought in by placing a merge request with a new commit hash to the main Exosphere repo.

This is suboptimal because:

- These merge requests aren't easy to review. In order to see full set of changes (including changes to the provisioning code), one also needs to go in [here](https://gitlab.com/exosphere/instance-config-mgt/-/compare) and compare the old specified commit to the new one.
- In order to test new local changes to instance provisioning code, they must be committed and pushed to a git repository somewhere (e.g. GitLab) that the instance can suck down from the internet. So, you end up with a lot of work-in-progress commits, or you need to amend your development workflow to rebase frequently and/or use a separate, disposable fork/branch to test changes.

### What would success / a fix look like?

We would like to meet these objectives:

1. It's easy to develop and test changes to provisioning code, optimally without having to push work-in-progress commits to GitLab
2. It's easy to propose and review changes to provisioning code, optimally with a single merge request containing all changes (mono-repository)
3. Changes to master won't break existing/production Exosphere deployments for a perceptible period of time. [^1]
4. It's easy for cloud operators to offer a customized Exosphere with their own provisioning code that is different from ours
5. CI jobs automatically test provisioning code changes alongside corresponding Exosphere code changes
6. It's architected with no more complexity than needed, and introduces as few new technologies/systems/dependencies as feasible
7. The solution scales well as our provisioning code grows, when we implement planned new capabilities/features
8. Doesn't require us to do sophisticated new things with bash in cloud-init user data, when we've been trying to move away from that and toward Ansible

## Choices

### 1. Merge Request template requires link to diff of provisioning code

Add an item to the MR template to require a 'diff' link if the MR changes the Git commit hash in `src/ServerDeploy.elm`. The 'diff' link should contain a GitLab link with the old hash and the new hash, like so: <https://gitlab.com/exosphere/instance-config-mgt/-/compare/011afcdc84a68f07e90d44fd5c2ecc911303ad87...0ceda941129d1aaa4013ae2ff81a1fc9e68fba7b>

- Good because
    - Meets objectives 3, 5, 6, 7, and 8
    - Is compatible with a solution for objective 4
    - Very easy change to implement
- Bad because
    - Doesn't fully meet objective 1: testing changes requires pushing work-in-progress commits somewhere
    - Doesn't fully meet objective 2: reviewing changes requires looking in two places

### 2. Mono-repository that explicitly passes provisioning code through cloud-init user data

- Move provisioning code from [instance-config-mgt](https://gitlab.com/exosphere/instance-config-mgt) repo into the main Exosphere repo.
- When Exosphere is compiled, provisioning code is tarballed (and possibly compressed), base64-encoded, and passed as a new option to `config.js` (or possibly as a static file that is served alongside Exosphere and fetched by Exosphere via XHR)
- Exosphere passes this base64 blob to the new instance in cloud-init user data
- New instance decodes the blob, decompresses/un-archives it, writes it to disk, and calls `ansible-playbook` to run it
- Good because
    - Meets objectives 1, 2, 3, 4, 5, and arguably 6
    - Instance provisioning relies less on network connectivity and availability of code hosting site
- Bad because
    - Doesn't meet objective 7: our limit for cloud-init user data is somewhere between 16 and 64 KB
    - Doesn't meet objective 8

### 3. Mono-repository with new instances downloading provisioning code from code hosting site

- Move provisioning code from [instance-config-mgt](https://gitlab.com/exosphere/instance-config-mgt) repo into the main Exosphere repo
- Add two options to `config.js` which allow the deployer/user to specify a repository URL and commit/branch/tag for new instances to consume provisioning code from.
    - If these options are not populated, new instances will consume code from upstream master on GitLab.
- CI jobs which deploy production environments and test changes (headless browser tests) set explicit repository URL and commit hash in `config.js` for new instances to consume provisioning code from
- Good because
    - Meets objectives 2, 3, 4, 5, 6, 7, and 8
    - Straightforward to implement
- Bad because
    - Doesn't fully meet objective 1: testing changes requires pushing work-in-progress commits somewhere

### 4. Mono-repository with new instances downloading provisioning code from code hosting site, plus commit-free testing of local changes

(by explicitly passing them through cloud-init user data, per @julianpistorius)

- Includes everything from solution 3, plus uses solution 2 (only for testing local changes)
- Adds a script for developer to run which allows testing local changes to provisioning code, by doing the following:
    - Creates a diff of any local changes to provisioning code
    - base64-encodes this diff (perhaps compressing it first)
    - passes this base64-encoded diff to Exosphere by writing it out in `config.js` (or possibly to a static file that is served alongside Exosphere and fetched by Exosphere via XHR)
    - Exosphere passes the diff to new instances as cloud-init user data
- cloud-init code that runs on new instances does the following:
    - Looks for any base64-encoded diff of changes to provisioning code
    - If such a diff is present, un-encodes it (and perhaps decompresses it), and applies it to downloaded code before running it
- New documentation orienting developers to the process for testing local changes to provisioning code
- Good because
    - Meets objectives 1, 2, 3, 4, 5, 6, and 7!
- Bad because
    - Doesn't meet objective 8
    - Possibly fiddly to implement and troubleshoot (@cmart opinion)

## Decision and Consequences

Implement solution 3 now, and keep solution 4 as a follow-up issue to implement later.

## Footnotes

[^1]: In the main Exosphere repo, there is a ~10 minute delay between merging new code to master on GitLab and deploying that code to the production sites. If the Exosphere production sites consume instance provisioning code from whatever is on the master branch on GitLab, then each change to master creates a ~10 minute window where the provisioning code that is downloaded/run by new Exosphere-launched instances is newer than the Exosphere code that launched those instances. This could temporarily break instance deployments if, for example, a change to the provisioning code requires Exosphere to pass it a new variable. We could prevent this from happening if the CI job sets the appropriate commit hash of Exosphere to consume provisioning code from, in config.js, and Exosphere uses that hash instead of just the master branch.