# Contributing to Exosphere

Exosphere has a lighter-weight process for new contributors. If you're making your _first or second_ contribution to Exosphere, you only need [Quick Start for New Contributors](docs/contributor-quick-start.md). Feel empowered to ignore the rest of this doc until making your _third or subsequent_ contribution to Exosphere.

See [Contributor skills](docs/contributor-skills.md) for the skills you need (or will learn!) while working on different parts of the project.

## Core Contributor Onboarding

### Development Environment Setup

In addition to [Running Exosphere For Development Work](docs/run-exosphere.md#for-development-work), we recommend that you set up the following.

- **`elm-format` on save**
  - [configure your editor](https://github.com/avh4/elm-format#editor-integration) to apply code formatting whenever you save a file.
  - If you save files often, you save yourself a lot of typing and indenting work.
  - Similarly, if you find yourself editing `js`, `json`, or `html` files, you can [enable Prettier integration](https://prettier.io/docs/en/editors.html) to automatically format those.
- **`pre-commit` for verifying your commits**
  `pre-commit` is a utility for managing git hooks. We use it to run tests, format tools, and code analysis tools.
  - Install [`pre-commit`](https://pre-commit.com/index.html#install)
    - If you already have Python, run `pip install --user pre-commit`
    - Using [Homebrew](https://brew.sh/) on MacOS, run `brew install pre-commit`
    - If you use [Conda](https://conda.io/), run `conda install -c conda-forge pre-commit`
  - Run `npm install` and `npm run prepare` to set up `pre-commit` in your development environment.
  - When you try to `git commit`, pre-commit will run these, and stop the commit if anything fails:
    - unit tests in `tests/`
    - `elm-analyse` static analysis tool
    - `elm-format` Elm code formatter
    - `elm-review` code linting tool
    - `prettier` JavaScript, JSON, and HTML formatter
    - `typescript` Validating JavaScript configuration files
  - This will catch many common issues long before GitLab's CI pipeline does.
  - If you need to bypass this for any reason, `git commit --no-verify` will skip this validation.

### Design Review Process

When solving an issue of weight 5-10, we strongly encourage you to have a design/scoping discussion with others on the project. Consider writing a short implementation plan and posting it as an issue comment.

When solving an issue of weight 20 or higher, we require you to write a step-wise implementation plan, post it as an issue comment, and collect feedback from at least one maintainer. Please do this early, before requesting code review. _This could save you many hours of re-work._

### Submitting a Contribution

When creating a merge request (MR), please assign it to yourself, and begin the title with `Draft: ` until you believe it passes the [MR Quality Checklist](docs/quality-checklist.md). Then, remove the `Draft: ` prefix to mark the MR as ready.

Maintainers are happy to provide guidance, even if your MR is an early draft. Feel free to ask in chat. You can also add someone to the "Reviewers" section to request review from a specific person. (Leave the MR assigned to yourself.)

MRs are generally reviewed within 1-2 working days. An MR should be merged as soon as it is approved by two maintainers, and it has a passing CI pipeline (see below). The two-maintainer rule is occasionally relaxed for periods of decreased maintainer availability.

If your MR fixes one or more issues, please do not close them before your MR is merged. As long as you write (e.g.) `fixes #123` in the MR description, the merge will close the issues automatically.


---

The information below is for reference. You don't need to understand it to contribute, but it may be helpful in some situations.

## Continuous Integration

Our continuous integration (CI) pipeline runs:

- [elm-format](https://github.com/avh4/elm-format) (to ensure that contributions comply with the
  [Elm Style Guide](https://elm-lang.org/docs/style-guide))
- [prettier](https://prettier.io/) (to ensure consistent javascript & html file formatting)
- [typescript](https://www.typescriptlang.org/) (to validate javascript configuration files)
- [elm-analyse](https://stil4m.github.io/elm-analyse/)
- [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/)
- [unit tests](tests/README.md)
- End-to-end tests which exercise the application in real web browsers

You can run all of these but the browser tests locally. The easiest way is to set up `husky` (per the section above) and try to `git commit`. Or, you can test manually with these commands:

 ```bash
 npm install
 npm run test
 npm run elm:analyse
 npm run elm:format
 npm run elm:review
 npm run js:format
 npm run js:typecheck
 ```

### Enabling CI On Your Fork

Optionally, you can *enable GitLab CI/CD on your fork project* to test the pipeline before submitting a merge request.

1. On GitLab, go to your fork's CI/CD settings (at `https://gitlab.com/your-gitlab-username-here/exosphere/edit`)
2. Expand the "Visibility, project features, permissions" section
3. Ensure your Project Visibility is "Public"
4. Enable CI/CD for "Everyone With Access"

![Enable CI/CD in project settings](docs/assets/gitlab-enable-ci-cd.png)

5. Scroll down and click "Save changes" (it's easy to miss this button)

![Enable CI/CD in project settings](docs/assets/gitlab-enable-ci-cd-save-changes.png)

The CI/CD pipeline should run the next time you push a commit to your fork project on GitLab. Pipeline status should be visible at `https://gitlab.com/your-gitlab-username/exosphere/-/pipelines`, and also in any merge request that you submit to the upstream Exosphere project.

### End-to-end browser tests

Our CI pipeline also runs end-to-end tests with real browsers.  If you are a regular contributor, you can enable browser tests to run from your fork project.

For these tests to work, you will need:

1. An ACCESS account (Go to https://access-ci.org and create one)
2. Access to the "Exosphere Integration Testing" (INI210003) Jetstream2 allocation. If you do not have access to the INI210003 Jetstream2 allocation, please ask the maintainers.
3. A Jetstream2 administrator to set an OpenStack password for your user account
4. Set `OS_USERNAME` and `OS_PASSWORD` environment variables in the GitLab CI/CD settings of your own fork of Exosphere

Here is how to add OpenStack credentials as environment variables to your GitLab repository settings:

![Environment variables for end-to-end browser tests](docs/assets/environment-variables-e2e-browser-tests.png)

Note that the variables are _masked, but not protected_.

The next time you push a commit to your fork project, the browser tests should run in the CI pipeline.

#### Special Branch Behavior

The CI pipeline's end-to-end browser tests have special behavior for `master` and `dev` branches, as opposed to all other git repository branches.

**`master` and `dev` branches:** The CI pipeline deploys Exosphere to production environments ([try.exosphere.app](https://try.exosphere.app/) and [exosphere.jetstream-cloud.org](https://exosphere.jetstream-cloud.org/) for master branch, [try-dev.exosphere.app](https://try-dev.exosphere.app/) for dev branch), then runs the tests against these live production environments. If you are working on a fork of `exosphere/exosphere` on GitLab, these deploy jobs will not succeed (because you hopefully lack the secrets needed to deploy to production) and the tests may fail as well. So, contributors are encouraged _not_ to work on branches named `master` or `dev` at this time, even on your own fork of the project.

**All other branches:** Compiled Exosphere assets from the `elm_make` stage are combined with Selenium and a headless browser in a custom container, and the tests point at Exosphere served by this container. (See the `.build_with_kaniko`-based jobs in the `dockerize` stage.) Browser tests for merge requests run in the `test` stage along with the other tests (like Elm unit tests, `elm-analyze`, etc.). This is all self-contained within the CI pipeline; it does not use or modify live production environments.

## Architecture Decisions

We use lightweight architecture decision records. See: <https://adr.github.io/>

Our architecture decisions are documented in: [docs/adr/README.md](docs/adr/README.md)
