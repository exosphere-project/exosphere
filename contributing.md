## Contributor guidelines

- Be respectful
- Be constructive
- Be kind


## Submitting Code

Our CI pipeline runs:

- [elm-format](https://github.com/avh4/elm-format) (to ensure that contributions comply with the   
  [Elm Style Guide](https://elm-lang.org/docs/style-guide)
- [elm-analyse](https://stil4m.github.io/elm-analyse/)
  
 Please use `elm-format` and `elm-analyse` before you submit a merge request:
 
 ```bash
 npm install
 npm run elm:format
 npm run elm:analyse
 ```


## End-to-end browser tests

Our CI pipeline also runs end-to-end tests with real browsers. For these tests to work you need:

1. Access to a Jetstream allocation
2. Valid TACC (Texas Advanced Computing Center) credentials
3. Set `taccusername` and `taccpass` environment variables in the GitLab CI/CD settings of your own fork of Exosphere

Ask the maintainers for testing credentials if you don't have a Jetstream allocation.

How to add TACC credentials as environment variables to your GitLab repository settings:

![Environment variables for end-to-end browser tests](docs/environment-variables-e2e-browser-tests.png)

The CI pipeline's end-to-end browser tests have special behavior for `master` and `dev` branches, as opposed to all other git repository branches.

**`master` and `dev` branches:** The CI pipeline deploys Exosphere to production environments ([try.exosphere.app](https://try.exosphere.app/) and [exosphere.jetstream-cloud.org](https://exosphere.jetstream-cloud.org/) for master branch, [try-dev.exosphere.app](https://try-dev.exosphere.app/) for dev branch), then runs the tests against these live production environments. If you are working on a fork of `exosphere/exosphere` on GitLab, these deploy jobs will not succeed (because you hopefully lack the secrets needed to deploy to production) and the tests may fail as well. So, contributors are encouraged _not_ to work on branches named `master` or `dev` at this time, even on your own fork of the project.

**All other branches:** Compiled Exosphere assets from the `elm_make` stage are combined with Selenium and a headless browser in a custom container, and the tests point at Exosphere served by this container. (See the `.build_with_kaniko`-based jobs in the `dockerize` stage.) Browser tests for merge requests run in the `test` stage along with the other tests (like Elm unit tests, `elm-analyze`, etc.). This is all self-contained within the CI pipeline; it does not use or modify live production environments.

## Architecture Decisions

We use lightweight architecture decision records. See: <https://adr.github.io/>

Our architecture decisions are documented in: [docs/adr/README.md](docs/adr/README.md)
