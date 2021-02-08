# ADR 1: Choose technologies for automated, end-to-end browser tests

## Status

Proposed.

## Context

In order to prevent regressions we want to automatically test the Exosphere application with real browsers.

We have an existing suite of browser tests (see the `integration-tests/` directory in this repository).

## Decision

Use Gitlab CI to run the existing integration tests for any proposed merge requests (and on a scheduled basis - TODO).

The e2e GitLab CI jobs include a service based on a container which packages the Selenium remote web driver along with
real browsers (Chrome and Firefox): <https://github.com/SeleniumHQ/docker-selenium>

We extend this Selenium+Browser container image to include:

1. Exosphere assets: Compiled Exosphere assets based on the latest commit in the current branch
2. Exosphere service: A `supervisord` program that launches a local web server (`http-server`) to serve Exosphere 
3. Utility to set the local test hostname: A `supervisord` program that maps the `app.exosphere.localhost` domain name 
   to `127.0.0.1` using `/etc/hosts`

(See `integration-tests/docker/supervisor/exosphere.conf` for 2 and 3 above.) 

We use [Kaniko](https://github.com/GoogleContainerTools/kaniko) to build this container as part of the GitLab CI 
pipeline. See <https://docs.gitlab.com/ee/ci/docker/using_kaniko.html>, including why GitLab recommends it.

If a browser test fails then a screenshot is created and saved as a job artifact. This helps the developer debug the 
problem. (We can extend this solution to record video of the browser sessions, using for example: 
<https://github.com/saily/vnc-recorder>)

## Consequences

These automated tests reduce the need for manual testing of the Exosphere app in a browser.

It will also make it possible to run compatibility tests against multiple versions of OpenStack.

This does introduce a couple of hurdles for new contributors: they need cloud credentials (currently TACC credentials), 
and they need to add these credentials as environment variables in the GitLab CI settings of their Exosphere fork.
