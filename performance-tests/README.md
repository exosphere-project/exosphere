# Performance Tests

> :test_tube: This is currently **experimental**.

## Overview

These tests are implemented using:

- [Playwright](https://github.com/microsoft/playwright) for testing and automation.
- [Lighthouse](https://github.com/GoogleChrome/lighthouse) for auditing, metrics and best practice recommendations.

## Setup

**Tip:** To avoid browser & driver problems, use an OS with a graphical user interface (even if you intend to run the performance tests in headless mode).

### Install dependencies

The performance testing dependencies are larger than for the core project so they are included under optional dependencies.

```sh
npm install --include=optional
```

Install a browser for Playwright to use (& all its dependencies):

```sh
npx playwright install --with-deps
```

<details><summary>Dependency troubleshooting.</summary>

Playwright can also install its dependencies using:

```sh
npx playwright install-deps
```

Or, failing that, using `apt-get` if using Ubuntu:

```sh
sudo apt-get install libevent-2.1-7t64 libgstreamer-plugins-bad1.0-0 libflite1 libavif16 gstreamer1.0-libav
```

</details>


### Set environment variables

In the project root, copy & rename `./.env.example` to `.env` & fill it in with your OpenStack credentials.

```
OS_AUTH_URL=
OS_DOMAIN=
OS_USERNAME=
OS_PASSWORD=
OS_PROJECT=
OS_REGION=
```

### Authenticate

Run an automated authentication script to sign in.

If running from a terminal in a GUI environment:

```sh
npm run perf:auth -- --headed
```

This will save your logged-in user state to `./performance-tests/.auth`.

**Tip:** This runs in headed mode to troubleshoot browser or network permissions. Jumping straight to headless mode often leads to unexpected timeouts. Once headed mode auth has succeeded, you can typically switch to headless mode without any problems.

If running from terminal session over SSH:

```sh
xvfb-run --server-args="-screen 0 1024x768x24" npm run perf:auth
```

This runs in headless mode using _X Virtual Frame Buffer_ to create a virtual display. Subsequent calls needn't typically be wrapped by `xvfb-run`.

### Validate Sign-in State

To double check that your user state is valid & accessible to the test suite, you can run:

```sh
npm run perf:check
```

This will navigate to the home page & take a screenshot. You can review the screenshot in `./performance-tests/screenshots`.

If it didn't allow the automation to hydrate from a signed-in state, the test will fail with an error.

## Usage

To generate a Lighthouse performance audit, run:

```sh
npm run perf:test
```

You should see logs in the terminal from network requests, page loads, etc.

Wait until the test passes (~30s).

Performance reports will be available in html and csv format in `./performance-tests/reports`.
