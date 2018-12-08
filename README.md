# Exosphere

User-friendly, extensible client for cloud computing. Currently targeting OpenStack.

![development stage: alpha](https://img.shields.io/badge/stage-alpha-orange.svg)
[![pipeline status](https://gitlab.com/exosphere/exosphere/badges/master/pipeline.svg)](https://gitlab.com/exosphere/exosphere/commits/master)
![hi mom!](https://img.shields.io/badge/hi-mom!-blue.svg)

- **Do you have access to an OpenStack cloud?** Want a really pleasant way to use it?
- **Are you a cloud operator?** Want an easy way to offer same to your users?

...then Exosphere may be for you!

## Features and Goals

**Right now:**
- The most user-friendly way to manage cloud computers on OpenStack
- Works great for:
  - Compute-intensive workloads ("I need a really big computer")
    - Including GPU instances
  - Persistent servers ("I need this one to stick around for years")
  - Disposable experiments ("I need a place to try this thing")
- Delivers on each instance:
  - One-click terminal, no knowledge of SSH required
  - One-click [graphical dashboard](https://cockpit-project.org/)
- Use with any OpenStack cloud
- Completely standalone app, no custom backend/server required
- App is engineered for ease of adoption, troubleshooting, and development
  - [No runtime exceptions!](https://elm-lang.org/)
  - Open source and [open](https://gitlab.com/exosphere/exosphere/issues) [development](https://gitlab.com/exosphere/exosphere/merge_requests?scope=all&utf8=%E2%9C%93&state=merged) [process](https://gitlab.com/exosphere/exosphere/wikis/user-testing/Person-L-('L'-as-in-Andrew-Lenards-%F0%9F%99%87)). Come hack with us!

**Soon:**
- Support for the following as first-class resources:
  - Docker and Singularity containers
  - Jupyter Notebooks
- Compute cluster orchestration: head and worker nodes
- One-click remote graphical session to your cloud instances (with support for 3D GPU acceleration). No knowledge of VNC/SSH/etc. required!
- Community-supported deployment automations for custom services and scientific workflows

**Later:**
- Multi-cloud support (providers other than OpenStack)
- Automated deployment of data processing clusters (Hadoop, Spark, etc.)

## Try Exosphere

Right now we recommend trying Exosphere as an [Electron](https://electronjs.org/) app, rather than in a web browser.

### Build and Run Exosphere as Electron App

First [install node.js + npm](https://www.npmjs.com/get-npm). (If you use Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

Then install the project's dependencies (including Elm & Electron). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
```

git Sync the Git submodules:

```bash
git submodule sync --recursive
git submodule update --init --recursive
```

To compile and run the app:

```bash
npm run electron-build
npm run electron-start-dev
```

To watch for changes to `*.elm` files, auto-compile when they change, and hot-reloading of the app:

```bash
npm run electron-watch-dev
```

Based on the instructions found here:

<https://medium.com/@ezekeal/building-an-electron-app-with-elm-part-1-boilerplate-3416a730731f>

### Try Exosphere in a browser (not currently recommended)

[Try Exosphere on GitLab Pages](https://exosphere.gitlab.io/exosphere/index.html)

#### Why is a Browser Not Recommended?

Connecting to cloud providers from Exosphere running in a browser is currently problematic because of the same-origin policy (making cross-origin credentialed requests and viewing headers of the responses). A way around this is to 1. install a browser extension like [CORS Everywhere](https://addons.mozilla.org/en-US/firefox/addon/cors-everywhere/), and/or(?) 2. Connect to an OpenStack cloud whose Keystone is configured to allow cross-origin requests. This works for testing/evaluation purposes but is not recommended for security reasons.

#### Build and Run Exosphere (in a browser)

First [install node.js + npm](https://www.npmjs.com/get-npm). (If you use Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

Then install the project's dependencies (including Elm). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
```

Sync the Git submodules:

```bash
git submodule sync --recursive
git submodule update --init --recursive
```

To compile the app:
```
elm make src/Exosphere.elm
```

Then browse to the compiled index.html.


### Note about self-signed certificates for terminal and server dashboard

Currently the Cockpit dashboard and terminal for a provisioned server is served using a self-signed TLS certificate.
While we work on a permanent solution which does not require trusting self-signed certificates we have to enable the
`ignore-certificate-errors` switch for Electron.   

```javascript
// Uncomment this for testing with self-signed certificates
app.commandLine.appendSwitch('ignore-certificate-errors', 'true');
```

Do not enable this by default.

Until the permanent solution has been implemented, please do not use the terminal or server dashboard functionality over untrusted networks, and do not type or transfer any sensitive information into a server via a terminal window or dashboard view.

## Collaborate

[Real-time chat](https://c-mart.sandcats.io/shared/ak1ymBWynN1MZe0ot1yEBOh6RF6fZ9G2ZOo2xhnmVC5) (sign in with email or GitHub)

## Package Exosphere as a distributable Electron app

This uses [electron-builder](https://www.electron.build/). See the link for more information.

### On/For Mac OS X

```bash
git submodule sync --recursive
git submodule update --init --recursive
npm install
npm run electron-build
npm run dist
```

### On/For Linux

(Tested with Ubuntu 16.04)

```bash
git submodule sync --recursive
git submodule update --init --recursive
npm install
npm run electron-build
npx electron-builder --linux deb tar.xz
```

Note:

- Currently only tested with MacOS and Linux (Ubuntu 16.04) - need testing and instructions for Windows.
- Add instructions for [code signing](https://www.electron.build/code-signing)  

## UI, Layout, and Style

### Basics

- Exosphere uses [elm-ui](https://github.com/mdgriffith/elm-ui) for UI layout and styling. Where we can, we avoid defining HTML and CSS manually.
- Exosphere also uses parts of the experimental [elm-style-framework](https://github.com/lucamug/elm-style-framework), which is consumed as a git submodule rather than an Elm package (because of <https://github.com/lucamug/elm-style-framework/issues/7>).
- Exosphere also uses app-specific elm-ui "widgets", see `src/Widgets`. Some of these are extended/modified elm-style-framework widgets, and some are unique to Exosphere. We are moving toward using these re-usable widgets as the basis of our UI.  

### Style Guide

- You can view a rendering of all the widgets included in elm-style-framework here: <http://guupa.com/elm-style-framework/framework.html>
  - Note that Exosphere overrides default colors in elm-style-framework, so the colors in this demo will not match up exactly what you will see in Exosphere or its style guide.
- There is also an Exosphere "style guide" demonstrating use of Exosphere's custom widgets, some of which are modified widgets from elm-style-framework.

You can launch a live-updating Exosphere style guide by doing the following:
- Run `npm run live-style-guide`
- Browse to <http://127.0.0.1:8000>

This guide will automatically refresh whenever you save changes to code in `src/Style`!

You can also build a "static" style guide by running `npm run build-style-guide`. This will output styleguide.html.

### How to Add New Widgets

- Create a module for your widget (or update an existing module) in `src/Style/Widgets`
- Add example usages of your widget in `src/Style/StyleGuide.elm`
- Preview your widget examples in the style guide (see above) to ensure they look as intended

### Imports

In the spirit of [PEP 8](https://www.python.org/dev/peps/pep-0008/), each file should import modules grouped into sections as follows:

1. Elm Standard libraries
2. Community libraries
3. Local app-specific libraries/imports

Unlike Python/PEP8 you will not be able to separate each section with a space because elm-format will remove the spaces. The spaces are not an Elm convention.

The imports in each section should be in alphabetical order.

## OpenStack and CORS

In order to use Exosphere in a browser as opposed to Electron (again, this is still not recommended), OpenStack services must be configured to allow cross-origin requests. This is because Exosphere is served from a different domain than the OpenStack APIs.

(todo describe security implications)

The OpenStack admin guide has a great page on how to enable CORS across OpenStack services. This guide was removed but is fortunately [still accessible via Wayback Machine](https://web.archive.org/web/20160305193201/http://docs.openstack.org/admin-guide-cloud/cross_project_cors.html).

At minimum, need the following in glance.conf, keystone.conf, and neutron.conf:

```
[cors]
allowed_origin: *
```

The following in nova.conf:

```
[cors]
allowed_origin = *
allow_headers = Content-Type,Cache-Control,Content-Language,Expires,Last-Modified,Pragma,X-Custom-Header,OpenStack-API-Version,X-Auth-Token
```
