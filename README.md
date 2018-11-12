# Exosphere

![development stage: alpha](https://img.shields.io/badge/stage-alpha-orange.svg)
[![pipeline status](https://gitlab.com/exosphere/exosphere/badges/master/pipeline.svg)](https://gitlab.com/exosphere/exosphere/commits/master)
![hi mom!](https://img.shields.io/badge/hi-mom!-blue.svg)

User-friendly, extensible client for cloud computing. Currently targeting OpenStack.

- **Do you have access to an OpenStack cloud?** Want a really nice way to use it?
- **Are you a cloud operator?** Want to offer same to your users?

Then read on!

## Features and Goals

**Right now:**
- The most user-friendly way for scientists and developers to manage cloud computers
- Works great for:
  - Compute-intensive workloads ("I need a really big computer")
    - GPU instances
  - Persistent servers ("I need this one to stick around for years")
  - Disposable experiments ("I need a place to try this thing")
- Delivers on each instance:
  - One-click terminal, no knowledge of SSH required
  - One-click [graphical dashboard](https://cockpit-project.org/)
- Completely standalone app, no custom backend/server required
- App is engineered for ease of deployment, troubleshooting, and development
- Open source and [open](https://gitlab.com/exosphere/exosphere/issues) [development](https://gitlab.com/exosphere/exosphere/merge_requests?scope=all&utf8=%E2%9C%93&state=merged) [process](https://gitlab.com/exosphere/exosphere/wikis/user-testing/Person-L-('L'-as-in-Andrew-Lenards-%F0%9F%99%87). Come hack with us!

**Soon:**
- Support for the following as first-class resources:
  - Docker and Singularity containers
  - Jupyter Notebooks
- Compute cluster orchestration: head and worker nodes
- One-click remote graphical session to your cloud instances (with support for 3D GPU acceleration), no knowledge of VNC/SSH/etc. required!
- Community-supported deployment automations for custom services and scientific workflows

**Later:**
- Multi-cloud support (providers other than OpenStack)
- Automated deployment of data processing clusters (Hadoop, Spark, etc.)

## Collaborate

[Real-time chat](https://c-mart.sandcats.io/shared/ak1ymBWynN1MZe0ot1yEBOh6RF6fZ9G2ZOo2xhnmVC5) (sign in with email or GitHub)

## Try Exosphere

Right now we recommend

[Try Exosphere on GitLab Pages](https://exosphere.gitlab.io/exosphere/index.html)

## Build and Run Exosphere

First [install node.js + npm](https://www.npmjs.com/get-npm). (On Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

Then install the project's dependencies (including Elm). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
```

To compile the app:
```
elm make src/Exosphere.elm
```

Then browse to the compiled index.html.

## Build and Run Exosphere as Electron App

First [install node.js + npm](https://www.npmjs.com/get-npm). (On Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

Then install the project's dependencies (including Elm & Electron). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
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


### Note about self-signed certificates for terminal and server dashboard

Currently the Cockpit dashboard and terminal for a provisioned server is served using a self-signed TLS certificate.
While we work on a permanent solution which does not require trusting self-signed certificates we have to enable the
`ignore-certificate-errors` switch for Electron.   

```javascript
// Uncomment this for testing with self-signed certificates
app.commandLine.appendSwitch('ignore-certificate-errors', 'true');
```

Do not enable this by default.

Until the permanent solution has been implemented, do not use the terminal or server dashboard functionality over
untrusted networks, and do not type or transfer any sensitive information into a server via a terminal window or
dashboard view.


## Package Exosphere as a distributable Electron app

This uses [electron-builder](https://www.electron.build/). See the link for more information.

### On/For Mac OS X

```bash
npm install
npm run electron-build
npm run dist
```

### On/For Linux

(Tested with Ubuntu 16.04)

```bash
npm install
npm run electron-build
npx electron-builder --linux deb tar.xz
```

Note:

- Currently only tested with MacOS and Linux (Ubuntu 16.04) - need testing and instructions for Windows.
- Add instructions for [code signing](https://www.electron.build/code-signing)  


## Try Exosphere in browser

Note: connecting to cloud providers from Exosphere running in a browser is currently problematic because of the same-origin policy (making cross-origin credentialed requests and viewing headers of the responses). A way around this is to 1. install a browser extension like [CORS Everywhere](https://addons.mozilla.org/en-US/firefox/addon/cors-everywhere/), and 2. Connect to an OpenStack cloud whose Keystone is configured to allow cross-origin requests. This works for testing/evaluation purposes but is not recommended for security reasons.

- First, [install Elm](https://guide.elm-lang.org/install.html)
- Run `elm-reactor` from the root of this repository
- Visit `http://localhost:8000/src/Exosphere.elm`

## Style Guide

### Imports

In the spirit of [PEP 8](https://www.python.org/dev/peps/pep-0008/), each file should import modules grouped into sections as follows:

1. Elm Standard libraries
2. Community libraries
3. Local app-specific libraries/imports

Unlike Python/PEP8 you will not be able to separate each section with a space because elm-format will remove the spaces. The spaces are not an Elm convention.

The imports in each section should be in alphabetical order.

## OpenStack and CORS

In order to use Exosphere in a browser (as opposed to Electron), OpenStack services must be configured to allow cross-origin requests. This is because Exosphere is served from a different domain than the OpenStack APIs.

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
