# Exosphere

User-friendly, extensible client for cloud computing. Currently targeting OpenStack.

![development stage: alpha](https://img.shields.io/badge/stage-alpha-orange.svg)
[![pipeline status](https://gitlab.com/exosphere/exosphere/badges/master/pipeline.svg)](https://gitlab.com/exosphere/exosphere/commits/master)
[![chat on gitter](https://img.shields.io/badge/chat_on-gitter-teal.svg)](https://gitter.im/exosphere-app/community)
[![chat on matrix](https://img.shields.io/badge/chat_on-matrix-blue.svg)](https://riot.im/app/#/room/#exosphere:matrix.org)

**New: [Try Exosphere in your web browser](https://try.exosphere.app/exosphere/)**

## Features and Goals

- **Do you have access to an OpenStack cloud?** Want a really pleasant way to use it?
- **Are you a cloud operator?** Want an easy way to offer same to your users?

...then Exosphere may be for you!

(See also values-goals.md for what drives the Exosphere project.) 


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

## Compatibility

Exosphere requires Queens release of OpenStack (released February 2018).

## Try Exosphere

There are three ways to get started using Exosphere.

1. Very Quick Start: The easiest way is at <https://try.exosphere.app/exosphere/>. This uses a proxy for OpenStack API requests. It is very easy to try but doesn't yet support all features (e.g. one-click shell and instance dashboard).
2. Install on your computer: The next easiest way is to download a pre-compiled installer for your operating system. You can find the latest builds for OSX, Linux, and Windows here: https://try.exosphere.app/packages/
3. Build yourself: If you want to try all app features, you can run Exosphere locally using [Electron](https://electronjs.org/). This is described below.

### Build and Run Exosphere as Electron App

> "It was very easy to build"
> - Connor Osborn

First [install node.js + npm](https://www.npmjs.com/get-npm). (If you use Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

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

> "This watch mode is great"
> - Connor Osborn

Based on the instructions found here:

<https://medium.com/@ezekeal/building-an-electron-app-with-elm-part-1-boilerplate-3416a730731f>


### Build Exosphere and Run in a Browser

If you are building Exosphere for consumption in a web browser, please also see docs/cors-proxy.md.

First [install node.js + npm](https://www.npmjs.com/get-npm). (If you use Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

Then install the project's dependencies (including Elm). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
```


To compile the app:
```
elm make src/Exosphere.elm --output elm.js
```

Then browse to index.html.

### Build and Run Exosphere with Docker

If you want to build exosphere (as shown above) for a browser but do not want 
to install node on your system, you can use the [Dockerfile](Dockerfile)
to build a container instead. First, build the container:

```bash
docker build -t exosphere .
```

And then run, binding port 80 to 8080 in the container:

```bash
$ docker run -it -p 80:8080 exosphere
Starting up http-server, serving ./
Available on:
  http://127.0.0.1:8080
  http://172.17.0.3:8080
Hit CTRL-C to stop the server
```

You can open your browser to [http://127.0.0.1](http://127.0.0.1) to see the interface.
If you want a development environment to make changes to files, you can run
the container and bind the src directory:

```bash
$ docker run --rm -v $PWD/src:/usr/src/app/src -it --name exosphere -p 80:8080 exosphere
```

And then either run the above command with `-d` (for detached)

```bash
$ docker run -d --rm -v $PWD/src:/usr/src/app/src -it --name exosphere -p 80:8080 exosphere
```

or in another window execute a command to the container to rebuild the elm.js file:

```bash
$ docker exec exosphere elm make src/Exosphere.elm --output elm.js
Success! Compiled 47 modules.

    Exosphere ───> elm.js
```

If you need changes done to other files in the root, you can either bind them
or make changes and rebuild the base. You generally shouldn't make changes to files
from inside the container that are bound to the host, as the permissions will be
modified.

If you want to copy the elm.js from inside the container (or any other file) you can do:

```bash
docker cp exosphere:/usr/src/app/elm.js my-elm.js
```

When it's time to cleanup, you can do `docker stop exosphere` and `docker rm exosphere`.

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

## Runtime configuration options

Most users should not need to change these; they are primarily intended for cloud operators and others who wish to offer a customized build of Exosphere to a specific group of users. Set these in the `flags` JSON object in `port.js`.

| *Option*                  | *Possible Values* | *Description*                                                       |
|---------------------------|-------------------|---------------------------------------------------------------------|
| showDebugMsgs             | false, true       |                                                                     |
| openstackApiProxyUrl      | null, string      | See `docs/cors-proxy.md`; required for using Exosphere in a browser |
| cloudsWithTlsReverseProxy | (see docs)        | See `docs/tls-terminating-proxy.md`; required for Guacamole support |

## Collaborate

Talk to Exosphere developers and other users in real-time via [gitter](https://gitter.im/exosphere-app/community), or `#exosphere:matrix.org` on [Matrix / Riot](https://riot.im/app/#/room/#exosphere:matrix.org). The chat is bridged across both platforms, so join whichever you prefer. You can also [browse an archive](https://view.matrix.org/room/!qALrQaRCgWgkQcBoKG:matrix.org/) of the chat history.

See also contributing.md for contributor guidelines.

## Package Exosphere as a distributable Electron app

This uses [electron-builder](https://www.electron.build/). See the link for more information.

### On/For Mac OS X

```bash
npm install
npm run electron-build
npm run dist
```

To launch Exosphere from the OS X package, [you may need to right-click and then "Open"](https://www.iclarified.com/28180/how-to-open-applications-from-unidentified-developers-in-mac-os-x-mountain-lion), because it is an "un-signed" application.

### On/For Linux

(Tested with Ubuntu 16.04)

```bash
npm install
npm run electron-build
npx electron-builder --linux deb tar.xz
```

### On/For Windows

If you need access to Windows, get a virtual machine [here](https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/).

- Install git (and git bash, etc) if you don't have already
- Clone the repo
- [Download](nodejs.org/en/download) and install node + NPM if you don't have already
  - You may not also need the Chocolatey C++/Python stuff?
- `npm install`
- `npm run electron-build`
- `npx electron-builder --windows`
- look for the `.exe` in the `dist/` subfolder of exosphere

When running downloaded package, you'll see a scary message from Windows, click "more info" and then "run anyway".

Note:

- Add instructions for [code signing](https://www.electron.build/code-signing)  

## UI, Layout, and Style

### Basics

- Exosphere uses [elm-ui](https://github.com/mdgriffith/elm-ui) for UI layout and styling. Where we can, we avoid defining HTML and CSS manually.
- Exosphere also consumes some [elm-ui-widgets](https://package.elm-lang.org/packages/Orasund/elm-ui-widgets/latest/)
- Exosphere also uses app-specific elm-ui "widgets", see `src/Widgets`.

### Style Guide

- There is an Exosphere "style guide" demonstrating use of Exosphere's custom widgets.

You can launch a live-updating Exosphere style guide by doing the following:
- Run `npm run live-style-guide`
- Browse to <http://127.0.0.1:8000>

This guide will automatically refresh whenever you save changes to code in `src/Style`!

You can also build a "static" style guide by running `npm run build-style-guide`. This will output styleguide.html.

### How to Add New Widgets

- Create a module for your widget (or update an existing module) in `src/Style/Widgets`
- Add example usages of your widget in `src/Style/StyleGuide.elm`
- Preview your widget examples in the style guide (see above) to ensure they look as intended
