# Exosphere: the User-Friendliest Interface for Non-proprietary Cloud Infrastructure

- Empowers researchers and other non-IT professionals to deploy their code and run services on [OpenStack](https://www.openstack.org)-based cloud infrastructure without advanced knowledge of virtualization or networking concepts
- Fills the gap between interfaces built for system administrators like OpenStack Horizon, and intuitive-but-proprietary services like DigitalOcean
- Enables cloud operators to deliver a user-friendly, powerful interface to their community with customized branding, nomenclature, and single sign-on integration

[![screenshot of Exosphere](docs/screenshot-for-readme.png)](docs/screenshot-for-readme.png)


## Quick Start

- **[try.exosphere.app](https://try.exosphere.app)** in your browser, if you have access to an existing OpenStack cloud with internet-facing APIs
- Use **[exosphere.jetstream-cloud.org](https://exosphere.jetstream-cloud.org)** if you have an allocation on [Jetstream Cloud](https://jetstream-cloud.org/)

## Overview and Features

_Wait, what is OpenStack?_ OpenStack is the operating system and APIs that power public research clouds at [Jetstream](https://jetstream-cloud.org) and [CyVerse](https://cyverse.org),  private clouds at organizations like [Wikimedia](https://www.mediawiki.org/wiki/Wikimedia_Cloud_Services_team) and [CERN](https://clouddocs.web.cern.ch/), and public commercial clouds like [OVH](https://us.ovhcloud.com/public-cloud/), [Fuga](https://fuga.cloud/), and [Vexxhost](https://vexxhost.com/). You can also run OpenStack on your own hardware to provide cloud infrastructure-as-a-service for your organization!

_OK, what can I do with Exosphere?_

- Easily create instances to run your code, and volumes to manage your data
  - Works great for containers, intensive compute jobs, disposable experiments, and persistent web services
- Get **one-click, browser-based shell** access to cloud resources with Exosphere's [Apache Guacamole](http://guacamole.apache.org) integration
  - One-click **graphical desktop with GPU support** shipping by August 2021
- **Pretty graphs** show resource utilization of each instance at a glance
- If you're a cloud operator, deliver a customized interface with white-labeling, localized nomenclature, and single sign-on
- 100% self-hostable, 99% standalone client application
  - Two small proxy servers facilitate secure web browser connections to OpenStack APIs and services running on user-launched cloud instances
- On the roadmap:
  - First-class support for containers and data science workbenches
  - Cluster orchestration
  - Community-curated deployment automations for scientific workflows and custom services
- Fully open source and open development process -- come hack with us!
  - See Exosphere's [values and goals](docs/values-goals.md)
  
Exosphere will be a primary user interface for [Jetstream 2](https://itnews.iu.edu/articles/2020/NSF-awards-IU-10M-to-build-Jetstream-2-cloud-computing-system-.php), an [NSF](https://www.nsf.gov)-funded science and engineering cloud. Jetstream 2 will be available to any US-based researcher starting late 2021.

## Collaborate With Us

To start a conversation or ask for help, talk to us in real-time on [Matrix / Element](https://riot.im/app/#/room/#exosphere:matrix.org). You can also [browse an archive](https://view.matrix.org/room/!qALrQaRCgWgkQcBoKG:matrix.org/) of the chat history.

We use GitLab to track issues and contributions. To request a new feature or report a bug, [create a new issue](https://gitlab.com/exosphere/exosphere/-/issues/new) on our GitLab project.

See [contributing.md](contributing.md) for contributor guidelines.

Architecture decisions are documented in [docs/adr/README.md](docs/adr/README.md).

If you want to work on the application UI and styling, see [style.md](docs/style.md) for an orientation.

## Advanced Topics

The following techniques are intended for cloud operators, advanced users, and for development purposes. We suggest that new users start with one of the hosted applications linked above.

### Build and Run Exosphere Locally

If you are building Exosphere for consumption in a web browser, please also see [cloud-cors-proxy.md](docs/cloud-cors-proxy.md).

First [install node.js + npm](https://www.npmjs.com/get-npm). (If you use Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

Then install the project's dependencies (including Elm). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
```

To compile the app and serve it using a local development server run this command:

```
npm run live
```

Then browse to <http://app.exosphere.localhost:8000/>

To enable the Elm Debugger in the local development server run the following command instead:

```
npm run live-debug
```

Note: The local development server uses elm-live. It detects changes to the Exosphere source code, recompiles it, and
refreshes the browser with the latest version of the app. See [elm-live.com](https://www.elm-live.com/) for more
information.

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

or in another window execute a command to the container to rebuild the elm-web.js file:

```bash
$ docker exec exosphere elm make src/Exosphere.elm --output elm-web.js
Success! Compiled 47 modules.

    Exosphere ───> elm-web.js
```

If you need changes done to other files in the root, you can either bind them
or make changes and rebuild the base. You generally shouldn't make changes to files
from inside the container that are bound to the host, as the permissions will be
modified.

If you want to copy the elm-web.js from inside the container (or any other file) you can do:

```bash
docker cp exosphere:/usr/src/app/elm-web.js my-elm.js
```

When it's time to cleanup, you can do `docker stop exosphere` and `docker rm exosphere`.

### Exosphere Compatibility

#### To use with an OpenStack Cloud

- Exosphere works with OpenStack Queens version (released February 2018) or later.
- Exosphere works best with clouds that have [automatic allocation of network topology](https://docs.openstack.org/neutron/latest/admin/config-auto-allocation.html) enabled.

#### To host the Exosphere Web Application  

- The Exosphere client-side application can be served as static content from any web server.
- Exosphere's two supporting proxy servers ([Cloud CORS Proxy](docs/cloud-cors-proxy.md) and [User Application Proxy](docs/user-app-proxy.md)) require [Nginx](https://nginx.org) configured with browser-accepted TLS (e.g. via [Let's Encrypt](https://letsencrypt.org)). The User Application Proxy requires a wildcard TLS certificate; Let's Encrypt issues these free of charge.

### Runtime configuration options

These options are primarily intended for cloud operators who wish to offer a customized deployment of Exosphere to their user community. Set these in `config.js`.

| *Option*                  | *Possible Values*          | *Description*                                                       |
|---------------------------|----------------------------|---------------------------------------------------------------------|
| showDebugMsgs             | false, true                |                                                                     |
| cloudCorsProxyUrl         | null, string               | See `docs/cloud-cors-proxy.md`; required to use app in web browser  |
| cloudsWithUserAppProxy    | (see docs)                 | See `docs/user-app-proxy.md`; required for Guacamole support        |
| palette                   | null, JSON object          | Pass custom colors to style Exosphere, see example below            |
| logo                      | null, string               | Path to custom logo to show in top-left corner of app               |
| favicon                   | null, string               | Path to custom favicon                                              |
| appTitle                  | null, string               | Title to show in top-left corner of app                             |
| defaultLoginView          | null, openstack, jetstream | Which login view to display by default                              |
| aboutAppMarkdown          | null, string (markdown)    | What to show in the "About the app" section of Help/About view      |
| supportInfoMarkdown       | null, string (markdown)    | What to show when user clicks "Get support" button                  |
| userSupportEmail          | null, string (markdown)    | Email address to ask users to send problem report                   |
| openIdConnectLoginConfig  | null, JSON object          | See `docs/federated-login.md` for more info and example JSON        |
| featuredImageNamePrefix   | null, string               | A (public) image is 'featured' if the name starts with this string  |
| defaultImageExcludeFilter | null, JSON object          | A key:value property to exclude images from UI, see example below   |
| localization              | null, JSON object          | Pass custom localization strings for the UI, see example below      |

#### Example Custom Palette

This declares a primary and secondary color in the app.

```
palette: { primary: {r: 0, g: 108, b: 163 }, secondary : {r: 96, g: 239, b: 255 } } 
```

#### Example Image Exclude Filter

This excludes images built by, and intended for the Atmosphere platform.

```
defaultImageExcludeFilter: { filterKey : "atmo_image_include", filterValue : "true" } 
```

#### Example Localization JSON object

This allows a deployer to customize terms used by Exosphere for their organization or community.

```
localization: {
    openstackWithOwnKeystone: "cloud",
    openstackSharingKeystoneWithAnother: "region",
    unitOfTenancy: "project",
    maxResourcesPerProject: "resource limits",
    pkiPublicKeyForSsh: "SSH public key",
    virtualComputer: "instance",
    virtualComputerHardwareConfig: "size",
    cloudInitData: "boot script",
    commandDrivenTextInterface: "terminal",
    staticRepresentationOfBlockDeviceContents: "image",
    blockDevice: "volume",
    nonFloatingIpAddress: "internal IP address",
    floatingIpAddress: "public IP address",
    graphicalDesktopEnvironment: "graphical desktop environment"
    }
```