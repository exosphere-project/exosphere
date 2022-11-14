# Running Exosphere

There are several ways to run Exosphere: locally on your computer (for use or development purposes), or on a server (to offer a production deployment to others). This document covers most of them.

## Locally, Using Docker

You can run Exosphere using a Docker container. The Docker container includes a Cloud CORS Proxy (CCP) through which Exosphere communicates with OpenStack API endpoints. This has the following benefits:

1. You can use Exosphere to access OpenStack APIs which are not accessible from outside an organization's secure network (as long as the computer running the container can access the OpenStack APIs)
2. Your cloud credentials will never pass through the proxy servers managed by the Exosphere project for the convenience of most users

Note: See [solving-cors-problem.md](docs/solving-cors-problem.md) for background information about the Cloud CORS Proxy (CCP).

### Use an official Exosphere container image

This is the simplest option for using Exosphere, but it is not recommended for development work.

```bash
docker run --publish 127.0.0.1:8000:8000 registry.gitlab.com/exosphere/exosphere
```

Open URL in a browser: <http://127.0.0.1:8000/>

### Build a container image from the source code

This is recommended for development work, and also for general use when you want to modify Exosphere's configuration.

```bash
git clone https://gitlab.com/exosphere/exosphere.git
cd exosphere
docker build -t exosphere -f ./docker/standalone.Dockerfile .
docker run --publish 127.0.0.1:8000:8000 exosphere
```

Open URL in a browser: <http://127.0.0.1:8000/>

### Use Docker when developing Exosphere

If you want to work on the Exosphere code but do not want to install `node` on your system, then you can use the [Dockerfile](Dockerfile) in the root directory of the repository to build a development container instead.

First, build the container:

```bash
docker build -t exosphere .
```

And then run, binding port 8000 to 8000 in the container:

```bash
docker run --rm -it --name exosphere --publish 127.0.0.1:8000:8000 exosphere
```

You should see `elm-live` starting:

```
elm-live:
  Hot Reloading is ON

  Warning: Hot Reloading does not replay the messages of the app.
  It just restores the previous state and can lead to bad state.
  If this happen, reload the app in the browser manually to fix it.


elm-live:
  Server has been started! Server details below:
    - Website URL: http://0.0.0.0:8000
    - Serving files from: /usr/src/app
    - Proxying requests starting with /proxy to https://try-dev.exosphere.app/proxy

elm-live:
  The build has succeeded. 

elm-live:
  Watching the following files:
    - src/**/*.elm
```

You can open your browser to [http://app.exosphere.localhost:8000/](http://app.exosphere.localhost:8000/) to see the interface.

If you want a development environment to make changes to files, you can run
the container and bind the src directory:

```bash
$ docker run --rm -v $PWD/src:/usr/src/app/src -it --name exosphere -p 8000:8000 exosphere
```

You can then edit the Elm source code on your host using your favorite editor and `elm-live` inside the container will
detect the changes, automatically recompile the source code, and then reload the app in your browser.

If you need changes done to other files in the root, you can either bind them
or make changes and rebuild the base. You generally shouldn't make changes to files
from inside the container that are bound to the host, as the permissions will be
modified.

If you want to copy the elm-web.js from inside the container (or any other file) you can do the following in another
terminal window:

```bash
docker cp exosphere:/usr/src/app/elm-web.js my-elm.js
```

When it's time to cleanup, press Ctrl-C in the terminal window running `elm-live`.

## Locally, Not Using Docker

First [install node.js + npm](https://www.npmjs.com/get-npm).

- If you use Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.
- If you are using Mac OS X, you may need to add `127.0.0.1       app.exosphere.localhost` to `/etc/hosts`

Then install the project's dependencies (including Elm). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
```

To compile the app and serve it using a local development server run this command:

```
npm start
```

Then browse to <http://app.exosphere.localhost:8000/>

To enable the Elm Debugger in the local development server run the following command instead:

```
npm run live-debug
```

Note: The local development server uses elm-live. It detects changes to the Exosphere source code, recompiles it, and
refreshes the browser with the latest version of the app. See [elm-live.com](https://www.elm-live.com/) for more
information.

## Hosting Exosphere on a Server

- The Exosphere client-side application can be served as static content from any web server.
- Exosphere's two supporting proxy servers ([Cloud CORS Proxy](docs/solving-cors-problem.md) and [User Application Proxy](docs/user-app-proxy.md)) require [Nginx](https://nginx.org) configured with browser-accepted TLS (e.g. via [Let's Encrypt](https://letsencrypt.org)). The User Application Proxy requires a wildcard TLS certificate; Let's Encrypt issues these free of charge.

