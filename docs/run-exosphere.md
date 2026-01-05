# Running Exosphere

There are several ways to run Exosphere. This document covers them, from simple to complex. If you just want to demo Exosphere with an OpenStack cloud whose APIs are accessible over the public internet, you can use one of our hosted sites linked in the [readme](../README.md#try-exosphere).

## Quickest Local Option

Use this option when you don't need to modify any configuration, and you are OK using the Exosphere project's hosted proxy. _(See [solving-cors-problem.md](solving-cors-problem.md) for background information about the Cloud CORS Proxy (CCP).)_


```bash
docker run --rm --publish 127.0.0.1:8000:8000 registry.gitlab.com/exosphere/exosphere
```

Then, open in your browser: <http://127.0.0.1:8000>

## To Modify Configuration or Use Your Own Proxy

```bash
git clone https://gitlab.com/exosphere/exosphere.git
cd exosphere
# At this point, make any desired changes to `config.js`, `cloud_configs.js`, etc.
docker build -t exosphere -f ./docker/standalone.Dockerfile .
docker run --rm --publish 127.0.0.1:8000:8000 exosphere
```

Then, open in your browser: <http://127.0.0.1:8000>

The `standalone.Dockerfile` is a more production-ready option that serves the app with Nginx. If you want to allow connections from other computers (like a reverse proxy server that terminates TLS for your users), remove the `127.0.0.1:` from the `--publish` option in `docker run`.

To apply further configuration changes after you start the container: stop it (`Ctrl-C` or `⌘-C`), then re-run the `docker build` and `docker run` steps.

## For Development Work

### Repository

First, clone the repository. If you plan to make a contribution, fork the project on GitLab, and clone your fork instead of the upstream repo.

```bash
git clone https://gitlab.com/exosphere/exosphere.git
cd exosphere
```

### Node.js Development Option

This option is flexible but requires a [Node.js](https://nodejs.org) environment on your computer. If you would like a more isolated development environment, consider the [Docker Development Option](#docker-development-option).

Local development using Node.js relies on an external [proxy server](solving-cors-problem.md) for connectivity to OpenStack clouds. It defaults to a proxy hosted by the Exosphere project unless you specify your own in `config.js`.

First, [install Node.js](https://nodejs.org/en/download).

- If you use Ubuntu or Debian, you may also need to `apt-get install nodejs-legacy`.
- If you are using macOS, you may need to add `127.0.0.1 app.exosphere.localhost` to `/etc/hosts`.

Install the project's dependencies (including Elm) using `npm`. From the root of the exosphere repo run:

```bash
npm install
```

To compile the app and serve it using a local development server run this command:

```bash
npm start
```

Then, open in your browser: <http://app.exosphere.localhost:8000>

When you save a file in your editor, [`elm-watch`](https://lydell.github.io/elm-watch/) will detect and recompile your changes, then hot-reload the app in your browser. (Be aware that hot-reloading can occasionally fail; if it does, you can refresh your web browser to see your changes.)

### Docker Development Option

From the root of the exosphere repo, build the container and run it. There are bind mounts for `src/` and all other files needed by the application.

```bash
docker build -t exosphere .
./docker/start-docker-dev.sh exosphere
```

You should see [`elm-watch`](https://lydell.github.io/elm-watch/) starting in the `docker run` output:

```
✅ Exosphere
✅ DesignSystem

📊 server: http://localhost:8000, network: http://172.17.0.2:8000
📊 web socket connections: 1, elm-watch-node workers: 1
```

Then, open in your browser: <http://app.exosphere.localhost:8000>

(If you are using macOS, you may need to add `127.0.0.1 app.exosphere.localhost` to `/etc/hosts`.)

While the container is running, you can edit the Elm source code in the `src/` directory on your computer. When you save a file in your editor, `elm-watch` will detect and recompile your changes, then hot-reload the app in your browser. You'll see any compiler errors as more output from the `docker run` command. (Be aware that hot-reloading can occasionally fail; if it does, you can refresh your web browser to see your changes.)

If you need to change any files or configuration not in your bind mounts, you need to stop (`Ctrl-C` or `⌘-C`) the `docker run` command, then re-run the `docker build` and `docker run` commands above.

If you want to copy the compiled app (`elm-web.js`, or any other file) from the container, you can run this in another terminal window:

```bash
docker cp exosphere:/usr/src/app/elm-web.js my-elm.js
```

When you're done running the container, stop it by pressing `Ctrl-C` or `⌘-C` in the `docker run` window (the one running `elm-watch`).

## Running on a Production Server

- Once it's compiled (to `elm-web.js`), the Exosphere application can be served as static content from any web server.
- Exosphere's two supporting proxy servers ([Cloud CORS Proxy](solving-cors-problem.md) and [User Application Proxy](user-app-proxy.md)) require [Nginx](https://nginx.org) configured with browser-accepted TLS (e.g. via [Let's Encrypt](https://letsencrypt.org)). The User Application Proxy requires a wildcard TLS certificate; Let's Encrypt issues these free of charge.