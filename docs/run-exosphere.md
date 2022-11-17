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

### Docker Development Option (New Contributors Start Here)

First, clone the repository. If you plan to make a contribution, fork the project on GitLab, and clone your fork instead of the upstream repo.

```bash
git clone https://gitlab.com/exosphere/exosphere.git
cd exosphere
```

Build the container and run it with a bind mount to `src/`:

```
docker build -t exosphere .
docker run --rm -v $PWD/src:/usr/src/app/src -it --name exosphere -p 127.0.0.1:8000:8000 exosphere
```

You should see `elm-live` starting in the `docker run` output:

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

Then, open in your browser: [http://app.exosphere.localhost:8000](http://app.exosphere.localhost:8000)

(If you are using Mac OS, you may need to add `127.0.0.1 app.exosphere.localhost` to `/etc/hosts`.)

While the container is running, you can edit the Elm source code in the `src/` directory on your computer. When you save a file in your editor, `elm-live` will detect and recompile your changes, then hot-reload the app in your browser. 😎 You'll see any compiler errors as more output from the `docker run` command. (Be aware that hot-reloading will occasionally fail, and you'll need to refresh your web browser to see your changes.)

If you need to change any files or configuration _outside the `src/` directory,_ you need to stop (`Ctrl-C` or `⌘-C`) the `docker run` command, then re-run the `docker build` and `docker run` commands above.

If you want to copy the compiled app (`elm-web.js`, or any other file) from the container, you can run this in another terminal window:

```bash
docker cp exosphere:/usr/src/app/elm-web.js my-elm.js
```

When you're done running the container, stop it by pressing `Ctrl-C` or `⌘-C` in the `docker run` window (the one running `elm-live`).

### Node.js and `npm` Development Option

The Node.js and `npm` option offers more customizability than the Docker option, and works for people who don't want to use Docker, but it requires a Node.js environment on your computer. It relies on an external [proxy server](solving-cors-problem.md) for connectivity to OpenStack clouds. It defaults to a proxy hosted by the Exosphere project unless you specify your own in `config.js`.

First, [install node.js + npm](https://www.npmjs.com/get-npm).

- If you use Ubuntu or Debian, you may also need to `apt-get install nodejs-legacy`.
- If you are using Mac OS, you may need to add `127.0.0.1 app.exosphere.localhost` to `/etc/hosts`.

Then install the project's dependencies (including Elm). Convenience command to do this (run from the root of the exosphere repo):

```bash
npm install
```

To compile the app and serve it using a local development server run this command:

```
npm start
```

Then, open in your browser: <http://app.exosphere.localhost:8000>

To enable the Elm Debugger in the local development server run the following command instead:

```
npm run live-debug
```

When you save a file in your editor, `elm-live` will detect and recompile your changes, then hot-reload the app in your browser. (Be aware that hot-reloading will occasionally fail, and you'll need to refresh your web browser to see your changes.)

## Running on a Production Server

- Once it's compiled (to `elm-web.js`), the Exosphere application can be served as static content from any web server.
- Exosphere's two supporting proxy servers ([Cloud CORS Proxy](solving-cors-problem.md) and [User Application Proxy](user-app-proxy.md)) require [Nginx](https://nginx.org) configured with browser-accepted TLS (e.g. via [Let's Encrypt](https://letsencrypt.org)). The User Application Proxy requires a wildcard TLS certificate; Let's Encrypt issues these free of charge.