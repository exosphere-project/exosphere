# Exosphere

User-friendly, extensible client for cloud computing. Currently targeting OpenStack.

## Collaborate

[Real-time  chat](https://c-mart.sandcats.io/shared/ak1ymBWynN1MZe0ot1yEBOh6RF6fZ9G2ZOo2xhnmVC5) (sign in with email or GitHub)

## Try Exosphere as Electron App

We plan to build OS-specific application packages for Exosphere using [electron-builder](https://www.electron.build/), but haven't gotten to it yet. Here's how to get Exosphere running with a few extra steps.

First, [install Elm](https://guide.elm-lang.org/install.html) and [install node.js + npm](https://www.npmjs.com/get-npm). (On Ubuntu/Debian you may also need to `apt-get install nodejs-legacy`.)

Then install Electron. Convenience commands to do this (run from the root of the exosphere repo):

```bash
npm install electron
npm install
```

To compile and run the app:

```bash
npm run elm
npm run start
```

To watch for changes to `*.elm` files, auto-compile when they change, and hot-reloading of the app:

```bash
npm run watch
```

Based on the instructions found here:

<https://medium.com/@ezekeal/building-an-electron-app-with-elm-part-1-boilerplate-3416a730731f>

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
